####################
## Trait definitions
####################
#
# i.e. implement the @traitdef macro
#
# It looks like
# @traitdef Cmp{X,Y} <: Eq{X,Y} begin
#     isless(x,y) -> Bool
#     @constraints begin
#         X==Y
#     end
# end

# 1) parse the header
###
function parsecurly(def::Expr )
    # parses :(Cmp{x,y})
    # into: :Cmp, [:x,:y], :(Cmp{x,y}), ()

    # parses :(Monad{X{Y}})
    # into: :Monad, [:(X{Y})], :(Monad{X}), ()

    # multiple parametric trait:
    # parses :(ComboTr{X{Y}, Z{T}})
    # into: :ComboTr, [:(X{Y}), :(Z{T}) ]], :(ComboTr{X,Z}), ()

    # Note that if we have
    #       :(ComboTr{X{Y}, Z{Y}}) # note the same Y
    # The trait constructor will have an assertion that the parameter
    # in X and Z must match
    name = def.args[1]
    paras = Any[]
    append!(paras,def.args[2:end] )
    trait = Expr( :curly, name,
        map( x->typeof(x)==Symbol ? x : Base.Meta.isexpr( x, :curly )? x.args[1] : error( "Traits: unknown " * string(x) ),
        def.args[2:end])... )
    return name, paras, trait, :(())
end
function parsecomp(def::Expr)
    # parses :(Cmp{x,y} <: Eq{x,y})
    # into:  :Cmp, [:x,:y], :(Cmp{x,y}), :((Eq{x,y},))

    # parses :(Tr2{X{Z1},Y{Z2}} <: Tr2base{X,Y})
    # into:  :Tr2, [:(X{Z1}),:(Y{Z2})], :(Tr2{X,Y}), :((Tr2base{X,Y},))
    # the supertraits' parameters are redundant and, if given, are stripped out. So the following
    # would produce the same output
    #        :(Tr2{X{Z1},Y{Z2}} <: Tr2base{X{Z1},Y{Z2}})
    if  def.args[2]!=:<:
        error("not a <:")
    end
    name, paras, trait = parsecurly(def.args[1])
    supertraits = :()

    if !Base.Meta.isexpr( def.args[3], :curly )
        error( "Traits: RHS of " * string( def ) * " must be a curly (Trait) expression" )
    end
    c = def.args[3]
    supertrait = Expr( :curly, c.args[1],
        map( x->typeof(x)==Symbol ? x : Base.Meta.isexpr( x, :curly )? x.args[1] : error( "Traits: unknown " * string(x) ),
        c.args[2:end] )... )

    push!(supertraits.args, supertrait )
    return name, paras, trait, supertraits
end

function parsetuple(def::Expr)
    # parses :(Cmp{x,y} <: Eq{x,y}, Sz{x}, Uz{y})
    # into   :Cmp, [:x,:y], :(Cmp{x,y}), :((Eq{x,y},Sz{x},Uz{y}))

    # parses :(Tr2{X{Z1},Y{Z2}} <: Tr2base{X,Y}), Tr1base{X}
    # into:  :Tr2, [:(X{Z1}),:(Y{Z2})], :(Tr2{X,Y}), :((Tr2base{X,Y},Tr1base{X}))
    # the supertraits' parameters are redundant and, if given, are stripped out. So the following
    # would produce the same output
    #        :(Tr2{X{Z1},Y{Z2}} <: Tr2base{X{Z1},Y{Z2}}, Tr1base{X{Z1}})
    name, paras, trait, supertraits = parsecomp(def.args[1])
    for i in 2:length(def.args)
        c = def.args[i]
        if !Base.Meta.isexpr( c, :curly )
            error( "Traits: supertrait #" * string(i) * " is not a curly (Trait) expression" )
        end
        push!( supertraits.args, Expr( :curly, c.args[1],
            map( x->typeof(x)==Symbol ? x : Base.Meta.isexpr( x, :curly )? x.args[1] : error( "Traits: unknown " * string(x) ),
            c.args[2:end] )... ) )
    end
    return name, paras, trait, supertraits
end

function parsetraithead(def::Expr)
    # Transforms
    # :(Cmp{X,Y} <: Eq{X,Y}, Tr1{X})
    # into
    # trait = :(Cmp{X,Y})
    # supertraits = :(Eq{X,Y}, Tr1{X})
    # paras = [:X,:Y]
    #
    # Returns:
    # :(immutable Cmp{X,Y} <: Trait{(Eq{X,Y}, Tr1{X})} end)

    if def.head==:tuple # contains several parents
        name, paras, trait, supertraits = parsetuple(def)
    elseif def.head==:comparison # contains a <:
        name, paras, trait, supertraits = parsecomp(def)
    elseif def.head==:curly # no parents
        name, paras, trait, supertraits = parsecurly(def)
    else
        error("Interface specification error")
    end
    # check supertraits<:Traits
    for i =1:length(supertraits.args)
        st = supertraits.args[i].args[1]
        eval_curmod(:(@assert istraittype($st)))
    end
    # make :(immutable Cmp{X,Y} <: Trait{(Eq{X,Y}, Tr1{X})} end)
    out = :(immutable $trait <: Traits.Trait{$supertraits} end)

    # capture type parameters e.g. the Y in Monad{X{Y}}
    headassoc = Symbol[]
    for p in paras
        if Base.Meta.isexpr( p, :curly )
            @assert( typeof( p.args[2] ) == Symbol )
            if !in( p.args[2], headassoc )
                push!( headassoc, p.args[2] )
            end
        end
    end
    return out, name, paras, headassoc
end

# 2) parse the function definitions
###

function parsebody(name::Symbol, body::Expr, paras::Array{Any,1}, headassoc::Array{Symbol,1} )
    # Transforms:
    # body = quote
    #     R = g(X)
    #     f1(X,Y) -> X,Int
    #     f2(Y) -> X
    #     @constraints begin
    #          X==Y
    #     end
    # end
    #
    # into
    # :([f1 => ((X,Y), (Int,Int)),
    #    f2 => ((Y,),  (X,)) ] )
    # :(Bool[X==Y])
    isassoc(ex::Expr) = ex.head==:(=) # associated types
    isconstraints(ex::Expr) = ex.head==:macrocall # constraints

    outfns = Expr(:dict)
    constr = :(Bool[])
    assoc = quote end
    params_rename = Dict{Symbol,Symbol}()
    local_typesyms = Set{Symbol}()
    push!( local_typesyms, name )
    for p in paras
        if typeof( p ) == Symbol
            push!( local_typesyms, p )
        end
    end
    # but first, add the assoc types from the head
    for s in headassoc
        local s_inited::Bool = false
        for (i,p) in enumerate( paras )
            if Base.Meta.isexpr( p, :curly ) && p.args[2] == s
                rootsym = symbol( string(p.args[1],"0") )
                hosttype = p.args[1]
                tailsym = symbol( string(p.args[1],"0_" ) )
                params_rename[ hosttype ] = rootsym
                push!( assoc.args, :($rootsym = Traits.tparprefix( $name, Val{$i}, $hosttype ) ) )
                if !s_inited
                    push!( assoc.args, :($s       = Traits.tparget( $name, Val{$i}, $hosttype ) ) )
                    s_inited=true
                else
                    teststmt = :( @assert( isequal( $s,  Traits.tparget( $name, Val{$i}, $hosttype ) ) ) )
                    #push!( teststmt.args, @sprintf( "In %s, %s does not match an earlier definition", p, s ) )
                    #@show( teststmt )
                    push!( assoc.args, teststmt )
                end
                push!( assoc.args, :($tailsym = Traits.tparsuffix( $name, Val{$i}, $hosttype ) ) )
                push!( local_typesyms, rootsym )
                push!( local_typesyms, s )
                push!( local_typesyms, tailsym )
            end
        end
    end

    for ln in Lines(body)
        if isconstraints(ln)
            parseconstraints!(constr, ln)
        elseif isassoc(ln)
            push!(assoc.args, ln)
        else # the rest of the body are function signatures
            parsefnstypes!(outfns, ln, local_typesyms, params_rename )
        end
    end
    # store associated types:
    tmp = :(TypeVar[])
    for ln in Lines(assoc)
        if Base.Meta.isexpr( ln, :macrocall )
            continue
        end
        if endswith( string(ln.args[1]), "0_" )
            continue
        end
        tvar = ln.args[1]
        stvar = string(tvar)
        push!(tmp.args, :(TypeVar(symbol($stvar) ,$tvar)))
    end
    push!(assoc.args, :(assoctyps = $tmp))
    return outfns, constr, assoc
end

# 2.5) parse constraints
####
# Note, @constraints is not really a macro-call.
function parseconstraints!(constr, block)
    # updates constr=Expr(:ref,...)
    if !(block.args[1]==symbol("@constraints"))
        throw(TraitException(
        "Only @constraints blocks allowed inside trait definition"))
    end
    for ln in Lines(block.args[2])
        push!(constr.args, ln)
    end
end

function parsefnstypes!(outfns::Expr, lnargs::Expr, local_typesyms::Set{Symbol}, params_rename::Dict{Symbol,Symbol} )
    ln = deepcopy( lnargs )
    argreplace!( ln, params_rename )
    rootsyms = values( params_rename )

    function addargscurlytails!( def )
        if typeof( def ) == Expr
            for a in def.args
                addargscurlytails!( a )
            end
            if Base.Meta.isexpr( def, :curly )
                if in( def.args[1], rootsyms )
                    push!( def.args, Expr( :(...), symbol( string(def.args[1])*"_" ) ) )
                end
            end
        end
    end

    function parsefn(def)
        # Parse to get function signature.
        # parses f(X,Y), f{X <:T}(X,Y) and X+Y
        tvars = Any[]
        if isa(def.args[1], Symbol)
            fn = def.args[1]
        elseif def.args[1].head==:curly
            fn = def.args[1].args[1]
            # get
            tvars = def.args[1].args[2:end]
        else
            throw(TraitException(
                             "Something went wrong parsing the trait definition body with line:\n$ln"))
        end
        argtype = :()
        for i = 2:length( def.args )
            a = deepcopy( def.args[i] )
            addargscurlytails!( a )
            if Base.Meta.isexpr( a, :(::) ) # shorthand
                t = a.args[1]
                push!( argtype.args, Expr( :curly, :Type, t ) )
            else
                push!( argtype.args, a )
            end
        end
        return fn, argtype, tvars
    end
    function parseret!(rettype, ln)
        # parse to get return types
        while ln.head!=:block
            ln = ln.args[end]
        end
        tmp = rettype.args
        rettype.args = Any[]
        push!(rettype.args, ln.args[end])
        append!(rettype.args, tmp)
    end


    rettype = :()
    tuplereturn = false
    if ln.head==:tuple
        tuplereturn = true
        # several ret-types:
        # f1(X,Y) -> X,Y
        append!(rettype.args, ln.args[2:end])
        ln = ln.args[1]
    end

    if ln.head==:(->) # f1(X,Y) -> x
        parseret!(rettype, ln)
        fn, argtype, tvars = parsefn(ln.args[1])
    elseif ln.head==:call # either f1(X,Y) or X + Y -> Z
        if isa(ln.args[end], Expr) && ln.args[end].head==:(->) # X + Y -> Z
            def = Expr(:call)
            append!(def.args, ln.args[1:end-1])
            if length(ln.args)==2
                append!(def.args, ln.args[end].args[1].args)
            else
                push!(def.args, ln.args[end].args[1])
            end
            parseret!(rettype, ln)
        else # f1(X,Y)
            def = ln
            rettype =  :(Any,)
        end
        fn, argtype, tvars = parsefn(def)
    else
        throw(TraitException(
                             "Something went wrong parsing the trait definition body with line:\n$ln"))
    end
    # replace types with constraints by TypeVars
    trans = Dict(zip([t.args[1] for t in tvars], tvars))  # this will error if there is a type-var without constraints!
    translate!(argtype.args, trans)
    tvar2tvar!(argtype.args)
    subt2tvar!(rettype.args)
    translate!(rettype.args, trans)
    tvar2tvar!(rettype.args)

    # if return is not a tuple, ditch the tuple
    if !tuplereturn
        rettype = rettype.args[1]
    end

    push!(outfns.args, :($fn => ($argtype, $rettype)))
end

# 3) piece it together
###

@doc """The `@traitdef` macro is used to construct a trait.  Example:

     ```
     @traitdef MyArith{X,Y} begin
         # associated types
         Z = promote_type(X,Y)
         D = (X,Y)<:(Integer, Integer) ? Float64 : Z

         # method signatures
         +(X,Y) -> Z
         -(X,Y) -> Z
         *(X,Y) -> Z
         /(X,Y) -> D

         # constraints on X,Y
         @constraints begin
             X<:Number
             Y<:Number
         end
     end
     istrait(MyArith{Int, Int8}) # -> true
     ```

     - Assignments are for associated types, here `Z,D`.  These are
       types which can be calculated from the input types `X,Y`

     - Function signature definitions which are of the form `fn(X,Y,
       Other-Types) -> Return-Types`.  The return types can be left away

     - Constraints are marked in a block `@constaints`.  The are
       constraints in terms of the input types `X,Y` and are evaluated
       at trait checking.

     Traits can subtrait others:

     ```
     @traitdef MyInv{X}<:MyArith{X,X} begin
         inv(X) -> X
     end
     istrait(MyInv{Int}) # -> false
     istrait(MyInv{Float64}) # -> true
     ```

     A trait can be declare to accept a type parameter:
    ```
    @traitdef SemiFunctor{X{Y}} begin
        fmap( Function, X{Y} ) -> Any # we ignore the output sig for now
    end
    fmap{T}( f::Function, a::Nullable{T}) = Nullable(f(a))
    istrait(SemiFunctor{Nullable{Int}}) # -> true
    ```

    By default, the type parameter in question must be the last one. To override this (e.g. Array), see @traitimpl
     """ ->
macro traitdef(head, body)
    ## make Trait type
    traithead, name, paras, headassoc= parsetraithead(head)
    # make the body
    meths, constr, assoc = parsebody(name, body, paras, headassoc)
    # make sure a generic function of all associated types exisits

    traitbody = quote
        methods::Dict{Union(Function,DataType), Tuple}
        constraints::Vector{Bool}
        assoctyps::Vector{TypeVar}
        function $((name))()
            $assoc
            new( $meths, $constr, assoctyps)
        end
    end
    # add body to the type definition
    traithead.args[3] = traitbody
    return esc(traithead)
end
