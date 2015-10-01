####################
## Trait definitions
####################
#
# i.e. implement the @traitdef macro
#
# It looks like
# @traitdef Cmp{X,Y} <: Eq{X,Y} begin
#     T = eltype(X) # associated type
#     isless(x,y) -> Bool
#     @constraints begin
#         X==Y
#     end
# end

traitparamscount_registry = Dict{Symbol, Array{Int,1}}()

# this function is augmented on-the-fly by additional methods declared using
# traitimpl macro
traitparams{S,T,N}( ::Type{Val{S}}, ::T, ::Type{Val{N}} ) = (T.parameters[end],)

function traitparams_delayed{T}( s::Symbol, dt::Type{T}, n::Int )
    ex = :( Traits.traitparams( Val{$s}, $dt, Val{$n} ) )
    ex.args[2].args[2] = Expr( :quote, s )
    if VERBOSE
        println( ex )
    end
    eval( ex )
end

function traitparamscount( tr::Symbol )
    global traitparamscount_registry
    if haskey( traitparamscount_registry, tr )
        return traitparamscount_registry[ tr ]
    else
        return Int[0]
    end
end

# 1) parse the header
###
function parsecurly(def::Expr)
    # parses :(Cmp{x,y})
    # into: :Cmp, [:x,:y], :(Cmp{x,y}), ()

    # parses :(Monad{X{Y}})
    # into: :Monad, [:(X{Y})], :(Monad{X}), ()

    # multiple parametric trait:
    # parses :(ComboTr{X{Y}, Z{T}})
    # into: :ComboTr, [:(X{Y}), :(Z{T}) ]], :(ComboTr{X,Z}), ()

    # Note that if we have
    #       :(ComboTr{X{Y}, Z{Y}}) # note the same Y
    # The trait constructor will have an assertion that the parameter in X and Z must match
    name = def.args[1]
    paras = Union{Symbol,Expr}[]
    append!(paras,def.args[2:end])
    trait = Expr( :curly, name,
        map( x->typeof(x)==Symbol ? x : Base.Meta.isexpr( x, :curly )? x.args[1] : error( "Traits: unknown " * string(x) ),
        def.args[2:end])... )
    return name, paras, trait, :(Tuple{})
end

function parsecomp(def::Expr)
    # parses :(Cmp{x,y} <: Eq{x,y})
    # into:  :Cmp, [:x,:y], :(Cmp{x,y}), :(Tuple{Eq{x,y}})

    # parses :(Tr2{X{Z1},Y{Z2}} <: Tr2base{X,Y})
    # into:  :Tr2, [:(X{Z1}),:(Y{Z2})], :(Tr2{X,Y}), :((Tr2base{X,Y},))
    # the supertraits' parameters are redundant and, if given, are stripped out. So the following
    # would produce the same output
    #        :(Tr2{X{Z1},Y{Z2}} <: Tr2base{X{Z1},Y{Z2}})
    if  def.args[2]!=:<:
        error("not a <:")
    end
    name, paras, trait = parsecurly(def.args[1])

    if !Base.Meta.isexpr( def.args[3], :curly )
        error( "Traits: RHS of " * string( def ) * " must be a curly (Trait) expression" )
    end
    c = def.args[3]
    supertraittype = Expr( :curly, c.args[1],
        map( x->typeof(x)==Symbol ? x : Base.Meta.isexpr( x, :curly )? x.args[1] : error( "Traits: unknown " * string(x) ),
        c.args[2:end] )... )
    supertraits = :(Tuple{$(supertraittype)})
    return name, paras, trait, supertraits
end

function parsetuple(def::Expr)
    # parses :(Cmp{x,y} <: Eq{x,y}, Sz{x}, Uz{y})
    # into   :Cmp, [:x,:y], :(Cmp{x,y}), :(Tuple{Eq{x,y},Sz{x},Uz{y}})

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
    # trait = :(Cmp{X,Y}
    # supertraits = :(Tuple{Eq{X,Y}, Tr1{X}})
    # paras = [:X,:Y]
    # 
    # Returns:
    # :(immutable Cmp{X,Y} <: Trait{Tuple{Eq{X,Y}, Tr1{X}}} end)
    
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
    for i =2:length(supertraits.args)
        st = supertraits.args[i].args[1]
        eval_curmod(:(@assert istraittype($st)))
    end
    # make :(immutable Cmp{X,Y} <: Trait{(Eq{X,Y}, Tr1{X})} end)
    out = :(immutable $trait <: Traits.Trait{$supertraits} end)

    # capture type parameters e.g. the Y in Monad{X{Y}}
    paramscount = Int[]
    for p in paras
        if Base.Meta.isexpr( p, :curly )
            for i=2:length(p.args)
                @assert( typeof( p.args[i] ) == Symbol )
            end
            push!( paramscount, length(p.args)-1 )
        else
            push!( paramscount, 0 )
        end
    end
    global traitparamscount_registry

    if haskey( traitparamscount_registry, name ) && traitparamscount_registry[ name ] != paramscount
        println( "Trait " * string( name ) * " is redefined with a different params signature." )
    end
    traitparamscount_registry[ name ] = paramscount
    return out, name, paras
end

# 2) parse the function definitions
###

function parsebody(name::Symbol, body::Expr, paras::Array{Union{Symbol,Expr},1} )
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
    # :(Bool[X==Y]),
    # :(...associated types...)
    isassoc(ex::Expr) = ex.head==:(=) # associated types
    isconstraints(ex::Expr) = ex.head==:macrocall # constraints

    outfns = :(Traits.FDict())
    constr = :(Bool[])
    assoc = quote end
    local_typesyms = Set{Symbol}()
    push!( local_typesyms, name )
    # but first, add the assoc types from the head
    paramscount = traitparamscount( name )

    for (i,p) in enumerate( paras )
        if Base.Meta.isexpr( p, :curly )
            hosttype = p.args[1]
            names = p.args[2:end]

            lhsnames = map( x->in(x,local_typesyms ) ? symbol("__",x,i,"__") : x , names )

            # the reason we do eval_curmod is because we don't want
            # to precompile this part: the actual trait parameters
            # are defined later
            assocexpr = :( () = placeholder) # note placeholder
            append!( assocexpr.args[1].args, lhsnames )
            # to replace the placeholder above
            subexpr = :( Traits.traitparams_delayed(:$name, $hosttype, $i ) )
            subexpr.args[2] = Expr( :quote, name )
            assocexpr.args[2] = subexpr
            push!( assoc.args, assocexpr )

            for j in 1:length(names)
                push!( local_typesyms, names[j] )
                if lhsnames[j] != names[j]
                    lhs = lhsnames[j]
                    rhs = names[j]
                    teststmt = :(
                        if( $lhs != $rhs )
                            Base.throw( string( "Trait: ", " pos=", $(i), " giventype=",
                                String($hosttype) ," curlypos=",$(j), " expects ",String($rhs), ". got ", string($lhs) ) )
                        end )
                    teststmt.args[2].args[2].args[2].args[2] = "Trait: " * string(name)
                    push!( assoc.args, teststmt )
                end
            end
            push!( local_typesyms, hosttype )
        elseif typeof( p ) == Symbol
            push!( local_typesyms, p )
        end
    end
    for ln in Lines(body)
        if isconstraints(ln)
            parseconstraints!(constr, ln)
        elseif isassoc(ln)
            push!( local_typesyms, ln.args[1] )
            push!(assoc.args, ln)
        else # the rest of the body are function signatures
            parsefnstypes!(outfns, ln, local_typesyms )
        end
    end
    # store associated types
    tmp = :(Any[])
    for tvar in local_typesyms
        if tvar == name
            continue
        end
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

function parsefnstypes!(outfns::Expr, lnargs::Expr, local_typesyms::Set{Symbol} )
    ln = deepcopy( lnargs )

    # parse one line containing a function definition
    function parsefn(def)
        # Parse to get function signature.
        # parses f(X,Y), f{X <:T}(X,Y) and X+Y
        # into f and _f(...)

        getsymbol(fn) = symbol("__"*string(fn))
        
        _fn = deepcopy(def)
        if isa(def.args[1], Symbol) # f(X,Y) or X+Y
            fn = def.args[1]
            _fn.args[1] = getsymbol(fn)
        elseif def.args[1].head==:curly # f{X}(X,Y)
            fn = def.args[1].args[1]
            _fn.args[1].args[1] = getsymbol(fn)
        else
            throw(TraitException(
                  "Something went wrong parsing the trait function definition:\n$fn"))
        end
        # transform X->::X
        for i=2:length(_fn.args)
            _fn.args[i] = :(::$(_fn.args[i]))
        end
        return fn, _fn
    end
    function parseret(rettype, ln)
        # parse to get return types
        while ln.head!=:block
            ln = ln.args[end]
        end
        tmp = rettype
        rettype = Any[] # per-pend
        push!(rettype, ln.args[end])
        append!(rettype, tmp)
        return rettype
    end
    
    rettype = Any[]
    tuplereturn = false
    if ln.head==:tuple
        tuplereturn = true
        # several ret-types:
        # f1(X,Y) -> X,Y
        for r in ln.args[2:end]
            push!(rettype, r)
        end
        ln = ln.args[1]
    end
    
    if ln.head==:(->) # f1(X,Y) -> X
        rettype = parseret(rettype, ln)
        fn, _fn = parsefn(ln.args[1])
    elseif ln.head==:call # either f1(X,Y) or X + Y -> Z
        if isa(ln.args[end], Expr) && ln.args[end].head==:(->) # X + Y -> Z
            def = Expr(:call)
            append!(def.args, ln.args[1:end-1])
            if length(ln.args)==2
                append!(def.args, ln.args[end].args[1].args)
            else
                push!(def.args, ln.args[end].args[1])
            end
            rettype = parseret(rettype, ln)
        else # f1(X,Y)
            def = ln
            rettype =  Any[:(Any)]
        end
        fn, _fn = parsefn(def)
    else
        throw(TraitException(
                             "Something went wrong parsing the trait definition body with line:\n$ln"))
    end
    # if return is not a tuple, ditch the tuple
    if !tuplereturn
        rettype = :(::$(rettype[1]))
    else
        rettype = :(::Tuple{$(rettype...)})
    end
    tmp = _fn.args[2:end]
    _fn.args = _fn.args[1:1]
    push!(_fn.args, rettype)
    append!(_fn.args, tmp)
    # make _fn
    _fn = :($_fn = nothing)
    push!(outfns.args, :($fn => $_fn))
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
     """ ->
macro traitdef(head, body)
    ## make Trait type
    traithead, name, paras = parsetraithead(head)
    # make the body
    meths, constr, assoc = parsebody(name, body, paras )
    # make sure a generic function of all associated types exisits
    
    traitbody = quote
        methods::Traits.FDict
        constraints::Vector{Bool}
        assoctyps::Vector{TypeVar}
        function $((name))()
            $assoc
            new( $meths, $constr, assoctyps)
        end
    end
    # add body to the type definition
    traithead.args[3] = traitbody

    if VERBOSE
        println( traithead )
    end
    return esc(traithead)
end
