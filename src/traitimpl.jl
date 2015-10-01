######################
# Trait implementation
######################
#
# Provides @traitimpl macro which will do:
#  - check supertraits are implemented
#  - find module where the generic function is defined
#  - make methods
#
# Example syntax:
# @traitimpl Cmp{MyType1, MyType1} begin
#     isless(x::MyType1, y::MyType1) = x.t<y.t
# end
#
# ToDo:
# - implement parametric methods
# - more checks?

function get_fname(ex::Expr)
    # :(sin{T1<:Interger}(x::T1) = sin(x.t1))
    # -> :sin{T1<:Interger}
    out = ex.args[1].args[1]
    if typeof(out)==Symbol || out.head==:curly
        return out
    else
        error("Could not parse function definition: $ex")
    end
end

function get_fname_only(ex)
    # :(sin{T1<:Interger}(x::T1) = sin(x.t1))
    # -> :sin
    ex = get_fname(ex)
    isa(ex,Symbol) ? ex : ex.args[1]
end

function get_fsig(ex::Expr) 
    ex.args[1].args[2:end]
end

function check_macro_body(bodyargs, implfs, trait)
    # TODO: this check is broken
    
    # check that there are no double defs
    length(trait().methods)==length(implfs) || error("Duplicate method definition(s)")
    # check right number of defs
    length(trait().methods)==length(implfs) || error("Not right number of method definitions")
    # check that the signature of implfs agrees with trait().methods
    for (f,sig) in trait().methods
        # for now just check length.
        if length(sig[1])!=length(get_fsig(implfs[f])) 
            error("""Method definition:
                     $f  $sig
                     does not match implementation:
                     $(implfs[f])""")
        end
    end
    nothing
end

function parse_impl_body(name::Symbol, paras::Array{Union{Symbol,Expr},1}, body::Expr)
    # TODO try and remove eval:
    implfs = Dict()
    for ln in Lines(body)
        if !isdefined(get_fname_only(ln))
            # define a standard generic function:
            eval_curmod(:($(get_fname_only(ln))() = error("Not defined")))
        end
        implfs[eval_curmod(get_fname_only(ln))] = ln
    end
    return implfs
end

function prefix_module!(ex::Expr, modname::Symbol)
    # Prefix the module of the function, so import is not necessary:
    # :(+(x::T1, y::T2) = x.t1 + y.t2) -> :(Base.+(x::T1, y::T2) = x.t1 + y.t2)
    if ex.head== :(=)
        if ex.args[1].head!=:call
            error("Not a function definition :\n$ex")
        end
    elseif ex.head!= :function
        error("Not a function definition:\n$ex")
    end
    
    fnname = get_fname(ex)
    fnname_only = get_fname_only(ex)    
    if isa(fnname, Symbol)
        ex.args[1].args[1] = :($modname.$fnname)
    elseif fnname.head==:curly
        if isa(fnname.args[1], Symbol)
            ex.args[1].args[1].args[1] = :($modname.$fnname_only)
        end
    else
        error("something went wrong, $fnname is not a Symbol")
    end
    nothing
end

function parse_impl_subcurly!( def::Expr, traitname::Symbol, pos::Int, paras::Array, traitexpr::Expr, arrows::Array, paramcount::Array{Int,1} )
    parse_impl_vanilla!( def.args[1], traitname, pos, paras, traitexpr, arrows, paramscount )
end

function parse_impl_arrow!( def::Expr, traitname::Symbol, pos::Int, paras::Array, traitexpr::Expr, arrows::Array, paramcount::Array{Int,1} )
    # expect the left must be a curly
    # check if the curly size matches expectations
    lhs = def.args[1]
    if !Base.Meta.isexpr( lhs, :curly )
        throw( string( def ) * "  LHS="*string(lhs)*". Expect curly {...} " )
    end
    lhsnames = lhs.args[2:end]
    # the RHS of the -> is a block, with the first arg being location expr
    rhs = def.args[2].args[2]
    # the 2nd line is either a symbol, or a tuple of symbols
    if typeof( rhs ) == Symbol
        rhsnames = Any[ rhs ]
    elseif Base.Meta.isexpr( rhs, :tuple )
        rhsnames = rhs.args
    end
    # we make sure that all RHS symbols are present inside the LHS curly
    for s in rhsnames
        if !in(s,lhsnames)
            throw( "RHS symbol " * string(s) * " not found in LHS curly" )
        end
    end
    # AND we make sure that the length of RHS symbols are equal to expected counts
    if length(rhsnames) != paramcount[pos]
        foundn = length(rhsnames )
        n = paramcount[pos]
        throw( string( traitname ) * " expects " * string(n) * " parameters. Found " * string(foundn) )
    end
    arrowexpr = :( Traits.traitparams{}(::Type{Val{}}, ::Type{$lhs}, ::Type{Val{$pos}} ) = tuple() )
    # this populate the first curly
    append!( arrowexpr.args[1].args[1].args, lhsnames )
    # this populates the 2nd curly
    push!( arrowexpr.args[1].args[2].args[1].args[2].args, Expr( :quote, traitname ) )
    # this populates the last curly
    append!( arrowexpr.args[2].args[2].args, rhsnames )
    push!( arrows, arrowexpr )

    # there should not be more statements afterwards
    push!( paras, lhs )
    push!( traitexpr.args, lhs )
end

function parse_impl_vanilla!( arg::Union{Expr,Symbol}, traitname::Symbol, pos::Int, paras::Array, traitexpr::Expr, arrows::Array, paramscount::Array{Int,1} )
    if paramscount[pos] > 0
        # make sure the datatype represented by arg has at least that many
        # parameters
        n = paramscount[pos]
        dt = eval_curmod( arg )
        dtn = length( dt.parameters )
        if dtn < n
            throw( "trait "*string(traitname)*" requires " * 
            string(arg) * " to have "*string(n)*"+ parameters.")
        end
        names = map( x->x.name, dt.parameters )
        arrowexpr = :( Trait.traitparams{}(::Val{}, ::$arg{}, ::Val{$pos} ) = tuple() )
        # this populate the first curly
        append!( arrowexpr.args[1].args[1].args, names )
        # this populates the 2nd curly
        push!( arrowexpr.args[1].args[2].args[1].args, Expr( :quote, traitname ) )
        # this populates the 3rd curly
        append!( arrowexpr.args[1].args[3].args[1].args, names )
        # this populates the last curly
        append!( arrowexpr.args[2].args[2].args, names[end-(n-1):end] )

        # the end result is something like
        # Trait.traitparams{A,B}(::Val{:Tr},::Array{A,B},::Val{1}) = tuple(A,B)

        push!( arrows, arrowexpr )
    end
    push!( paras, arg )
    push!( traitexpr.args, arg )
end

function parse_impl_arg!( arg::Union{Expr,Symbol}, traitname::Symbol, pos::Int, paras::Array, traitexpr::Expr, arrows::Array, paramscount::Array{Int,1} )
    if Base.Meta.isexpr( arg, :-> )
        parse_impl_arrow!( arg, traitname, pos, paras, traitexpr, arrows, paramscount )
    elseif Base.Meta.isexpr( arg, :curly )
        parse_impl_subcurly!( arg, traitname, pos, paras, traitexpr, arrows, paramscount )
    elseif typeof( arg ) == Symbol || Base.Meta.isexpr( arg, :(.) )
        parse_impl_vanilla!( arg, traitname, pos, paras, traitexpr, arrows, paramscount )
    else
        throw( "unknown traitimpl expression head " * string(arg))
    end
end

function parse_impl_curly(def::Expr)
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
    traitexpr = Expr( :curly, name )
    arrows = Expr[]

    paramscount = traitparamscount( name )
    for i=2:length(def.args)
        parse_impl_arg!( def.args[i], name, i-1, paras, traitexpr, arrows, paramscount[(i-1):end] )
    end

    if length( paras ) != length( paramscount )
        throw( string( def )* ": length of params=" * string(length(paras))*
            ". Expect "*string(length(paramcount)))
    end
    return name, paras, traitexpr, arrows
end

@doc """The `@traitimpl` macro can be used to implement a trait for a
    set of types.  Note however, that traits are also be implemented
    implicitly, i.e. any set of types is part of a trait if it
    fulfills it.

    Example continuing from the documentation of `@traitdef`, implementing the
    `MyArith` trait:

    ``` 
    type A; a end; type AB; b end
    @traitimpl MyArith{A,AB} begin
     +(x::A,y::AB) = A(x.a+y.b)
     -(x::A,y::AB) = A(x.a-y.b)
     *(x::A,y::AB) = A(x.a*y.b)
     /(x::A,y::AB) = A(x.a/y.b)
    end
    istrait(MyArith{A, AB}) # -> true
    ```

    if a trait accepts a type parameter, by default it is the last one
    ```
    @traitdef SemiFunctor{X{Y}} begin
     fmap( Function, X{Y}) -> X{Y}
    end
    @traitimpl SemiFunctor{Nullable{Y}} begin
     fmap{Y}( f::Function, x::Nullable{Y} ) = Nullable(f(x.value))
    end
    istrait( SemiFunctor{Nullable{Int} }) # -> true
    ```
    However, for Array type, we could still use SemiFunctor. However,
    Array takes 2 parameters, so we have to guide Traits.jl to the right
    one for the current Trait context, using the additional
    @traitimpl declaration syntax, like so:
    ```
    @traitimpl SemiFunctor{ Array{K,N} -> K } begin
     fmap{Y}( f::Function, x::Array{Y,1} ) = map(f, x)
    end
    istrait( SemiFunctor{Array{Int,1} }) # -> true
    istrait( SemiFunctor{Array{Int,2} }) # -> false
     ```

    This is particularly useful when the parameter order required by
    the Trait and the parameter order defined for a type is not the same.

    Behind the scene, the form
    ```
    Array{K,N} -> K
    ```
    is transformed into a traitparams method declaration
    ```
    function traitparams{K,N}( ::SemiFunctor, ::Array{K,N}, ::Val{1} ) = K
    ```
    which reads as "when the first trait argument
    is Array{K,V} for SemiFunctor, the relevant parameter for it is K".

    If the arrow construct is not given it is assumed that
    * when the trait requires n parameters from the type, the last nth
      parameters of the datatype in their natural order would be them.
    * if there fewer parameters than required, it would throw here.
    """ ->
macro traitimpl(head, body)
    ## Parse macro header
    if VERBOSE
        println( "parse header")
    end
    name, paras, trait_expr, arrows = parse_impl_curly(head)

    if VERBOSE
        println( "arrows:\n", arrows )
        println( "parse body")
    end
    ## Parse macro body
    implfs = parse_impl_body(name,paras,body)
    #check_macro_body(body.args, implfs, trait) # doesn't work with associated types
    println( "prepare output")

    out = quote
    end

    if isempty( arrows )
        push!( out, :(
            ## Check supertraits are implemented:
            if !istrait(traitgetsuper($trait_expr))
                istrait(traitgetsuper($trait_expr); verbose=true)
                throw(TraitException("""Not all supertraits of $($trait_expr) are implemented.
                                     Implement them first."""))
            end ) )
    end
    for a in arrows
        push!(out.args, a)
    end
    ## Make methods
    for (fn, fndef) in implfs
        modname = module_name(Base.function_module(fn, Tuple{Vararg{Any}}))
        prefix_module!(fndef, modname)
        push!(out.args,fndef)
    end

    ## Assert that the implementation went smoothly
    if isempty( arrows )
        push!(out.args, :(istrait($trait_expr) ? nothing :  @assert istrait($trait_expr, verbose=true)))
    end

    if VERBOSE
        println( out )
    end

    return esc(out)
end
