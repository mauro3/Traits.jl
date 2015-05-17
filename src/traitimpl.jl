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

function parse_body(body::Expr)
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

     Notes

     - the type annotations are mandatory.  No parameterized methods
       are allowed (for now).
     """ ->
macro traitimpl(head, body)
    ## Parse macro header
    name, paras, trait_expr = parsecurly(head)

    ## Parse macro body 
    implfs = parse_body(body)
    #check_macro_body(body.args, implfs, trait) # doesn't work with associated types

    out = quote
        ## Check supertraits are implemented:
        if !istrait(traitgetsuper($trait_expr))
            istrait(traitgetsuper($trait_expr); verbose=true)
            throw(TraitException("""Not all supertraits of $($trait_expr) are implemented.
                                 Implement them first."""))
        end
    end
    ## Make methods
    for (fn, fndef) in implfs
        modname = module_name(Base.function_module(fn, Tuple{Vararg{Any}}))
        prefix_module!(fndef, modname)
        push!(out.args,fndef)
    end
    
    ## Assert that the implementation went smoothly
    push!(out.args, :(istrait($trait_expr) ? nothing :  @assert istrait($trait_expr, verbose=true)))
    return esc(out)
end
