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
# - more checks?

function get_fname(ex::Expr)
    # :(sin(x::T1) = sin(x.t1))
    # -> :sin
    out = ex.args[1].args[1]
    if typeof(out)==Symbol
        return out
    elseif out.head==:curly
        out = out.args[1]
    else
        error("Could not parse function definition: $ex")
    end
end
function get_fsig(ex::Expr) 
    ex.args[1].args[2:end]
end

function check_macro_body(bodyargs, implfs, trait)
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
    implfs = Dict()
    for ln in body.args
        if isa(ln, Expr) && !(ln.head==:line)
            if !isdefined(get_fname(ln))
                # define a standard generic function:
                eval_curmod(:($(get_fname(ln))() = error("Not defined")))
            end
            implfs[eval_curmod(get_fname(ln))] = ln
        end
    end
    return implfs
end

function prefix_module!(ex::Expr, modname::Symbol)
    # Prefix the module of the function, so import is not necessary:
    # :(+(x::T1, y::T2) = x.t1 + y.t2) -> :(Base.+(x::T1, y::T2) = x.t1 + y.t2)
    ex.head== :(=) || error("Not a function definition: $ex")
    ex.args[1].head==:call || error("Not a function definition: $ex")
    
    fnname = get_fname(ex)
    if typeof(fnname)==Symbol
        ex.args[1].args[1] = :($modname.$fnname)
    elseif fnname.head==:(.)
        # has a module already prefixed
    else
        error("something went wrong, $fnname is not a Symbol")
    end
end

macro traitimpl(head, body)
    ## Parse macro header
    name, paras, trait_expr = parsecurly(head)
    trait = eval_curmod(trait_expr)

    ## Check supertraits are implemented:
    if !(istrait(traitgetsuper(trait); verbose=true))
        throw(TraitException("""Not all supertraits of $trait are implemented.
             Implement them first."""))
    end
    ## Parse macro body 
    implfs = parse_body(body)
    check_macro_body(body.args, implfs, trait)

    ## Make methods
    out = quote end
    for (fn, fndef) in implfs
        modname = module_name(Base.function_module(fn))
        prefix_module!(fndef, modname)
        push!(out.args,fndef)
    end
    
    ## Assert that the implementation went smoothly
    push!(out.args, :(@assert istrait($trait_expr)))
    return esc(out)
end
