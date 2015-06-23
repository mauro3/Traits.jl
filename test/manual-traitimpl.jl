## Testing hand-coded trait implementations
###########################################

# impl will do:
#  - check supertraits are implemented
#  - import bindings
#  - make methods

# @traitimpl D2{T1, T2} begin
# +(x::T1, y::T2) = x.t1 + y.t2
# -(x::T1, y::T2) = x.t1 - y.t2
# end

# becomes:

# helper functions
function get_fnname(ex::Expr) 
    out = ex.args[1].args[1]
    if typeof(out)==Symbol
        return out
    elseif out.head==:curly
        out = out.args[1]
    else
        error("Could not parse function definition: $ex")
    end
end
get_fnsig(ex::Expr) = ex.args[1].args[2:end]
function prefix_module!(ex::Expr, modname::Symbol)
    # prefix the module of the function:
    # :(+(x::T1, y::T2) = x.t1 + y.t2) -> :(Base.+(x::T1, y::T2) = x.t1 + y.t2)
    ex.head== :(=) || error("Not a function definition: $ex")
    ex.args[1].head==:call || error("Not a function definition: $ex")
    
    fnname = get_fnname(ex)
    if typeof(fnname)==Symbol
        ex.args[1].args[1] = :($modname.$fnname)
    elseif fnname.head==:(.)
        # has a module already prefixed
    else
        error("something went wrong, $fnname is not a Symbol")
    end
end

type T1
t1::Float64
end
type T2
t2::Float64
end

# functions in trait definition
implfns = [:(+(x::T1, y::T2) = x.t1 + y.t2),
           :(-(x::T1, y::T2) = x.t1 - y.t2)]
#fn3 = :(sin(x::T1) = sin(x.t1))

# check supertraits are satisfied
Base.sin(x::T1) = sin(x.t1)
Base.sin(x::T2) = sin(x.t2)
Base.cos(x::T1) = cos(x.t1)
Base.cos(x::T2) = cos(x.t2)
@assert istrait(traitgetsuper(D2{T1, T2}))

# make dict of implfns
tmp = implfns
implfns = Dict()
for implfn in tmp
    implfns[eval(get_fnname(implfn))] = implfn
end
# check that there are no double defs
length(tmp)==length(implfns) || error("Duplicate method definition(s)")

# check right number of defs
length(D2{T1,T2}().methods)==length(implfns) || error("Not right number of method definitions")
# check that the signature of fns agrees with D2{T1,T2}().methods
for (fn,_fn) in D2{T1,T2}().methods
    # for now just check length
    if length(_fn.env.defs.sig)-1!=length(get_fnsig(implfns[fn])) # -1 because of return type
        error("""Method definition:
                 $fn  $sig
                 does not match implementation:
                 $(implfns[fn])""")
    end
end

# make method definitions
for (fn, fndef) in implfns
    modname = module_name(Base.function_module(fn, Tuple{Vararg{Any}}))
    # I think meta-programming is needed to make the new function def
    prefix_module!(fndef, modname)
    eval(fndef)
end

# checks
@assert istrait(D2{T1, T2})
@assert T1(5) + T2(4)==5+4
