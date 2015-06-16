# patching issues in base

# To work with Tuple-types/types
import TupleTypes: getpara

# https://github.com/JuliaLang/julia/issues/10178#issuecomment-74136186
println("  This warning is ok:")
function Core.Inference.func_for_method(m::Method, tt, env)
    if !m.isstaged
        return m.func.code
    else
        f=ccall(:jl_instantiate_staged,Any,(Any,Any,Any),m,tt,env)
        return f.code
    end
end
println("  endof ok-warning.")


# eltype for dicts
Base.eltype{K}(::Type{Associative{K}}) = (K,Any)
Base.eltype(::Type{Associative}) = (Any,Any)

