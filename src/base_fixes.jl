# patching issues in base

# https://github.com/JuliaLang/julia/pull/10164
Base.eltype{N}(::Type{AbstractArray{N}}) = N

# https://github.com/JuliaLang/julia/issues/10178#issuecomment-74136186
println("  This warning is ok:")
function Base.func_for_method(m::Method, tt, env)
    if !m.isstaged
        return m.func.code
    else
        f=ccall(:jl_instantiate_staged,Any,(Any,Any,Any),m,tt,env)
        return f.code
    end
end
println("  endof ok-warning.")
