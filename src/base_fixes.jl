# patching issues in base

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


# eltype for dicts
Base.eltype{K}(::Type{Associative{K}}) = (K,Any)
Base.eltype(::Type{Associative}) = (Any,Any)

# iterate over Tuple
Base.length{T<:Tuple}(t::Type{T}) = length(t.parameters)
Base.start{T<:Tuple}(::Type{T}) = 1
Base.next{T<:Tuple}(t::Type{T}, state) = (t.parameters[state], state+1)
Base.done{T<:Tuple}(t::Type{T}, state) = length(t)<state

# indexing into Tuple
Base.getindex{T<:Tuple}(t::Type{T}, i::Integer) = t.parameters[i]

