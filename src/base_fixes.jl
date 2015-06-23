# patching issues in base

export indextype

# This just redefines isbits as is, because of a strange bug, see commit  76ec7fba3a88e
#Base.isbits(t::TypeConstructor) = false

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

# eltype for dicts
Base.eltype{K,V}(::Type{Associative{K,V}}) = V
Base.eltype{K}(::Type{Associative{K}}) = Any
Base.eltype(::Type{Associative}) = Any

indextype{K}(::Type{Associative{K}}) = K
indextype(::Type{Associative}) = Any
indextype(::Type{Associative}) = Any
indextype(t::DataType) = eltype(super(t))

indextype(::Any) = Int # fall back...

println("  endof ok-warning.")


# iterate over Tuple. Remove after merge of https://github.com/JuliaLang/julia/pull/11547
Base.length{T<:Tuple}(t::Type{T}) = length(t.parameters)
Base.start{T<:Tuple}(::Type{T}) = 1
Base.next{T<:Tuple}(t::Type{T}, state) = (t.parameters[state], state+1)
Base.done{T<:Tuple}(t::Type{T}, state) = length(t)<state

# indexing into Tuple
Base.getindex{T<:Tuple}(t::Type{T}, i::Integer) = t.parameters[i]

