# Type-traits are used to formally specify the interface of an
# abstract type.

# Example AbstractArray, see
# https://github.com/JuliaLang/julia/issues/10064
#
# @traitdef_type AbstractArray{T,N} begin
#     size(AbstractArray) -> NTuple{N,Int}
#     getindex(AbstractArray, Int) -> T
# end

# should be translated into:

immutable Trait_AbstractArray{X} <: Traits.Trait{()}
    methods::Dict
    constraints::Array{Bool,1}
    assoctyps::Array{Any,1}
    function Trait_AbstractArray()
        T = X.parameters[1]
        N = X.parameters[2]
        assoctyps = [TypeVar(:T, T), N]
        new(Dict(
                 size => ((X,), NTuple{N,Int}),
                 getindex => ((X,Integer), T)
                 ),
            Bool[],
            assoctyps
            )
    end
end
# (Note that the user probably shouldn't need to use
# Trait_AbstractArray, but instead would just use AbstractArray in trait-functions after the ;
# @traitfn f{X; then what?
#
# @traitfn f{X; X::AbstractArray} ...
# or maybe better after all
# @traitfn f{X; Trait_AbstractArray{X}} ...

@test istrait(Trait_AbstractArray{Array}, verbose=true)
@test istrait(Array, verbose=true)

# these are not Traits as the size and getindex functions are not
# defined for AbstractArray just for its subtypes
@test !istrait(Trait_AbstractArray{AbstractArray}, verbose=true)
@test !istrait(AbstractArray)

@test istrait(Trait_AbstractArray{Array{Int}}, verbose=true)
@test istrait(Array{Int}, verbose=true)

@test istrait(Trait_AbstractArray{Array{Int,2}}, verbose=true)
@test istrait(Array{Int,2}, verbose=true)
@test istrait(Matrix{Int}, verbose=true)

# Problems: too permissive?  istrait(Trait_AbstractArray{Array})==true
type Ar1<:AbstractArray
end
@test !istrait(Trait_AbstractArray{Ar1})

type Ar2{T,N}<:AbstractArray{T,N}
end
@test !istrait(Trait_AbstractArray{Ar2})
@test !istrait(Trait_AbstractArray{Ar2{Int,2}})

type Ar3{T,N}<:AbstractArray{T,N}
end
Base.size{T,N}(a::Ar3{T,N}) = NTuple{N,Int}(tuple(Int[i for i=1:N]...))
Base.getindex{T,N,I<:Integer}(a::Ar3{T,N}, i::I) = one(T)
@test !istrait(Trait_AbstractArray{Ar3})
@test istrait(Trait_AbstractArray{Ar3{Int,2}}, verbose=true)
