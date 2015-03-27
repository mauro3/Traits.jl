# Type-traits are used to formally specify the interface of an
# abstract type.

# @traitdef_type AbstractArray{T,N} begin
#     size(AbstractArray) -> NTuple{N,Int}
#     getindex(AbstractArray, Int) -> T
# end

# should be translated into:

immutable Trait_AbstractArray1{X} <: Traits.Trait{()}
    methods::Dict
    constraints::Array{Bool,1}
    assoctyps::Array{Any,1}
    function Trait_AbstractArray1()
        T = X.parameters[1]
        N = X.parameters[2]
        assoctyps = [TypeVar(:T, T), N]
        new(Dict(
                 size => ((X,), NTuple{N,Int}),
                 getindex => ((X,Integer), T)
                 ),
            Bool[X<:AbstractArray],
            assoctyps
            )
    end

end

@test istrait(Trait_AbstractArray1{Array}, verbose=true)
@test istrait(Trait_AbstractArray1{Array{Int}}, verbose=true)
@test istrait(Trait_AbstractArray1{Array{Int,2}}, verbose=true)

# Problems: too permissive?  istrait(Trait_AbstractArray1{Array})==true
type Ar1<:AbstractArray
end
@test !istrait(Trait_AbstractArray1{Ar1})

type Ar2{T,N}<:AbstractArray{T,N}
end
@test !istrait(Trait_AbstractArray1{Ar2})
@test !istrait(Trait_AbstractArray1{Ar2{Int,2}})

type Ar3{T,N}<:AbstractArray{T,N}
end
Base.size{T,N}(a::Ar3{T,N}) = NTuple{N,Int}(tuple(Int[i for i=1:N]...))
Base.getindex{T,N,I<:Integer}(a::Ar3{T,N}, i::I) = one(T)
@test !istrait(Trait_AbstractArray1{Ar3})
@test istrait(Trait_AbstractArray1{Ar3{Int,2}}, verbose=true)

#########
# using supertraits instead

@traitdef ArrayLike{X} begin
    T = X.parameters[1]  # eltype
    N = X.parameters[2]  # dimension
    size(X) -> NTuple{N,Int}
    getindex(X, Int) -> T
end

# @traitdef_type AbstractArray <: ArrayLike{AbstractArray} begin
# end

immutable Trait_AbstractArray2{X} <: Traits.Trait{(ArrayLike{X},)}
    methods::Dict
    constraints::Array{Bool,1}
    assoctyps::Array{Any,1}
    function Trait_AbstractArray2()
        assoctyps = []
        new(Dict(
                 ),
            Bool[X<:AbstractArray],
            assoctyps
            )
    end

end

@test istrait(Trait_AbstractArray2{Array}, verbose=true)
@test istrait(Trait_AbstractArray2{Array{Int}}, verbose=true)
@test istrait(Trait_AbstractArray2{Array{Int,2}}, verbose=true)
# Problems: too permissive?  istrait(Trait_AbstractArray2{Array})==true

@test !istrait(Trait_AbstractArray2{Ar1})

@test !istrait(Trait_AbstractArray2{Ar2})
@test !istrait(Trait_AbstractArray2{Ar2{Int,2}})

@test !istrait(Trait_AbstractArray2{Ar3})
@test istrait(Trait_AbstractArray2{Ar3{Int,2}}, verbose=true)
