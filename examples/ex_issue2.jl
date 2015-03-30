######
# workaround for issue #2
##########
using Traits

@traitdef Foo_NotWorking{X} begin
    fnxx{Y<:Integer}(X, Vector{Y}) -> Integer
end

# This errors because of https://github.com/JuliaLang/julia/issues/9043
# 
# @traitimpl Foo_NotWorking{Float64} begin
#     fnxx(x::Float64, y::Array{Uint8,1}) = y[round(Integer,x)]
# end

@traitdef Foo{X} begin
    Y = getFooY(X) 
    getFooY(Type{X}) -> DataType
    fnx(X, Y) -> Integer
    @constraints begin
        eltype(Y)<:Integer
    end
end

@traitimpl Foo{Float64} begin
    getFooY(::Type{Float64}) = Array{Uint8,1}
    fnx(x::Float64, y::Array{Uint8,1}) = y[round(Integer,x)]
end

@traitimpl Foo{Int} begin
    getFooY(::Type{Int}) = Array{Integer,1}
    fnx(x::Int, y::Array{Integer,1}) = y[round(Integer,x)]
end

@traitfn tf89{X, Y<:Integer; Foo{X}}(x::X, a::Vector{Y}) = fnx(x, a) + 5

tf89(2., Uint8[1,2])
tf89(2, Integer[1,2])

# tf89(2., Int[1,2]) # errors

## or this also works:

@traitdef Bar{X} begin
    Y = getBarY(X) 
    getBarY(Type{X}) -> DataType
    gnx(X, Vector{Y}) -> Integer
    @constraints begin
        Y<:Integer
    end
end

@traitimpl Bar{Float64} begin
    getBarY(::Type{Float64}) = Uint8
    gnx(x::Float64, y::Array{Uint8,1}) = y[round(Integer,x)]
end

@traitimpl Bar{Int} begin
    getBarY(::Type{Int}) = Integer
    gnx(x::Int, y::Array{Integer,1}) = y[round(Integer,x)]
end

@traitimpl Bar{Int} begin
    getBarY(::Type{Int}) = Integer
    gnx(x::Int, y::Array{Integer,1}) = y[round(Integer,x)]
end

@traitimpl Bar{Int8} begin
    getBarY(::Type{Int8}) = Integer
    gnx{Y<:Integer}(x::Int8, y::Array{Y,1}) = y[round(Integer,x)]
end
istrait(Bar{Int8}, verbose=true)

@traitfn tf90{X, Y<:Integer; Bar{X}}(x::X, a::Vector{Y}) = gnx(x, a) + 5

tf90(2., Uint8[1,2])
tf90(2, Integer[1,2])
# tf90(2, Int[1,2]) # errors
tf90(Int8(2), Integer[1,2])
tf90(Int8(2), Int[1,2])
tf90(Int8(2), UInt[1,2])

# tf90(2., Int[1,2]) # errors
