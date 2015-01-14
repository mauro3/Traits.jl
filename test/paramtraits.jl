using Traits
using Base.Test

@traitdef SemiFunctor{X{Y}} begin
    fmap( Function, X{Y} ) -> Any
end

# this definition should satisfy the SemiFunctor requirement for Nullable
fmap{Y}( f::Function, x::Nullable{Y} ) = Nullable( f(x.value) )

println( "test SemiFunctor{Nullable}" )
@test istrait( SemiFunctor{Nullable{Int}})

# This change the default type parameter position.
# Note that it doesn't change the fact that Array has the same SemiFunctor
# trait, just in this case (Array) we should use the first type parameter
# in the trait context.
# There is no other easy way to do it.
@traitimpl SemiFunctor{ Array{Y...} } begin
    fmap{Y}( f::Function, x::Array{Y,1} ) = map(f, x)
end

# This test that the suffix would be (1,)
print( "Type parameter suffix for SemiFunctor{Array{Int,1}} == ")
println( Traits.tparsuffix( SemiFunctor, Val{1}, Array{Int,1} ) )
@test Traits.tparsuffix( SemiFunctor, Val{1}, Array{Int,1} ) == (1,)

println( "test istrait{ SemiFunctor{Array{Int,1}}} == true" )
@test istrait( SemiFunctor{Array{Int,1}} )
println( "test istrait{ SemiFunctor{Array{Int,2}}} == false" )
@test istrait( SemiFunctor{Array{Int,2}} ) == false

println( "test multi-field traits each field is parametric")
@traitdef MyParamTr1{X{Y}, Z{Y}} begin
    @constraints begin
        Y <: Number
    end
    ftest1( X{Y}, Z{Y} ) -> X{Z{Y}}
end

println( " ... test its implementation" )
@traitimpl MyParamTr1{ Array{T...}, Nullable{T} } begin
    ftest1{T}( a::Array{T,1}, x::Nullable{T} ) = isnull(x) ?
            Nullable{T}[] : Nullable{T}[ Nullable(_*x.value) for _ in a ]
end

println( " ... check Array x Nullable" )
@assert istrait( MyParamTr1{ Array{Int,1}, Nullable{Int} }, verbose=true )
@assert ! istrait( MyParamTr1{ Array{Int,2}, Nullable{Int} } )
@assert ! istrait( MyParamTr1{ Array{Int,1}, Nullable{Float64} } )
println( " ... check dispatch")
@assert isequal( ftest1( [1,2,3], Nullable(2) ),[Nullable(2),Nullable(4),Nullable(6)] )
