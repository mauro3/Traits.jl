using Traits
using Base.Test

import Traits: tparusesuffix
@traitdef SemiFunctor{X{Y}} begin
    fmap( Function, X{Y} ) -> Any
end

fmap{Y}( f::Function, x::Nullable{Y} ) = Nullable( f(x.value) )

println( "test SemiFunctor{Nullable}" )
@test istrait( SemiFunctor{Nullable{Int}})

# This change the order
@traitimpl SemiFunctor{ Array{Y...} } begin
    fmap{Y}( f::Function, x::Array{Y,1} ) = map(f, x)
end

@test Traits.tparsuffix( SemiFunctor, Val{1}, Array{Int,1} ) != ()

println( "test istrait{ SemiFunctor{Array{Int,1}}}" )
@test istrait( SemiFunctor{Array{Int,1}} )
println( "test istrait{ SemiFunctor{Array{Int,2}}}" )
@test istrait( SemiFunctor{Array{Int,2}} ) == false
