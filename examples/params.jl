using Traits

@traitdef Functor{X{Y}} begin
    fmap(Function,X) -> X
end

@traitimpl Functor{ Array{T,N} -> T } begin
    fmap{T}( f::Function, x::Array{T,1} ) = map( f, x )
end
