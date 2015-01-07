using Traits

immutable FuncFullSig{TI, TO}
    f::Function
end

call{TI,TO}( fs::FuncFullSig{TI,TO}, x::TI ) = (fs.f(x))::TO

@traitdef Monad{X{Y}} begin
    mreturn(::X, Y) -> X{Y} # we cannot infer X so we have to supply it
    bind( X{Y}, FuncFullSig{Y, X{Y}} ) -> X{Y}
end

@traitimpl Monad{Nullable{Y}} begin
    mreturn{Y}( ::Type{Nullable}, x::Y ) = Nullable{Y}(x)
    bind{Y,Z}( x::Nullable{Y}, f::FuncFullSig{Y, Nullable{Z}} ) = begin
        if isnull(x)
            return Nullable{Z}()
        else
            try
                return f.f( x.value )::Nullable{Z}
            catch
                return Nullable{Z}()
            end
        end
    end
end

@assert isequal( mreturn( Nullable, 1.0 ), Nullable{Float64}( 1.0 ) )
testfunc( x::Float64 ) = x == 0.0 ? Nullable{Float64}() : Nullable{Float64}( 1.0/x )
@assert isequal( bind( Nullable{Float64}( 2.0 ), FuncFullSig{Float64,Nullable{Float64}}( testfunc ) ), Nullable{Float64}( 0.5 ) )
@assert isequal( bind( Nullable{Float64}( 0.0 ), FuncFullSig{Float64,Nullable{Float64}}( testfunc ) ), Nullable{Float64}() )

# Note: FuncFullSig{TO} is itself a Monad
#=
@traitfn mreturn{;Monad{FuncFullSig{T}}}( x::T ) = FuncFullSig{T,T}(_->x)
@traitfn mreturn{;Monad{Array{T}}}( a::T ) = T[ a ]
#@traitfn mreturn{;Monad{Nullable{T}}}( a::T ) = Nullable{T}(a)

@traitfn function bind{;Monad{Array{T}}}( x::Array{T}, f::FuncFullSig{T, Array{T} } )
    T[ map( f.f, x )... ]
end
@traitfn function bind{;Monad{FuncFullSig{T}}}( x::FuncFullSig{T}, f::FuncFullSig{T, FuncFullSig{T} })
end
=#
