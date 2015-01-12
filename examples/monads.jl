using Traits

immutable FuncFullSig{TI, TO}
    f::Function
end

call{TI,TO}( fs::FuncFullSig{TI,TO}, x::TI ) = (fs.f(x))::TO

# I use the prefix "Semi" to remind ourselves that the
# there may not be a guarantee of function output type
@traitdef SemiFunctor{X{Y}} begin
    fmap( Function, X{Y} ) -> Any
end

@traitdef Functor{X{Y}} begin
    fmap( FuncFullSig{Y,Z}, X{Y} ) -> X{Z}
end

@traitdef SemiMonad{X{Y}} begin
    mreturn( ::X,Y) ->X{Y}
    bind( X{Y}, Function ) -> Any
end

# ::X is the shorthand for singleton type argument
@traitdef Monad{X{Y}} begin
    mreturn(::X, Y) -> X{Y} # we cannot infer X so we have to supply it
    bind( X{Y}, FuncFullSig{Y, X{Y}} ) -> X{Y}
end

# === implementation of traits
@traitimpl SemiMonad{ Nullable{Y} } begin
    mreturn{Y}( ::Type{Nullable}, x::Y ) = Nullable{Y}(x)
    bind{Y}( x::Nullable{Y}, f::Function ) = begin
        if isnull(x)
            return Nullable()
        else
            try
                return f( x.value )
            catch
                return Nullable()
            end
        end
    end
end

@traitimpl SemiMonad{ Array{Y...} } begin
    mreturn{Y}( ::Type{Array}, x::Y ) = Y[x]
    bind{Y}( x::Array{Y,1}, f::Function ) = [ f(_) for _ in x ]
end

# === some combo traits ======
@traitdef MonadRelated1{ X{Y}, Z } <: SemiMonad{X} begin
    @constraints begin
        Y == Z
    end
end

@traitimpl SemiFunctor{ Array{Y...} } begin
    fmap{Y}( f::Function, x::Array{Y,1} ) = map(f, x)
end

# the deparameterize_type is ugly, but at least we don't have
# to do it many times
@traitfn mequal{M,Y;MonadRelated1{M,Y}}( x::M, y::Y ) =
    isequal( x, mreturn( Traits.deparameterize_type(M), y ) )

@traitfn mequal{Y,M;MonadRelated1{M,Y}}( x::Y, y::M ) =
    isequal( y, mreturn( Traits.deparameterize_type(M), x ) )

mequal( x, y ) = isequal( x, y )

@assert mequal( Nullable( 1.0 ), 1.0 )
@assert mequal( 1.0, Nullable( 1.0 ) )

# but it's better than that. Since we have Array being a SemiMonad,
# we get this for free
@assert mequal( 1.0, [ 1.0 ] )
@assert mequal( [1.0], 1.0 )

# now we compare an Array of nullables and a simple Array
@traitdef CollectionM{X{Y}} <: Collection{X} begin
    @constraints begin
        istrait( SemiMonad{Y} ) # we cannot put it in the header
    end
end
@traitdef IterM{X{Y}} <: Iter{X} begin
    @constraints begin
        istrait( SemiMonad{Y} ) # we cannot put it in the header
    end
end

@traitfn mequal(X,Y;CollectionM{X}, Collection{Y})( xc::X, yc::Y ) = begin
    println( "using CollectionM comparison")
    if length(xc) != length(yc)
        return false
    end
    xs = start(xc)
    ys = start(yc)
    while( !done(xc, xs) )
        x,xs = next(xc,xs)
        y,ys = next(yc,ys)
        if !mequal( x, y )
            return false
        end
    end
    return true
end
@traitfn mequal(X,Y;IterM{X}, Iter{Y})( xc::X, yc::Y ) = begin
    println( "using IterM comparison")
    xs = start(xc)
    ys = start(yc)
    while( !done(xc, xs) && !done( yc, ys) )
        x,xs = next(xc,xs)
        y,ys = next(yc,ys)
        if !mequal( x, y )
            return false
        end
    end
    if !done(xc,xs) || !done(yc,ys)
        return false
    end
    return true
end

@assert mequal( [ Nullable(1.0), Nullable(2.0) ], [1.0, 2.0 ] )
