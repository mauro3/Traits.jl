type Trait1 end
type Trait2 end
type Trait3 end
# now define function f which should dispatch on those traits
ftim(x,y) = _ftim(x,y, checkfn(x,y))
_ftim(x,y,::Type{Trait1}) = x+y
_ftim(x,y,::Type{Trait2}) = x-y
_ftim(x,y,::Type{Trait3}) = x*y
# default
checkfn{T,S}(x::T,y::S) = error("Function ftim not implemented for type ($T,$S)")
# associate types-tuples to Trait1, Trait2 or Trait3:
checkfn(::Int, ::Int) = Trait1
checkfn(::Int, ::FloatingPoint) = Trait2
checkfn(::FloatingPoint, ::FloatingPoint) = Trait3
# use
@assert ftim(3,4)==7      # Trait1
@assert ftim(3,4.)==-1.0  # Trait2
@assert ftim(3.,4.)==12.0 # Trait3
# Add another type-tuple to Trait3
checkfn(::String, ::String) = Trait3
@assert ftim("Lorem ", "Ipsum")=="Lorem Ipsum"
