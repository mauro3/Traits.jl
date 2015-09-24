###############
# recreate manual-traitdispatch.jl: f2

@traitfn ttf2{X,Y<:Integer; D1{Y}, D4{X,Y}}(x::X,y::Y) = x + sin(y)
@traitfn ttf2{S,T<:Integer; D1{S}, D1{T}  }(s::S,t::T) = sin(s) - sin(t)
@traitfn ttf2{X,Y<:AbstractFloat; D1{X}, D1{Y}  }(x::X,y::Y) = cos(x) - cos(y)

@test ttf2(4,5)==(4 + sin(5))
@test ttf2(4,5.) == cos(4)-cos(5.)

@test ttf2(MTT1(5),4) == sin(5)-sin(4)
@test ttf2(MTT2(5),3) == sin(3)+5

@test length(methods(f2))==length(methods(ttf2))
