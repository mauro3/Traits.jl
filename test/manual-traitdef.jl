## Testing hand-coded trait definitions
#######################################

# All type belong to the empty trait, as it makes no restriction on
# the types:
@test istrait( Tuple{} )

immutable Tr1{X1} <: Traits.Trait{Tuple{}}
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    Tr1() = new(Traits.FDict(), Bool[], [])
end
immutable Tr2{X1,X2} <: Traits.Trait{Tuple{}}
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    Tr2() = new(Traits.FDict(), Bool[], [])
end
immutable Tr3{X1,X2} <: Traits.Trait{Tuple{Tr1{X1}, Tr2{X1,X2}}}
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    Tr3() = new(Traits.FDict(), Bool[], [])
end

@test istraittype(Tr1)
@test istraittype(Tr1{A1})
@test istraittype( Tuple{Tr1{A1},Tr2{A1,A2}} )

@test traitgetpara(Tr1{A1})==Tuple{A1}
@test traitgetsuper(Tr1{A1})==Tuple{}
@test traitgetsuper(Tr2{A1,A2})==Tuple{}
@test traitgetsuper(Tr3{A1,A2})==Tuple{Tr1{A1},Tr2{A1,A2}}

# any type is part of a unconstrained trait:
@test istrait(Tr1{Int}, verbose=verbose)
@test istrait(Tr2{DataType,Int})
@test istrait(Tr3{AbstractString,DataType})
@test_throws TraitException istrait(Tr3{:a,7})  # maybe this should error?

immutable D1{X1} <: Traits.Trait{Tuple{}}
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    function D1()
        new(Traits.FDict(
             sin => (_sin( ::Float64, ::X1) = nothing),
             cos => (_cos( ::Float64, ::X1) = nothing)
             ),
            Bool[],
            []
            )
    end
end

@test istrait(D1{Int}, verbose=verbose)
@test !istrait(D1{AbstractString})

immutable D2{X1,X2} <: Traits.Trait{Tuple{D1{X1}, D1{X2}}}
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    function D2()
        new(Traits.FDict(
             (+) => (_plus( ::Any, ::X1, ::X2) = nothing),
             (-) => (_minus( ::Any, ::X1, ::X2) = nothing)
             ),
            Bool[],
            []
            )
    end
end

@test istrait(D2{Int, Int})
@test !istrait(D2{Int, AbstractString})

immutable D3{X1} <: Traits.Trait{Tuple{}}
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    function D3()
        new(Traits.FDict(
             getkey => (_getkey( ::Any, ::X1,::Any, ::Any) = nothing),
             get!   => (_get!( ::Any, ::X1, ::Any, ::Any) = nothing)
             ),
            Bool[],
            []
            )
    end
end

immutable D4{X1,X2} <: Traits.Trait{Tuple{}} # like D2 but without supertraits
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    function D4()
        new(Traits.FDict(
             (+) => (_plus( ::Any, ::X1, ::X2) = nothing),
             (-) => (_minus( ::Any, ::X1, ::X2) = nothing)
             ),
            Bool[],
            []
            )
    end
end

@test istrait(D3{Dict{Int,Int}})
@test !istrait(D3{Int})

@test istrait(Tuple{D1{Int}, D2{Int, Int}} )

@test istrait(D4{Int,AbstractFloat})

### adding other constraints

immutable CTr1{X1,X2} <: Traits.Trait{Tuple{}}
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    function CTr1()
        new(Traits.FDict(
             (+) => (_plus( ::Any, ::X1, ::X2) = nothing),
             ),
            Bool[
                 X1==X2
                 ],
            []
            )
    end
end

@test !istrait(CTr1{Int32, Int})
@test istrait(CTr1{Int, Int})

### adding other associated types

immutable CTrAs{X1,X2} <: Traits.Trait{Tuple{}}
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    function CTrAs()
        R = promote_type(X1, X2)
        D = Tuple{X1,X2}<:Tuple{Integer,Integer} ? Float64 : promote_type(X1, X2)
        assoctyps = Any[TypeVar(:R, R), TypeVar(:D, D)]
        new(Traits.FDict(
                  (+) => (_plus( ::Any, ::X1, ::X2) = nothing),
                  (-) => (_minus( ::Any, ::X1, ::X2) = nothing)
             ),
            Bool[],
            assoctyps
            )
    end
end

@test istrait(CTrAs{Int32, Int})
# @test istrait(CTrAs{Integer, Integer}) # doesn't work because return type of /(Integer, Integer)==Any
@test istrait(CTrAs{Int, Int})
@test !istrait(CTrAs{Int, AbstractString})

# parametric methods
####################

# @traitdef Tr01{X} begin
#     g01{T<:X}(T, T) -> T
# end
immutable Tr01{X} <: Traits.Trait{Tuple{}}
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    function Tr01()
        new(Traits.FDict(
                         g01 => (_g01{T<:X}( ::Type{T}, ::T, ::T) = nothing)
                         ),
            Bool[],
            []
            )
    end
end


g01(::Int, ::Int) = Int
@test istrait(Tr01{Int}) # == true as constraints Int isleaftype
@test !istrait(Tr01{Integer})
g01{I<:Integer}(::I, ::I) = I
@test istrait(Tr01{Integer}) # == true

# @traitdef Tr02{X} begin
#     g02{T<:X}(T, T) -> T
# end
immutable Tr02{X} <: Traits.Trait{Tuple{}}
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    function Tr02()
        new(Traits.FDict(
                         g02 => (_g02{T<:X}( ::Type{T}, ::T, ::T) = nothing)
                         ),
            Bool[],
            []
            )
    end
end

g02{I<:Integer}(::I, ::I) = Integer
# By using Base.return_types it is not possible to figure out whether
# the returned value is constrained or not by I:
if function_types_bug1
    @test istrait(Tr02{Integer})
    # or throw an error/warning here saying parametric return types
    # are only supported for leaftypes
else
    @test !istrait(Tr02{Integer}) # if function types get implemented this should be possible to catch
end
@test !istrait(Tr02{Int})
g02{I<:Integer}(::I, ::I) = I
@test istrait(Tr02{Int})


# @traitdef Tr03{X} begin
#     g03{T<:X}(T, Vector{T})
# end
immutable Tr03{X} <: Traits.Trait{Tuple{}}
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    function Tr03()
        new(Traits.FDict(
                         g03 => (_g03{T<:X}( ::T, ::T, ::Vector{T}) = nothing)
                         ),
            Bool[],
            []
            )
    end
end

g03{I<:Integer}(::I, ::Vector{I}) = 1
@test istrait(Tr03{Integer})
@test istrait(Tr03{Int})


# ## If I ever need to get to the trait parameters, something like this should work:
# # @traitdef Tr04{X} begin
# #     g04{T<:X}(T, Vector{T})
# # end
# immutable Tr04{X} <: Traits.Trait{Tuple{}}
#     methods::Traits.FDict
#     constraints::Vector{Bool}
#     assoctyps::Vector{Any}
#     function Tr04()
#         A1 = getassoc(X)
#         new(Traits.FDict(
#                          g04 => (_g04{T<:X}( ::T, ::T, ::Vector{T}, ::A1) = nothing)
#                          ),
#             Bool[],
#             [A1]
#             )
#     end
#     function Tr04(::Type{Traits._TestTraitPara})
#         A1 = Traits._TestAssoc{:A1}
#         new(Traits.FDict(
#                          g04 => (_g04{T<:X}( ::T, ::T, ::Vector{T}, ::A1) = nothing)
#                          ),
#             Bool[],
#             [A1]
#             )
#     end
# end
# getassoc{T<:Integer}(::Type{T}) = UInt
# g04{I<:Integer}(::I, ::Vector{I}, ::Integer) = 1
