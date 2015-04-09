## Testing hand-coded trait definitions
#######################################

# All type belong to the empty trait, as it makes no restriction on
# the types:
@test istrait( () )

immutable Tr1{X1} <: Traits.Trait{()}
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    Tr1() = new(Traits.FDict(), Bool[], [])
end
immutable Tr2{X1,X2} <: Traits.Trait{()}
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    Tr2() = new(Traits.FDict(), Bool[], [])
end
immutable Tr3{X1,X2} <: Traits.Trait{(Tr1{X1}, Tr2{X1,X2})}
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    Tr3() = new(Traits.FDict(), Bool[], [])
end

@test istraittype(Tr1)
@test istraittype(Tr1{A1})
@test istraittype( (Tr1{A1},Tr2{A1,A2}) )

@test traitgetpara(Tr1{A1})==(A1,)
@test traitgetsuper(Tr1{A1})==()
@test traitgetsuper(Tr2{A1,A2})==()
@test traitgetsuper(Tr3{A1,A2})==(Tr1{A1},Tr2{A1,A2})

# any type is part of a unconstrained trait:
@test istrait(Tr1{Int}, verbose=true)
@test istrait(Tr2{DataType,Int})
@test istrait(Tr3{String,DataType})
@test_throws TraitException istrait(Tr3{:a,7})  # maybe this should error?

immutable D1{X1} <: Traits.Trait{()}
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    function D1()
        new(Traits.FDict(
             sin => _sin(::X1) = Float64(), # note 1: _sin could be any symbol;
                                            # note 2: Float64() would throw an error but works with return_types
             cos => _cos(::X1) = Float64()
             ),
            Bool[],
            []
            )
    end
end

@test istrait(D1{Int}, verbose=true)
@test !istrait(D1{String})

immutable D2{X1,X2} <: Traits.Trait{(D1{X1}, D1{X2})}
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    function D2()
        new(Traits.FDict(
             (+) => _plus(::X1, ::X2) = Any(),
             (-) => _minus(::X1, ::X2) = Any()
             ),
            Bool[],
            []
            )
    end
end

@test istrait(D2{Int, Int})
@test !istrait(D2{Int, String})

immutable D3{X1} <: Traits.Trait{()}
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    function D3()
        new(Traits.FDict(
             getkey => _getkey(::X1,::Any,::Any) = Any(),
             get!   => _get!(::X1, ::Any, ::Any) = Any()
             ),
            Bool[],
            []
            )
    end
end

immutable D4{X1,X2} <: Traits.Trait{()} # like D2 but without supertraits
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    function D4()
        new(Traits.FDict(
             (+) => _plus(::X1, ::X2) = Any(),
             (-) => _minus(::X1, ::X2) = Any()
             ),
            Bool[],
            []
            )
    end
end

@test istrait(D3{Dict{Int,Int}})
@test !istrait(D3{Int})

@test istrait((D1{Int}, D2{Int, Int}) )

@test istrait(D4{Int,FloatingPoint})

### adding other constraints

immutable CTr1{X1,X2} <: Traits.Trait{()}
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    function CTr1()
        new(Traits.FDict(
             (+) => _plus(::X1, ::X2) = Any(),
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

immutable CTrAs{X1,X2} <: Traits.Trait{()}
    methods::Traits.FDict
    constraints::Vector{Bool}
    assoctyps::Vector{Any}
    function CTrAs()
        R = promote_type(X1, X2)
        D = (X1,X2)<:(Integer,Integer) ? Float64 : promote_type(X1, X2)
        assoctyps = Any[TypeVar(:R, R), TypeVar(:D, D)]
        new(Traits.FDict(
                  (+) => _plus(::X1, ::X2) = Any(),
                  (-) => _minus(::X1, ::X2) = Any()
             ),
            Bool[],
            assoctyps
            )
    end
end

@test istrait(CTrAs{Int32, Int})
# @test istrait(CTrAs{Integer, Integer}) # doesn't work because return type of /(Integer, Integer)==Any
@test istrait(CTrAs{Int, Int})
@test !istrait(CTrAs{Int, String})
