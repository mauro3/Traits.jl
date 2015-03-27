## Testing hand-coded trait definitions
#######################################

# All type belong to the empty trait, as it makes no restriction on
# the types:
@test istrait( () )


immutable Tr1{X1} <: Traits.Trait{()}
    methods
    constraints
    Tr1() = new(Dict(), [])
end
immutable Tr2{X1,X2} <: Traits.Trait{()}
    methods
    constraints
    Tr2() = new(Dict(), [])
end
immutable Tr3{X1,X2} <: Traits.Trait{(Tr1{X1}, Tr2{X1,X2})}
    methods
    constraints
    Tr3() = new(Dict(), [])
end

@test istraittype(Tr1)
@test istraittype(Tr1{A1})
@test istraittype( (Tr1{A1},Tr2{A1,A2}) )

@test traitgetpara(Tr1{A1})==(A1,)
@test traitgetsuper(Tr1{A1})==()
@test traitgetsuper(Tr2{A1,A2})==()
@test traitgetsuper(Tr3{A1,A2})==(Tr1{A1},Tr2{A1,A2})

# any type is part of a unconstrained trait:
@test istrait(Tr1{Int})
@test istrait(Tr2{DataType,Int})
@test istrait(Tr3{String,DataType})
@test_throws TraitException istrait(Tr3{:a,7})  # maybe this should error?

immutable D1{X1} <: Traits.Trait{()}
    methods
    constraints
    function D1()
        new(Dict(
             sin => ((X1,), Float64),
             cos => ((X1,), Float64),
             ),
            []
            )
    end
end

@test istrait(D1{Int})
@test !istrait(D1{String})

immutable D2{X1,X2} <: Traits.Trait{(D1{X1}, D1{X2})}
    methods
    constraints
    function D2()
        new(Dict(
             (+) => ((X1, X2), Any),
             (-) => ((X1, X2), Any)
             ),
            []
            )
    end
end

@test istrait(D2{Int, Int})
@test !istrait(D2{Int, String})

immutable D3{X1} <: Traits.Trait{()}
    methods
    constraints
    function D3()
        new(Dict(
             getkey => ((X1,Any,Any), Any),
             get!   => ((X1, Any, Any), Any)
             ),
            []
            )
    end
end

immutable D4{X1,X2} <: Traits.Trait{()} # like D2 but without supertraits
    methods
    constraints
    function D4()
        new(Dict(
             (+) => ((X1, X2), Any),
             (-) => ((X1, X2), Any)
             ),
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
    methods::Dict
    constraints::Array{Bool,1} # constraints are an array of functions
                            # which need to evaluate to true.  Their
                            # signature is f(X,Y) = ...

    function CTr1()
        new(Dict(
             (+) => ((X1, X2), Any),
             ),
            Bool[
                 X1==X2
                 ]
            )
    end
end

@test !istrait(CTr1{Int32, Int})
@test istrait(CTr1{Int, Int})

### adding other associated types

immutable CTrAs{X1,X2} <: Traits.Trait{()}
    methods::Dict
    constraints::Array{Bool,1} # constraints are an array of statements
                            # which need to evaluate to true.  Their
                            # signature is f(X,Y) = ...
    assoctyps::Array{TypeVar,1}
    function CTrAs()
        R = promote_type(X1, X2)
        D = (X1,X2)<:(Integer,Integer) ? Float64 : promote_type(X1, X2)
        assoctyps = [TypeVar(:R, R), TypeVar(:D, D)]
        new(Dict(
                 (+) => ((X1, X2), R),
                 (/) => ((X1, X2), D),
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
