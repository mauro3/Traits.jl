## Testing hand-coded trait definitions
#######################################

immutable Tr1{X1} <: Traits.Trait{()}
    fns
    Tr1() = new(Dict())
end
immutable Tr2{X1,X2} <: Traits.Trait{()}
    fns
    Tr2() = new(Dict())
end
immutable Tr3{X1,X2} <: Traits.Trait{(Tr1{X1}, Tr2{X1,X2})}
    fns
    Tr3() = new(Dict())
end

@test istrait(Tr1)
@test istrait(Tr1{A1})
@test istrait( (Tr1{A1},Tr2{A1,A2}) )

@test traitgetpara(Tr1{A1})==(A1,)
@test traitgetsuper(Tr1{A1})==()
@test traitgetsuper(Tr2{A1,A2})==()
@test traitgetsuper(Tr3{A1,A2})==(Tr1{A1},Tr2{A1,A2})

# any type is part of a unconstrained trait:
@test traitcheck(Tr1{Int}) 
@test traitcheck(Tr2{DataType,Int})
@test traitcheck(Tr3{String,DataType})
@test traitcheck(Tr3{:a,7})  # maybe this should error?

immutable D1{X1} <: Traits.Trait{()}
    fns
    function D1() 
        new([
             sin => ((X1,), (Float64,)), 
             cos => ((X1,), (Float64,)), 
             ])
    end
end

@test traitcheck(D1{Int})
@test !traitcheck(D1{String})

immutable D2{X1,X2} <: Traits.Trait{(D1{X1}, D1{X2})}
    fns
    function D2() 
        new([
             (+) => ((X1, X2), (Any,)),
             (-) => ((X1, X2), (Any,))
             ])
    end
end

@test traitcheck(D2{Int, Int})
@test !traitcheck(D2{Int, String})

immutable D3{X1} <: Traits.Trait{()}
    fns
    function D3() 
        new([
             getkey => ((X1,Any,Any), (Any,)),
             get!   => ((X1, Any, Any), (Any,))
             ])
    end
end

immutable D4{X1,X2} <: Traits.Trait{()} # like D2 but without supertraits
    fns
    function D4() 
        new([
             (+) => ((X1, X2), (Any,)),
             (-) => ((X1, X2), (Any,))
             ])
    end
end


@test traitcheck(D3{Dict})
@test !traitcheck(D3{Int})

@test traitcheck((D1{Int}, D2{Int, Int}) )
@test !traitcheck((D1{Int}, D2{Int, Int}, Traits.NoTrait) )

# All type belong to the empty trait, as it makes no restriction on
# the types.
@test traitcheck( () )  
