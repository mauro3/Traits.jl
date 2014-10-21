## test making traits
@traitdef MyIter{X}  begin
    start(X)
end


## Testing trait definitions
@test traitcheck(Cmp{Int,Int})
@test traitcheck(Cmp{Int,Float64})
@test !traitcheck(Cmp{Int,String})


coll = [Array, Dict, Set]
iter = [Traits.GenerateTypeVars, String, Int]
assoc = [Dict] # , ObjectIdDict]
index = [Array, Dict, Range]

for c in coll
    @test traitcheck(Collection{c})
    @test traitcheck(Iter{c})
    @test traitcheck(IterColl{c})
end
@test !traitcheck(Indexable{Set})

for c in iter
    @test traitcheck(Iter{c})
end

for c in assoc
    @test traitcheck(Assoc{c})
end

for c in index
    @test traitcheck(Indexable{c})
end

@test traitcheck(Iter{Array})
@test traitcheck(Iter{String})
@test traitcheck(Iter{Int})
@test !traitcheck(Iter{Nothing})

arith = [Int, Float64, Rational]
for a1 in arith
    for a2 in arith
        @test traitcheck(Arith{a1,a2})
    end
end

## test trait definition

@traitdef Tr20{X} begin
    length(X) -> Bool
end
@traitdef Tr21{X} <: Tr20{X} begin
    size(X) -> Bool
end
@traitdef Tr10{X,Y}  begin
    isless(X,Y) -> Bool
end
@traitdef Tr11{X,Y}  <: Tr10{X,Y} begin
   ==(X,Y) -> Bool
end

@traitdef Tr13{X,Y}  <: Tr11{X,Y}, Tr20{X}, Tr21{Y} begin
   ==(X,Y) -> Bool
end

@test traitgetsuper(Tr20)==()
@test traitgetsuper(Tr21)==(Tr20,)
@test traitgetsuper(Tr13)==(Tr11, Tr20, Tr21)

@test issubtrait(Tr21, Tr20)
@test issubtrait(Tr13, Tr11)
@test issubtrait(Tr13, Tr10)
@test issubtrait(Tr13, Tr21)
@test issubtrait(Tr13, Tr20)
