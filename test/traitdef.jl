## test making traits
@traitdef MyIter{X}  begin
    start(X)
end


## Testing trait definitions
@test istrait(Cmp{Int,Int})
@test istrait(Cmp{Int,Float64})
@test !istrait(Cmp{Int,String})


coll = [Array, Dict, Set]
iter = [Traits.GenerateTypeVars, String, Int]
assoc = [Dict] # , ObjectIdDict]
index = [Array, Dict, Range]

for c in coll
    @test istrait(Collection{c})
    @test istrait(Iter{c})
    @test istrait(IterColl{c})
end
@test !istrait(Indexable{Set})

for c in iter
    @test istrait(Iter{c})
end

for c in assoc
    @test istrait(Assoc{c})
end

for c in index
    @test istrait(Indexable{c})
end

@test istrait(Iter{Array})
@test istrait(Iter{String})
@test istrait(Iter{Int})
@test !istrait(Iter{Nothing})

arith = [Int, Float64, Rational]
for a1 in arith
    for a2 in arith
        @test istrait(Arith{a1,a2})
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
