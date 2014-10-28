## parsing
td = :(@traitdef Cr20{X} begin
    length(X)
end)
a,b = Traits.parsebody(td.args[end])
@test a==Expr(:dict, :(length=>((X,),(Any...,))))
@test b==:(Bool[])

td0 = :(@traitdef Cr20{X} begin
    length(X)
    
    @constraints begin
        string(X.name)[1]=='I'
    end
end)
a,b = Traits.parsebody(td0.args[end])
@test a==Expr(:dict, :(length=>((X,),(Any...,))))
@test b==:(Bool[(string(X.name))[1] == 'I'])

td1 = :(@traitdef Cr20{X} begin
    length(X) -> Int
    
    @constraints begin
        string(X.name)[1]=='I'
    end
end)
a,b = Traits.parsebody(td1.args[end])
@test a==Expr(:dict, :(length=>((X,),(Int,))))
@test b==:(Bool[(string(X.name))[1] == 'I'])

td2 = :(@traitdef Cr20{X} begin
    length(X) -> Int,Float64
    
    @constraints begin
        string(X.name)[1]=='I'
    end
end)
a,b = Traits.parsebody(td2.args[end])
@test a==Expr(:dict, :(length=>((X,),(Int,Float64))))
@test b==:(Bool[(string(X.name))[1] == 'I'])


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
index = [Array, Dict, StepRange{Int,Int}]

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

# test constraints

@traitdef Cr20{X} begin
    length(X) -> Any
    
    @constraints begin
        string(X.name)[1]=='I'
    end
end

@test Cr20{Int}().methods==[length => ((Int,),(Any,))]

@test !istrait(Cr20{Float32})
@test istrait(Cr20{Int})

@traitdef Cr21{X,Y} begin
    ==(X,Y)
    @constraints begin
        string(X.name)[1]=='I'
    end
    # probably not recommended but there can be several constraint
    # blocks:
    @constraints begin
        X==Y
    end
end

@test !istrait(Cr21{Float32, Float32})
@test istrait(Cr21{Int, Int})

@traitdef Cr22{X,Y} begin
    @constraints begin
        X==Y
    end
end
@test istrait(Cr22{Float32, Float32})
@test istrait(Cr22{Int, Int})
@test !istrait(Cr22{Int, Float32})
