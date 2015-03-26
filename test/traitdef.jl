## parsing
td = :(@traitdef Cr20{X} begin
    length(X)
end)
a,b = Traits.parsebody(td.args[end])
@test a==Expr(:dict, :(length=>((X,),Any)))
@test b==:(Bool[])

td0 = :(@traitdef Cr20{X} begin
    length(X)
    
    @constraints begin
        string(X.name)[1]=='I'
    end
end)
a,b = Traits.parsebody(td0.args[end])
@test a==Expr(:dict, :(length=>((X,),Any)))
@test b==:(Bool[(string(X.name))[1] == 'I'])

td1 = :(@traitdef Cr20{X} begin
    length(X) -> Int
    
    @constraints begin
        string(X.name)[1]=='I'
    end
end)
a,b = Traits.parsebody(td1.args[end])
@test a==Expr(:dict, :(length=>((X,),Int)))
@test b==:(Bool[(string(X.name))[1] == 'I'])

td2 = :(@traitdef Cr20{X,Y} begin
    X + Y -> Int,Float64
    -(X,Y) -> Int
    (/)(X,Y) -> Int
    
    @constraints begin
        string(X.name)[1]=='I'
    end
end)
a,b,c = Traits.parsebody(td2.args[end])
@test a==Expr(:dict, :((+) => ((X,Y),(Int,Float64))),
                     :((-) => ((X,Y),Int)),
              :((/) => ((X,Y),Int)))
@test b==:(Bool[(string(X.name))[1] == 'I'])
@test c.head==:block

td3 = :(@traitdef Cr20{X,Y} begin
    fn(X) -> Type{X}
end)
a,b,c = Traits.parsebody(td3.args[end])
@test a==Expr(:dict, :((fn) => ((X,),Type{X})))

td4 = :(@traitdef Cr20{X} begin
    fn{Y<:II}(X,Y) -> Type{X}
    fn76{K<:FloatingPoint, I<:Integer}(X, Vector{I}, Vector{K}) -> I
end)
a,b,c = Traits.parsebody(td4.args[end])
v = :(TypeVar(symbol("Y"),II))
t = :(TypeVar(symbol("I"),Integer))
k = :(TypeVar(symbol("K"),FloatingPoint))

@test a==Expr(:dict, :(fn=>((X,$v),Type{X})),
                     :(fn76=>((X,Vector{$t},Vector{$k}),$t))
              )


## test making traits

@traitdef MyIter{X}  begin
    start(X)
end

## Testing trait definitions
@test istrait(Cmp{Int,Int})
@test istrait(Cmp{Int,Float64})
@test !istrait(Cmp{Int,String})


coll = [Vector{Int}, Dict{Int,Int}, Set{Int}]
iter = [Traits.GenerateTypeVars{:upcase},  Int] #todo: add String,
if method_exists_bug
    assoc = [] #todo add again: Dict{Int,Int}] # , ObjectIdDict]
else
    index = [Array{Int,2}, Dict{Int,Int}, StepRange{Int,Int}]
end
index = [Array{Int,2}, StepRange{Int,Int}]

for c in coll
    @test istrait(Collection{c}, verbose=true)
    @test istrait(Iter{c}, verbose=true)
    @test istrait(IterColl{c}, verbose=true)
end
println("""After fix of https://github.com/JuliaLang/julia/issues/9135
        uncomment following line again and in commontraits.jl""")
# @test !istrait(Indexable{Set})

for c in iter
    @test istrait(Iter{c}, verbose=true)
end

for c in assoc
    @test istrait(Assoc{c}, verbose=true)
end

for c in index
    @test istrait(Indexable{c}, verbose=true)
end

@test istrait(Iter{Array}, verbose=true)
@test istrait(Iter{ASCIIString}, verbose=true)
@test istrait(Iter{Int}, verbose=true)
@test !istrait(Iter{Nothing})

arith = [Int, Float64, Rational{Int}]
for a1 in arith
    for a2 in arith
        @test istrait(Arith{a1,a2}, verbose=true)
    end
end

## test trait definition

@traitdef Tr20{X} begin
    length(X) -> Bool
end
@traitdef Tr21{X} <: Tr20{X} begin
    size(X) -> Bool
end
@traitdef Tr211{X} <: Tr21{X} begin
    size(X) -> Bool
end
@traitdef Tr2111{X} <: Tr211{X} begin
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
@test issubtrait(Tr211, Tr20)
@test issubtrait(Tr2111, Tr20)
@test issubtrait(Tr13, Tr11)
@test issubtrait(Tr13, Tr10)
@test issubtrait(Tr13, Tr21)
@test issubtrait(Tr13, Tr20)

@test issubtrait((Tr21,), (Tr20,))
@test issubtrait((Tr21,Tr11), (Tr20,Tr10))
@test issubtrait((Tr11,Tr21), (Tr10,Tr20))
@test !issubtrait((Tr21,Tr11), (Tr10,Tr20)) # todo: this should be true, I think

@test !issubtrait(Tr21{Int}, Tr20{Float64})
@test !issubtrait((Tr21{Int},), (Tr20{Float64},))

#--> need to be able to do this in terms of type variables.

# test functions parameterized on non-trait parameters

@traitdef Pr0{X} begin
    fn75{Y <: Integer}(X, Y) -> Y
end
fn75{Y <: Integer}(x::UInt8, y::Y) = y+x
if method_exists_bug2
    @test !istrait(Pr0{UInt8})
else
    @test istrait(Pr0{UInt8})
end
@test !istrait(Pr0{Int8})

fn75(x::UInt8, y::Int8) = y+x
@test !istrait(Pr0{UInt8})  # this works, not because only for y::Int8 not for all Integers

@traitdef Pr1{X}  begin
    fn76{I<:Integer}(X, Vector{I}) -> I
end
fn76{I<:Integer}(x::Uint8, v::Vector{I}) = v[x]
if method_exists_bug2
    @test !istrait(Pr1{UInt8})
else
    @test istrait(Pr1{UInt8})
end
@test !istrait(Pr1{UInt8})

# test constraints

@traitdef Cr20{X} begin
    length(X) -> Any
    
    @constraints begin
        string(X.name)[1]=='I'
    end
end

@test Cr20{Int}().methods==Dict(length => ((Int,),Any))

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


######
# istrait
#####
f12(x::Int) = 1
@traitdef UU{X} begin
    f12(X)
end

@test !istrait(UU{Any})     # ==false: this should be false
@test !istrait(UU{Integer}) # ==false: this should be false
@test !istrait(UU{Int8})    # ==false: this should be false

f13(x::Integer) = 1
@traitdef UU13{X} begin
    f13(X)
end

@test !istrait(UU13{Any})
@test istrait(UU13{Integer})
@test istrait(UU13{Int8})

#####
# Associated types
####
@traitdef Iter2{X} begin
    # type-functions based on return_type:
    State = Base.return_types(start, (X,))[1]  # this is circular but that is ok, as trait needs to be implemented.
    Item =  Base.return_types(next, (X,State))[1][1]
    
    # interface functions
    start(X) -> State
    next(X, State) -> Item, State
    done(X, State) -> Bool
    # automatically provides:
    # zip, enumerated, in, map, reduce, ...
end
@test istrait(Iter2{Int})
@test istrait(Iter2{Array})

####
# DataType constructors
####

@traitdef TT45{D} begin
    # This trait contains all datatypes which have a constructor with
    # no arguments.
    D() -> D
end
type A4758 end

@test istrait(TT45{A4758})
@test istrait(TT45{Dict{Int,Int}})
@test istrait(TT45{Set{Int}})
@test !istrait(TT45{Int})
@test !istrait(TT45{Array{Int,1}})

# This is the trait for datatypes with Array like constructors:
@traitdef TT46{Ar} begin
    T = Type{eltype(Ar)}
    Arnp = deparameterize_type(Ar)  # Array stripped of type parameters
    
    #Arnp(T, Int64) -> Ar
    Arnp(T, Int...) -> Ar # see issue #8 & https://github.com/JuliaLang/julia/issues/10642
    @constraints begin
        length(Ar.parameters)>1 # need at least two parameters to be array-like, right?
    end
end
@test !istrait(TT46{A4758})
if Traits.flag_check_return_types
    @test !istrait(TT46{Dict{Int,Int}})
else
    @test istrait(TT46{Dict{Int,Int}}, verbose=true) # this is a false positive
end
# @test istrait(TT46{Set{Int}}, verbose=true) this actually works, but not as expected and gives a deprecation warning
@test !istrait(TT46{Int})
@test istrait(TT46{Array{Int,1}}, verbose=true)
# @test istrait(TT46{Array{Int}}, verbose=true) # this does not pass currently because of https://github.com/JuliaLang/julia/issues/10642
@test istrait(TT46{Array}, verbose=true)
