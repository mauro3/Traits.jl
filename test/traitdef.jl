## parsing
td = :(@traitdef Cr20{X} begin
    length(X)
end)
a,b,c = Traits.parsebody(td.args[end])
# a is not hard to test because of the random gensym
@test a.head==:call
@test a.args[1]==:(Traits.FDict)
@test a.args[2].head==:(=>)
@test a.args[2].args[1] == :length
@test a.args[2].args[2].args[2] == :(Any())
@test b==:(Bool[])
@test c.args[1]==:(assoctyps = Any[])

td0 = :(@traitdef Cr20{X} begin
    length(X)
    
    @constraints begin
        string(X.name)[1]=='I'
    end
end)
a,b = Traits.parsebody(td0.args[end])
@test b==:(Bool[(string(X.name))[1] == 'I'])

td1 = :(@traitdef Cr20{X} begin
    length(X) -> Int
    
    @constraints begin
        string(X.name)[1]=='I'
    end
end)
a,b = Traits.parsebody(td1.args[end])
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
@test b==:(Bool[(string(X.name))[1] == 'I'])
@test c.head==:block

td3 = :(@traitdef Cr20{X,Y} begin
    fn(X) -> Type{X}
end)
a,b,c = Traits.parsebody(td3.args[end])

# td4 = :(@traitdef Cr20{X} begin
#     fn{Y<:II}(X,Y) -> Type{X}
#     fn76{K<:FloatingPoint, I<:Integer}(X, Vector{I}, Vector{K}) -> I
# end)
# a,b,c = Traits.parsebody(td4.args[end])
# v = :(TypeVar(symbol("Y"),II))
# t = :(TypeVar(symbol("I"),Integer))
# k = :(TypeVar(symbol("K"),FloatingPoint))

# @test a==Expr(:dict, :(fn=>((X,$v),Type{X})),
#                      :(fn76=>((X,Vector{$t},Vector{$k}),$t))
#               )


## test making traits

@traitdef MyIter{X}  begin
    start(X)
end

## Testing trait definitions in commontraits.jl
@test istrait(Cmp{Int,Int})
@test istrait(Cmp{Int,Float64})
@test !istrait(Cmp{Int,String})


coll = [Vector, Vector{Int}, Dict{Int}, Dict{Int,Int}, Set{Int}]
iter = [Traits.GenerateTypeVars{:upcase},  Int] #todo: add String,
if method_exists_bug1
    dicts = [] #todo add again: Dict{Int,Int}] # , ObjectIdDict]
else
    dicts = [Dict{Int}, Dict{Int,Int}] # Dict does not work, ObjectIdDict does not fulfill the trait
end
index = [Array{Int,2}, StepRange{Int,Int}]
c =1
for c in coll
    @test istrait(Collection{c}, verbose=verbose)
    @test istrait(Iter{c}, verbose=verbose)
    @test istrait(IterColl{c}, verbose=verbose)
end
@test !istrait(Indexable{Set})

for c in iter
    @test istrait(Iter{c}, verbose=verbose)
end

for c in dicts
    @test istrait(Assoc{c}, verbose=verbose)
end

for c in index
    @test istrait(Indexable{c}, verbose=verbose)
end

@test istrait(Iter{Array}, verbose=verbose)
@test istrait(Iter{ASCIIString}, verbose=verbose)
@test istrait(Iter{Int}, verbose=verbose)
@test !istrait(Iter{Nothing})

arith = [Int, Float64, Rational{Int}]
for a1 in arith
    for a2 in arith
        @test istrait(Arith{a1,a2}, verbose=verbose)
    end
end

## test trait definition
@traitdef FF{X} begin
    f948576()
end
@test !istrait(FF{Int})
f948576() = 1
@test istrait(FF{Int})

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
@test  issubtrait((Tr21,Tr11), (Tr20,Tr10))
@test !issubtrait((Tr21,Tr11), (Tr10,Tr20)) # todo: this should be true, as order shouldn't matter
@test issubtrait((Tr11,Tr21), (Tr10,Tr20))

@test !issubtrait(Tr21{Int}, Tr20{Float64})
@test !issubtrait((Tr21{Int},), (Tr20{Float64},))

####
# Test functions parameterized on non-trait parameters.
###
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

@traitdef Pr1{X}  begin
    fn76{I<:Integer}(X, Vector{I}) -> I
end
fn76{I<:Integer}(x::Uint8, v::Vector{I}) = v[x]
if method_exists_bug2
    @test !istrait(Pr1{UInt8})
else
    @test istrait(Pr1{UInt8})
end

@traitdef Pr2{X} begin
    fn77{Y<:Number}(X,Y,Y) -> Y
#    fn77{Y}(X)
end
fn77(a::Array,b::Int, c::Float64) = a[1]
@test !istrait(Pr2{Array})
fn77{Y<:Real}(a::Array,b::Y, c::Y) = a[1]
@test !istrait(Pr2{Array})
fn77{Y<:Number}(a::Array,b::Y, c::Y) = a[1]
@test istrait(Pr2{Array})

#####
# Trait functions parameterized on trait parameters
####

@traitdef Pr3{X} begin
    fn78{T<:X}(T,T)
end
fn78(b::Int, c::Int) = b
@test istrait(Pr3{Int})
fn78(b::Real, c::Real) = b
@test !istrait(Pr3{Real})
fn78{T}(b::T, c::T) = b
@test istrait(Pr3{Real})
@test istrait(Pr3{Any})

@traitdef Pr04{X} begin
    fnpr04{T<:X, S<:Integer}(T,T, S, S)
end
fnpr04(b::Int, c::Int, ::Int, ::Int) = b
@test !istrait(Pr04{Int})
fnpr04{I<:Integer}(b::Int, c::Int, ::I, ::I) = b
@test istrait(Pr04{Int})


@traitdef Pr05{X} begin
    fnpr05{T<:X, S<:Integer}(Dict{T,T}, Dict{S,T})
end
fnpr05{T<:FloatingPoint, S<:Integer}(::Dict{T,T}, ::Dict{S,T}) = 1
@test istrait(Pr05{Float64})

@traitdef Pr06{X} begin
    fnpr06{T<:X, S<:Integer}(Dict{T,S}, Dict{S,T})
end
fnpr06{T<:FloatingPoint, S<:Integer}(::Dict{T,S}, ::Dict{S,T}) = 1
@test istrait(Pr06{Float64})


@traitdef Pr07{X} begin
    fnpr07(X, X, Integer)
end
fnpr07{T<:Integer}(::T, ::T, ::Integer) = 1
@test !istrait(Pr07{Integer})
@test istrait(Pr07{Int})

# Test constraints
###
@traitdef Cr20{X} begin
    length(X) -> Any
    
    @constraints begin
        string(X.name)[1]=='I'
    end
end

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

# isbits associated types, issue #9

@traitdef AssocIsBits{X} begin
    T = X.parameters[1]  # a type
    N = X.parameters[2]  # a isbits
    S = X.parameters[3]  # a symbol
    getindex(X, Int) -> T
end
type T3484675{T,N,S} end
Base.getindex(::T3484675, i::Int) = i
AssocIsBits{T3484675{Int,4.5,:a}}()
@test istrait(AssocIsBits{T3484675{Int,4.5,:a}}) # errors because it is assumed that all
                                                 # parameters are TypeVars
#####
# Varags
#####
@traitdef TT31{X} begin
    foo31(X, Int...)
end
foo31(::String, x::UInt...) = 1
@test !istrait(TT31{String})
foo31(::String) = 2 # to avoid ambiguity warnings
foo31(::String, x::Int...) = 2
@test istrait(TT31{String})

@traitdef TT32{X} begin
    foo32(X...)
end
foo32(::String) = 1
@test !istrait(TT32{String})
foo32(a::String...) = 2 # to avoid ambiguity warnings
@test istrait(TT32{String})

@traitdef TT33{X} begin
    foo33{Y<:X}(X, Y...)
end
foo33(::String) = 1
@test !istrait(TT33{String})
foo33{T<:String}(::String, a::T...) = 2 
@test istrait(TT33{String})

####
# DataType constructors
####

@traitdef TT45{D} begin
    # This trait contains all datatypes which have a constructor with
    # no arguments.
    D() -> D
end
type A4758 end
type A4759
    a
end

@test istrait(TT45{A4758})
@test !istrait(TT45{A4759})
@test istrait(TT45{Dict{Int,Int}})
@test istrait(TT45{Set{Int}})
@test !istrait(TT45{Int})
@test !istrait(TT45{Array{Int,1}})



@traitdef TT44{D} begin
    Array(Type{D},Integer) # the standard array constructor, should be working for all Types
end
@test istrait(TT44{A4758})
@test istrait(TT44{A4759})
@test istrait(TT44{Dict{Int,Int}})
@test istrait(TT44{Set{Int}})
@test istrait(TT44{Int})
@test istrait(TT44{Array{Int,1}})


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
    @test istrait(TT46{Dict{Int,Int}}, verbose=verbose) # this is a false positive
end
# @test istrait(TT46{Set{Int}}, verbose=verbose) this actually works, but not as expected and gives a deprecation warning
@test !istrait(TT46{Int})
@test istrait(TT46{Array{Int,1}}, verbose=verbose)
# @test istrait(TT46{Array{Int}}, verbose=verbose) # this does not pass currently because of https://github.com/JuliaLang/julia/issues/10642
@test istrait(TT46{Array}, verbose=verbose)


