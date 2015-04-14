using Traits
# Check Cmp-trait (comparison) which is implemented in Traits.jl/src/commontraits.jl
@assert istrait(Cmp{Int,Float64})        # Int and Float64 can be compared
@assert istrait(Cmp{Int,String})==false  # Int and String cannot be compared

# make a new trait and add a type to it:
@traitdef MyTr{X,Y} begin
    foobar(X,Y) -> Bool # All type-tuples for which there is a method foo
                        # with that signature belong to MyTr 
end
type A
    a::Int
end
@assert istrait(MyTr{A,A})==false  # foobar not implement yet
foobar(a::A, b::A) = a.a==b.a      # implement it
@assert istrait(MyTr{A,A})         # voila!
@assert istrait(MyTr{Int,Int})==false

# make a function which dispatches on traits:
@traitfn ft1{X,Y; Cmp{X,Y}}(x::X,y::Y)  = x>y ? 5 : 6
@traitfn ft1{X,Y; MyTr{X,Y}}(x::X,y::Y) = foobar(x,y) ? -99 : -999

ft1(4,5)        # ==6    i.e. dispatches to first definition
ft1(A(5), A(6)) # ==-999 i.e. dispatches to second definition

try
    ft1("asdf", 5)
catch err
    println(err)
end
foobar(a::String, b::Int) = length(a)==b
ft1("asdf", 5)

## dispatch using subtraits
@traitdef MyTr2{X,Y} <: MyTr{X,Y} begin
    bar(X,Y) -> Bool
end

@traitfn gt1{X,Y; MyTr2{X,Y}}(x::X,y::Y)  = "MyTr2"
@traitfn gt1{X,Y; MyTr{X,Y}}(x::X,y::Y)   = "MyTr"

type B1{T}
    a::T
end
foobar(a::B1, b::B1) = a.a==b.a
type B2{T}
    a::T
end
foobar(a::B2, b::B2) = a.a==b.a
bar(a::B2, b::B2) = a.a==b.a
@assert istrait(MyTr{B1{Int},B1{Int}})  # true
@assert istrait(MyTr2{B1,B1})==false
@assert istrait(MyTr2{B2{Int},B2{Int}}) # need the type parameters
                                        # here for Julia to infer the
                                        # return type

@assert gt1(B1(1), B1(1))=="MyTr"
@assert gt1(B2(1), B2(1))=="MyTr2" 
