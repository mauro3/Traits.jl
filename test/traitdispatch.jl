function compare_code_native(f1, f2, types)
    # A very crude way to compare whether two functions produce the
    # same machine code: compare their lengths.  Returns the relative
    # difference of the length of code_native of two functions.
    #
    # Note, when running with --code-coverage the traits-functions
    # will be much longer than the duck-typed ones.
    df1 = Base._dump_function(f1, types, true, false)
    df2 = Base._dump_function(f2, types, true, false)
    # the first two lines have some file names, etc
    df1 = *(split(df1, "\n")[3:end]...)
    df2 = *(split(df2, "\n")[3:end]...)
    abs(length(df1)-length(df2))/length(df2)
end

###############
# recreate manual-traitdispatch.jl: f2

@traitfn ttf2{X,Y<:Integer; D1{Y}, D4{X,Y}}(x::X,y::Y) = x + sin(y)
@traitfn ttf2{S,T<:Integer; D1{S}, D1{T}  }(s::S,t::T) = sin(s) - sin(t)
@traitfn ttf2{X,Y<:FloatingPoint; D1{X}, D1{Y}  }(x::X,y::Y) = cos(x) - cos(y)

@test ttf2(4,5)==(4 + sin(5))
@test ttf2(4,5.) == cos(4)-cos(5.)

@test ttf2(MTT1(5),4) == sin(5)-sin(4)
@test ttf2(MTT2(5),3) == sin(3)+5

@test length(methods(f2))==length(methods(ttf2))

################
@traitfn ft1{X,Y; Eq{X,Y}}(x::X,y::Y) = x==y
ff1(x,y) = x==y
@test ff1(4,5)==ft1(4,5)
# @code_llvm ft1(4,5)
# @code_llvm ff1(4,5)
@test 0.02<compare_code_native(ff1, ft1, (Int,Int))<0.03
@test 0.015compare_code_native(ff1, ft1, (BigFloat,BigInt))<0.025

#################
@traitfn function ft2{X,Y; Arith{X,Y}}(x::X,y::Y) 
    out = zero(promote(x,y)[1])
    for xe in 1:round(Int,x)
        out += xe + y
    end
    out
end
function ff2{X,Y}(x::X,y::Y) 
    out = zero(promote(x,y)[1])
    for xe in 1:round(Int,x)
        out += xe + y
    end
    out
end
        
@test ff2(7.3,5.)==ft2(7.3,5.)
# check the generated code is within some % of each other
@test 0.05<compare_code_native(ff2, ft2, (Int,Int))<0.07
@test 0.05<compare_code_native(ff2, ft2, (BigFloat,BigInt))<0.7
# @code_llvm ft2(7.3,5.)
# @code_llvm ff2(7.3,5.)



##################

@traitfn ft3{X1,X2; Iter{X1}}(x::X1,y::X2) = (out=X2[]; for i in x; push!(out,i+y) end; out)
ff3{Y}(x,y::Y) = Y[i+y for i in x]

@test ff3([1:10],5)==ft3([1:10],5)
@test ff3(BigFloat[1:10],BigInt(5))==ft3(BigFloat[1:10],BigInt(5))
# @code_llvm ft3([1:10],5)
# @show "---------------------------------------------------"
# @code_llvm ff3([1:10],5)
@test compare_code_native(ff3, ft3, (Array{Int,1},Int))<0.17
@test compare_code_native(ff3, ft3, (Array{BigFloat,1},BigInt))<0.1


###############
@traitdef MyTr{X,Y} begin
    foobar(X,Y) -> Bool
end
type A
    a::Int
end
foobar(a::A, b::A) = a.a==b.a
@test istrait(MyTr{A,A})  # true
@test istrait(MyTr{Int,Int})==false

# make a function which dispatches on traits:
@traitfn ft111{X,Y; Cmp{X,Y}}(x::X,y::Y)  = x>y ? 5 : 6
@traitfn ft111{X,Y; MyTr{X,Y}}(x::X,y::Y) = foobar(x,y) ? -99 : -999

@test ft111(4,5)==6
@test ft111(A(5), A(6))==-999

@test_throws TraitException ft111("asdf", 5)
foobar(a::String, b::Int) = length(a)==b
@test ft111("asdf", 4)==-99

## dispatch using subtraits
@traitdef MyTr2{X,Y} <: MyTr{X,Y} begin
    bar(X,Y) -> Bool
end

@traitfn gt1{X,Y; MyTr2{X,Y}}(x::X,y::Y)  = "MyTr2"
@traitfn gt1{X,Y; MyTr{X,Y}}(x::X,y::Y)   = "MyTr"

type B1
    a::Int
end
foobar(a::B1, b::B1) = a.a==b.a
type B2
    a::Int
end
foobar(a::B2, b::B2) = a.a==b.a
bar(a::B2, b::B2) = a.a==b.a
@test istrait(MyTr{B1,B1})  # true
@test istrait(MyTr2{B1,B1})==false
@test istrait(MyTr2{B2,B2})

@test gt1(B1(1), B1(1))=="MyTr"
@test gt1(B2(1), B2(1))=="MyTr2" 


##########
## adding trait methods to existing functions:
@traitdef MyTr7{X} begin
    bobo(X) -> String
end

abstract AMTyp7
type MTyp71 <: AMTyp7
end
type MTyp72 <: AMTyp7
end
# add to above trait
@traitimpl MyTr7{MTyp71} begin
    bobo(X::MTyp71) = "Yeah"
end

import Base.sin
@traitfn sin{X; MyTr7{X}}(x::X) = bobo(x)
@test sin(MTyp71())=="Yeah"
@test_throws TraitException sin(MTyp72())
@test length(traitmethods(sin))==1

@traitfn Base.cos{X; MyTr7{X}}(x::X) = bobo(x)
@test cos(MTyp71())=="Yeah"
@test_throws TraitException cos(MTyp72())

######
# Ambiguities
######
@traitdef TrTr1{X} begin
    len1(X) 
end

@traitfn tf7465{X<:Integer,Y; TrTr1{X}}(x::X,y::Y) = x-y
@traitfn tf7465{X<:Integer,Y; TrTr1{Y}}(x::X,y::Y) = x+y

@traitimpl TrTr1{Int} begin
    len1(x::Int) = 5
end

# now ambiguous:
@test_throws Traits.TraitException tf7465(5,6)
# resolve
@traitfn tf7465{X<:Integer,Y; TrTr1{X}, TrTr1{Y}}(x::X,y::Y) = x*y
@test tf7465(5,6)==5*6

@traitdef TrTr22{X,Y} <: TrTr1{X}, TrTr1{Y} begin
    # empty
end

@traitfn tf7465{X<:Integer,Y; TrTr22{X,Y}}(x::X,y::Y) = x*y*1000
  # errors again because ambigours again
@test_throws Traits.TraitException tf7465(5,6)

## single argument ambiguities
####
@traitdef TrTr2{X} begin
    len2(X) 
end

@traitfn tttf{X; TrTr1{X}}(x::X) = len1(x)
@traitfn tttf{X; TrTr2{X}}(x::X) = len2(x)

@test tttf(5)==len1(5)  # works as TrTr2{Int} is not a trait

@traitimpl TrTr1{Float64} begin
    len1(x::Float64) = 5
end
@traitimpl TrTr2{Float64} begin
    len2(x::Float64) = 55
end

@test_throws Traits.TraitException tttf(5.)  # errors

@traitfn tttf{X; TrTr1{X}, TrTr2{X}}(x::X) = len2(x) # pick len2 over len1
@test tttf(5.)==len2(5.)

# # this can also be resolved by using a sub-trait, essentially equivalent:
# @traitdef STrTr{X} <: TrTr1{X}, TrTr2{X} begin
#     # empty
# end
# # no need to implement it for Float64: it's already a member:
# @test istrait(STrTr{Float64})


## test resolution of single argument ambiguities with subtraits
####
# came up here: https://github.com/mauro3/Traits.jl/pull/4#issuecomment-69742371
@traitdef U1{X} begin
    u1(X)
end
@traitdef U2{X} <: U1{X} begin
    u2(X)
end

@traitdef UU1{X} <: U1{X} begin
    uu1(X)
end
@traitdef UU2{X} <: U2{X} begin
    uu2(X)
end

@traitimpl U1{Int} begin
    u1(x::Int) = 1
end
@traitimpl U2{Int} begin
    u2(x::Int) = 2
end
@traitimpl UU1{Int} begin
    uu1(x::Int) = 11
end
@traitimpl UU2{Int} begin
    uu2(x::Int) = 12
end

@traitfn tttf238{X; UU1{X}}(x::X) = "this should loose"
@traitfn tttf238{X; UU2{X}}(x::X) = "this should win"  

println("This test in traitdispatch.jl should probably pass, fix dispatch and change here.")
@test_throws Traits.TraitException tttf238(5)=="this should win"

# however if U2 were a subtrait of something else but U1 then dispatch should be ambiguous:
@traitdef V1{X} begin
    v1(X)
end
@traitdef Other1{X} begin
    other1(X)
end

@traitdef V2{X} <: Other1{X} begin
    v2(X)
end

@traitdef VV1{X} <: V1{X} begin
    vv1(X)
end
@traitdef VV2{X} <: V2{X} begin
    vv2(X)
end

@traitimpl V1{Int} begin
    v1(x::Int) = 1
end
@traitimpl Other1{Int} begin
    other1(x::Int) = -1
end
@traitimpl V2{Int} begin
    v2(x::Int) = 2
end
@traitimpl VV1{Int} begin
    vv1(x::Int) = 11
end
@traitimpl VV2{Int} begin
    vv2(x::Int) = 12
end

@traitfn tttf240{X; VV1{X}}(x::X) = "neither should win 1"
@traitfn tttf240{X; VV2{X}}(x::X) = "neither should win 2"
@test_throws Traits.TraitException tttf240(5)
