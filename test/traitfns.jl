# tests parsing in src/traitfns.jl

f1e  = :(f1{X<:Int,TT; D1{X}, D2{X,TT}}(x::X,y::TT))
f1e_b= :(f1{X<:Int,TT; D1{X}, D2{X,TT}}(x::X,y::TT) = ())

f1e_function = :(function f1{X<:Int,TT; D1{X}, D2{X,TT}}(x::X,y::TT)
                ()
                end)

f1e_p = Traits.ParsedFn(
                  :f1, 
                  :(f1{X<:Int,TT}), 
                  Any[Expr(:<:, :X, :Int),:TT], 
                  Any[:(x::X), :(y::TT)], 
                  Any[:(D1{X}), :(D2{X,TT})],
                  :(()))
f1e_pt = Traits.ParsedFn(
                 :f1, 
                 :(f1{X1<:Int,X2}), 
                 Any[Expr(:<:, :X1, :Int),:X2],
                 Any[:(x1::X1), :(x2::X2)], 
                 Any[:(D1{X1}), :(D2{X1,X2})],
                 :(()))
function ==(p::Traits.ParsedFn, q::Traits.ParsedFn) 
    out = true
    for n in fieldnames(p)
        if n==:body # tricky to compare...
            continue
        end
        out = out && getfield(p,n)==getfield(q,n)
        if !out
            @show n, getfield(p,n), getfield(q,n)
        end
    end
    out
end

         
@test Traits.parsetraitfn_head(f1e)==f1e_p
@test Traits.translate_head(Traits.parsetraitfn_head(f1e))==f1e_pt
@test Traits.parsetraitfn(f1e_b)==(f1e_p, f1e_pt)

(a,b) = Traits.parsetraitfn(f1e_function)
a.body =:()
b.body =:()
@test a==f1e_p
@test b==f1e_pt

@test Traits.makefnhead(f1e_p.name, f1e_p.typs, f1e_p.sig).args[1]==f1e_p.fun
@test Traits.makefnhead(f1e_p.name, f1e_p.typs, f1e_p.sig).args[2:end]==f1e_p.sig

@test Traits.makefncall(f1e_p.name, f1e_p.sig)==:(f1(x,y))

@test Traits.get_concrete_type_symb(f1e_p.typs)==Any[:Int, :Any]

@test Traits.make_Type_sig([s.args[2] for s in f1e_p.sig])==Any[:(::Type{X}), :(::Type{TT})]

# next case
f1e2= :(f1{X<:Int; D1{X}, D2{X,X}}(x::X,y::X) = ())
f1e2_pt = Traits.ParsedFn(
                 :f1, 
                 :(f1{X1<:Int}), 
                 Any[Expr(:<:, :X1, :Int)],
                 Any[:(x1::X1), :(x2::X1)], 
                 Any[:(D1{X1}), :(D2{X1,X1})],
                 :(()))
@test Traits.parsetraitfn(f1e2)[2]==f1e2_pt

# next case
f1ee = :(tfd{K, V; D2{K,V}}(x::Vector{K}, y::V) = ())
f1ee_p = Traits.ParsedFn(
                  :tfd, 
                  :(tfd{K,V}), 
                  Any[:K,:V], 
                  Any[:(x::Vector{K}), :(y::V)], 
                  Any[:(D2{K,V})],
                  :(()))
f1ee_pt = Traits.ParsedFn(
                  :tfd, 
                  :(tfd{X1,X2}), 
                  Any[:X1,:X2], 
                  Any[:(x1::Vector{X1}), :(x2::X2)], 
                  Any[:(D2{X1,X2})],
                  :(()))
@test Traits.parsetraitfn(f1ee)==(f1ee_p, f1ee_pt)

# next case
f1ee2 = :(tfd{K, V; D2{K,V}}(x::Dict{K,Int}, y::V) = ())
f1ee2_p = Traits.ParsedFn(
                  :tfd, 
                  :(tfd{K,V}), 
                  Any[:K,:V], 
                  Any[:(x::Dict{K,Int}), :(y::V)], 
                  Any[:(D2{K,V})],
                  :(()))
f1ee2_pt = Traits.ParsedFn(
                  :tfd, 
                  :(tfd{X1,X2}), 
                  Any[:X1,:X2], 
                  Any[:(x1::Dict{X1,Int}), :(x2::X2)], 
                  Any[:(D2{X1,X2})],
                  :(()))
@test Traits.parsetraitfn(f1ee2)==(f1ee2_p, f1ee2_pt)

# next case
f1d2 = :(tfd{K, V; D2{K,V}}(x::Dict{K,Int}, y::V, z::Int) = ())
f1d2_p = Traits.ParsedFn(
                  :tfd, 
                  :(tfd{K,V}), 
                  Any[:K,:V], 
                  Any[:(x::Dict{K,Int}), :(y::V), :(z::Int)], 
                  Any[:(D2{K,V})],
                  :(()))
f1d2_pt = Traits.ParsedFn(
                  :tfd, 
                  :(tfd{X1,X2}), 
                  Any[:X1,:X2], 
                  Any[:(x1::Dict{X1,Int}), :(x2::X2), :(x3::Int)], 
                  Any[:(D2{X1,X2})],
                  :(()))
@test Traits.parsetraitfn(f1d2)==(f1d2_p, f1d2_pt)


# trait function creation
eval(:(@traitfn $f1e_b))
eval(:(@traitfn $f1e_function))
eval(:(@traitfn $f1e2))
# these give warnings for now
eval(:(@traitfn $f1ee))
eval(:(@traitfn $f1ee2))
eval(:(@traitfn $f1d2))


@traitfn yt1{X,Y; Arith{X,Y}}(x::X,y::Y) = x+y
a = yt1(5,6)
@test yt1(5,6)==5+6
@traitfn xt1{X<:Int,Y<:FloatingPoint; Arith{X,Y}}(x::X,y::Y) = x-y
@test_throws MethodError xt1(5,6)

if !(method_exists_bug1)
    od = ObjectIdDict(); od[5]=8
    @traitfn yt1{X,Y; Assoc{X}}(x::X,y::Y) = x==y
    @test yt1(Dict(5=>7), Dict(5=>8))==false
    @traitfn xt1{X<:Dict,Y<:ObjectIdDict; Assoc{X}}(x::X,y::Y) = x==y
    @test xt1(Dict(5=>7), od)==false
    @test_throws MethodError xt1(od, od)
end

# mixing with normal methods:
xt1(x::Int, y::Int) = 77
@test xt1(5,6)==77


# scope of helper methods when defined in different modules
####
## case 1

    module Mod1
    using Traits
    # to check what happens with exports, etc
    export MTyp1, M1Tr1, tf1, barbar
    
    type MTyp1
        mt::Int
    end
    
    @traitdef M1Tr1{X} begin
        barbar(X, Int) -> String
    end
    @traitimpl M1Tr1{MTyp1} begin
        barbar(a::MTyp1, b::Int) = "MTyp1, barbar"^b
    end
    
    @traitfn tf1{X, Y<:Int;  M1Tr1{X}}(a::X, b::Y) = barbar(a, b)

    # a normal generic function to be used later
    f1(x::Int) = 5x
    
    end # module Tst

using Mod1
@test tf1(MTyp1(7), 5)=="MTyp1, barbar"^5

# now try to extend tf1

# 1) add a new type to M1Tr1
type MTyp99
    uu::Int
end
@traitimpl M1Tr1{MTyp99} begin
    barbar(a::MTyp99, b::Int) = "MTyp99, barbar"^b
end
@test tf1(MTyp99(9), 5)=="MTyp99, barbar"^5
@test tf1(Main.MTyp99(9), 5)=="MTyp99, barbar"^5

# 2) add a new method to tf1
@traitdef M1Tr100{X} begin
    foofoo(X) -> String
end
type MTyp00
    gg::Int
end
@traitimpl M1Tr100{MTyp00} begin
    foofoo(a::MTyp00) = "MTyp00 foofoo"
end

import Mod1.tf1
# @show tf1(Traits._TraitStorage, MTyp1, Int)
# @show tf1(Traits._TraitStorage,Any,Int)
# typs = Any[:(Traits._TraitStorage),:X, Expr(:<:, :X1, :Int)]
# methods(eval(:tf1), Traits. get_concrete_type_Typetuple(typs))
println("  These warnings are ok:")  # Well, I'm not sure whether they are ok.  But at least normal...
@traitfn tf1{X, Y<:Int;  M1Tr100{X}}(a::X, b::Y) = foofoo(a)^b
println("  endof ok-warnings.")
#methods(eval(:tf1), Traits. get_concrete_type_Typetuple(typs))
#@show tf1(Traits._TraitStorage, Any, Int)

@test tf1(MTyp1(7), 5)=="MTyp1, barbar"^5
@test tf1(MTyp00(9), 5)== "MTyp00 foofoo"^5

# @show tf1(Traits._TraitStorage, MTyp1, Int)
# @show tf1(Traits._TraitStorage, Any, Int)

# # error:
@test tf1(MTyp99(9), 5)=="MTyp99, barbar"^5

# case 2: adding a @traitfn to a generic function defined in another module
@traitfn tf1_2{X;  M1Tr100{X}}(a::X) = foofoo(a)^99

############
# error
@traitfn ffgg{T; Eq{T,T}}(x::T,y::T) = 2x==y
@test ffgg(5,6)==false
@test ffgg(6,12)==true

######
# traitmethods in modules
@traitfn ff879{T; Eq{T,T}}(x::T,y::T) = 2x==y
@test length(traitmethods(ff879))==1
module A9374
using Traits
using Base.Test
@traitfn ff879{T; Eq{T,T}}(x::T,y::T) = 2x==y
@test length(traitmethods(ff879))==1
end
@test length(traitmethods(ff879))==1
