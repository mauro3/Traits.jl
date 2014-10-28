# tests parsing in src/traitfns.jl

f1e= :(f1{X<:Int,TT; D1{X}, D2{X,TT}}(x::X,y::TT))
f1e_b= :(f1{X<:Int,TT; D1{X}, D2{X,TT}}(x::X,y::TT) = ())

f1e2= :(f1{X<:Int; D1{X}, D2{X,X}}(x::X,y::X) = ())

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

f1e2_pt = Traits.ParsedFn(
                 :f1, 
                 :(f1{X1<:Int}), 
                 Any[Expr(:<:, :X1, :Int)],
                 Any[:(x1::X1), :(x2::X1)], 
                 Any[:(D1{X1}), :(D2{X1,X1})],
                 :(()))

@test Traits.parsetraitfn_head(f1e)==f1e_p
@test Traits.translate_head(Traits.parsetraitfn_head(f1e))==f1e_pt
@test Traits.parsetraitfn(f1e_b)==(f1e_p, f1e_pt)

@test Traits.parsetraitfn(f1e2)[2]==f1e2_pt

@test Traits.makefnhead(f1e_p.name, f1e_p.typs, f1e_p.sig).args[1]==f1e_p.fun
@test Traits.makefnhead(f1e_p.name, f1e_p.typs, f1e_p.sig).args[2:end]==f1e_p.sig

@test Traits.makefncall(f1e_p.name, f1e_p.sig)==:(f1(x,y))

@test Traits.get_concrete_type_symb(f1e_p.typs)==Any[:Int, :Any]

@test Traits.make_Type_sig(f1e_p.typs)==Any[:(::Type{X}), :(::Type{TT})]
@test Traits.make_Type_sig(Any[:(Base.Array), Expr(:<:, :X, :Int),:TT], ) == Any[:(::Type{Base.Array}), :(::Type{X}), :(::Type{TT})]

# trait function creation
@traitfn yt1{X,Y; Arith{X,Y}}(x::X,y::Y) = x+y
@traitfn yt1{X,Y; Assoc{X}}(x::X,y::Y) = x==y

@test yt1(5,6)==5+6
@test yt1(Dict(5=>7), Dict(5=>8))==false

@traitfn xt1{X<:Int,Y<:FloatingPoint; Arith{X,Y}}(x::X,y::Y) = x-y
@traitfn xt1{X<:Dict,Y<:ObjectIdDict; Assoc{X}}(x::X,y::Y) = x==y

@test_throws MethodError xt1(5,6)
od = ObjectIdDict(); od[5]=8
@test xt1(Dict(5=>7), od)==false
@test_throws MethodError xt1(od, od)

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

@traitfn tf1{X, Y<:Int;  M1Tr100{X}}(a::X, b::Y) = foofoo(a)^b
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
