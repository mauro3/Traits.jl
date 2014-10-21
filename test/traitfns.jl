# tests parsing in src/traitfns.jl

f1e= :(f1{X<:Int,TT; D1{X}, D2{X,TT}}(x::X,y::TT))
f1e_b= :(f1{X<:Int,TT; D1{X}, D2{X,TT}}(x::X,y::TT) = ())

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

@test Traits.parsetraitfn_head(f1e)==f1e_p
@test Traits.translate_head(Traits.parsetraitfn_head(f1e))==f1e_pt
@test Traits.parsetraitfn(f1e_b)==(f1e_p, f1e_pt)

@test Traits.makefnhead(f1e_p.name, f1e_p.typs, f1e_p.sig).args[1]==f1e_p.fun
@test Traits.makefnhead(f1e_p.name, f1e_p.typs, f1e_p.sig).args[2:end]==f1e_p.sig

@test Traits.makefncall(f1e_p.name, f1e_p.sig)==:(f1(x,y))

@test Traits.get_concrete_type(f1e_p.typs)==Any[:Int, :Any]

@test Traits.make_Type_sig(f1e_p.typs)==Any[:(::Type{X}), :(::Type{TT})]

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
