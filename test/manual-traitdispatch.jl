## Testing using traits for dispatch, hand-coded
################################################

## the definitions:
# @traitfn f1{X,Y<:Integer; D1{Y}, D4{X,Y}}(x::X,y::Y) = x + sin(y)
# @traitfn f1{S,T<:Integer; D1{S}, D1{T}  }(s::S,t::T) = sin(s) - sin(t)
# @traitfn f1{X,Y<:FloatingPoint; D1{X}, D1{Y}  }(x::X,y::Y) = cos(x) - cos(y)

# should be turned into:
f1{X,Y<:Integer}(x::X, y::Y)       = _trait_f1(x, y, _trait_type_f1(x,y) )
f1{S,T<:Integer}(s::S, t::T)       = _trait_f1(s, t, _trait_type_f1(s,t) )
f1{X,Y<:FloatingPoint}(x::X, y::Y) = _trait_f1(x, y, _trait_type_f1(x,y) )
# (just (re-)define f1(x,y) for all traitdef's.  Also use normal-types
# so that it doesn't occupy f1(x::Any, y::Any)

# the logic is:
@inline _trait_f1{X,Y<:Integer}(x::X, y::Y, ::Type{(D1{Y}, D4{X,Y})}) = x + sin(y)
@inline _trait_f1{S,T<:Integer}(s::S, t::T, ::Type{(D1{S}, D1{T})}) = sin(s) - sin(t)
@inline _trait_f1{X,Y<:FloatingPoint}(x::X, y::Y, ::Type{(D1{X}, D1{Y})}) = cos(x) - cos(y)

# the trait dispatch.  Here I rename all the arguments and type parameters.

function _trait_type_f1{X1,X2<:FloatingPoint}(::Type{X1}, ::Type{X2})
    # This method serves as a storage for the trait-types of a certain
    # normal-type signature.
    [(D1{X1}, D1{X2})], Any[:(D1{X1}, D1{X2})]
end
stagedfunction _trait_type_f1{X1,X2<:FloatingPoint}(x1::X1, x2::X2)
    traittypes = _trait_type_f1(x1, x2)[1]

    poss = Any[] # list of satisfied trait-types
    for Tr in traittypes
        if traitcheck(Tr)
            push!(poss, Tr)
        end
    end
    if length(poss)==0
        throw(Traits.TraitException("No matching trait found for function f1"))
    elseif length(poss)>1
        throw(Traits.TraitException("Several matching traits found for function f1"))
    end
    # construct function from poss[1]
    out = :(())
    for s in poss[1]
        push!(out.args, :($s))
    end
    return out
end


function _trait_type_f1{X1,X2<:Integer}(::Type{X1}, ::Type{X2})
    [(D1{X2}, D4{X1,X2}), (D1{X1}, D1{X2})], Any[:(D1{X2}, D4{X1,X2}), :(D1{X1}, D1{X2})]
end
stagedfunction _trait_type_f1{X1,X2<:Integer}(x1::X1, x2::X2)
    traittypes = _trait_type_f1(x1, x2)[1]

    poss = Any[]
    for Tr in traittypes
        if traitcheck(Tr)
            push!(poss, Tr)
        end
    end
    if length(poss)==0
        throw(Traits.TraitException("No matching trait found for function f1"))
    elseif length(poss)>1
        throw(Traits.TraitException("Several matching traits found for function f1"))
    end

    out = :(())
    for s in poss[1]
        push!(out.args, :($s))
    end
    return out
end

# @traitfn f1{X,Y<:Integer; D1{Y}, D4{X,Y}}(x::X,y::Y) = sin(x) + y
# @traitfn f1{S,T<:Integer; D1{S}, D1{T}  }(s::S,t::T) = sin(s) - sin(t)
# @traitfn f1{X,Y<:FloatingPoint; D1{X}, D1{Y}  }(x::X,y::Y) = cos(x) - cos(y)

type MT1  # is in D1 but not D4
    f
end
Base.sin(x::MT1) = sin(x.f)
Base.cos(x::MT1) = cos(x.f)

type MT2  # is in D4 but not D1
    f
end
+(y::MT2, x::Number) = x+y.f
-(y::MT2, x::Number) = x-y.f


@test_throws Traits.TraitException f1(4,5) # ambiguous trait-type
@test f1(4,5.) == cos(4)-cos(5.)

@test f1(MT1(5),4) == sin(5)-sin(4)
@test f1(MT2(5),3) == sin(3)+5

# ### Problem
# t{X,Y}(x::X,y::Y) = ((D1{X})(),(D2{X,Y})())
# @code_llvm t(4,5) # is not short

# tt{X,Y}(x::X,y::Y) = ((D1{X}),(D2{X,Y}))
# @code_llvm tt(4,5) # is short

# @code_llvm _trait_type_f1(5,6)

# @code_llvm f1(4.,5.)
# ff(x,y) = 2*x + y
# @code_llvm ff(4.,5.)


# -> return types not instances!

#######################################
# ## What about?

# ## the definitions:
# # @traitfn g1{X<:Int,Y; D1{X}, D2{X,Y}}(x::X,y::Y) = sin(x) + y
# # @traitfn g1{X<:Int,Y; D1{X}, D1{Y}}(x::X,y::Y) = sin(x) - sin(y)
# # @traitfn g1{X<:Float64,Y; D1{X}, D1{Y}}(x::X,y::Y) = sin(x) - sin(y)


# # should be turned into:
# stagedfunction g1{X<:Int,Y}(x::X, y::Y)

# end

# stagedfunction g1{X<:Float64,Y}(x::X, y::Y)

# end


# g1{X<:Int,Y}(x::X, y::Y)     = _trait_g1(x, y, _trait_type_g1(x,y) )
# g1{X<:Int,Y}(x::X, y::Y)     = _trait_g1(x, y, _trait_type_g1(x,y) )
# g1{X<:Float64,Y}(x::X, y::Y) = _trait_g1(x, y, _trait_type_g1(x,y) )
# # the logic for a given trait
# _trait_g1{X1<:Int,X2}(x::X1, y::X2, ::Type{(D1{X1}, D2{X1,X2})}) = sin(x) + y
# _trait_g1{X1<:Float64,X2}(x::X1, y::X2, ::Type{(D1{X1}, D1{X2})}) = sin(x) - sin(y)

# # now the trait-dispatch function also needs to be dependent on the type:
# _trait_type_g1{X<:Int,Y}(x::X,y::Y) = checks (D1{X1}, D2{X1,X2})

# _trait_type_g1{X<:Float64,Y}(x::X,y::Y) = checks (D1{X1}, D1{X2})
# # however now the trick with passing in _TraitSig does not work
# # anymore.  What should be done now to get the traits out of it?  Maybe a global?


# # No, leave it using Tim's trick.  As this separates nicely:
# # - normal function dispatch: f(x,y)
# # - logic: _trait_f1(x,y, ::Type{(...)}) = ...
# # - trait dispatch: _trait_type_f1(x,y}) = ...

# # ## Testing using traits for dispatch, hand-coded
# # ################################################

# # ## the definitions:
# # # @traitfn f1{X,Y; D1{X}, D2{X,Y}}(x::X,y::Y) = sin(x) + y
# # # @traitfn f1{X,Y; D1{X}, D1{Y}}(x::X,y::Y) = sin(x) - sin(y)

# # Should be turned into one stagedfunciton which does the trait dispatch
# # and either returns :(sin(x) + y) or :(sin(x) - sin(y))

# # Thus @traitfn needs to
# # - get the old definitions out of the
# #   - stagedfunction/function
# #   - or from elsewhere
# # - check if an existing definiton matches
# #   - overwrite if so
# #   - create new entry otherwise

# # One stagedfunction for each type signature (without the traits).
# # For each of them there might be several different trait-types.
# # (assume for now that all type variables are ordered).

# # One function of the same name which returns the different signatures
# # so the stagedfunction can be updated.

# # Problems: - avoid renaming variables, TypeVars if at all possible.
# #             Otherwise they need to be renamed in the body as well.

# # Ref:
# # - maybe should use: https://github.com/burrowsa/MetaTools.jl


# # # should be turned into:
# # f1(x, y) = _trait_f1(x, y, _trait_type_f1(x,y) )
# # # the logic for a given trait
# # # _trait_f1{X1,X2}(x::X1, y::X2, ::Type{(D1{X1}, D2{X1,X2})}) = 2*x + y
# # # _trait_f1{X1,X2}(x::X1, y::X2, ::Type{(D1{X1}, D3{X2})}) = get!(y, x, sin(x))

# # _trait_f1{X1,X2}(x::X1, y::X2, ::Type{(D1{X1}, D3{X2})}) = get!(y, x, sin(x))

# # @inline function _trait_f1{X1,X2}(x::X1,y::X2, ::Type{(D1{X1}, D2{X1,X2})})
# #     out = zero(promote(x,y)[1])
# #     for xe in 1:iround(x)
# #         out += xe + y
# #     end
# #     out
# # end

# # # ## note, this does not work:
# # # function _trait_f1{X1,X2}(x::X1,y::X2, ::Type{(D1{X1}, D2{X1,X2})})
# # #     Expr(:meta, :inline)
# # #     out = zero(promote(x,y)[1])
# # #     for xe in 1:iround(x)
# # #         out += xe + y
# # #     end
# # #     out
# # # end

# # function _trait_f1_notrait{X1,X2}(x::X1,y::X2)
# #     out = zero(promote(x,y)[1])
# #     for xe in 1:iround(x)
# #         out += xe + y
# #     end
# #     out
# # end


# # # the trait dispatch
# # stagedfunction _trait_type_f1(X1, X2)
# #     traittypes =  [(D1{X1}, D2{X1,X2}), (D1{X1}, D3{X2})]
# #     if X1<:_TraitSig
# #         # return all the trait-types.  This can be used to redefine
# #         # this function when more "method" definitions are added.
# #         return traittypes
# #     end

# #     poss = Any[]
# #     for Tr in traittypes
# #         if traitcheck(Tr)
# #             push!(poss, Tr)
# #         end
# #     end
# #     if length(poss)==0
# #         error("No matching trait found for function f1")
# #     elseif length(poss)>1
# #         error("Several matching traits found for function f1")
# #     end
# #     # construct function from poss[1]
# #     out = :(())
# #     for s in poss[1]
# #         push!(out.args, :($s))
# #     end
# #     return out
# # end

# # a = Dict()
# # @test f1(5,4) ==  _trait_f1_notrait(5,4)
# # f1(4,a)
# # @test a[4]==sin(4)
# # a[9] = 99
# # @test f1(9, a)==99

# # ff(x,y) = sin(x)+y

# # # ### Problem
# # # t{X,Y}(x::X,y::Y) = ((D1{X})(),(D2{X,Y})())
# # # @code_llvm t(4,5) # is not short

# # # tt{X,Y}(x::X,y::Y) = ((D1{X}),(D2{X,Y}))
# # # @code_llvm tt(4,5) # is short

# # # @code_llvm _trait_type_f1(5,6)

# # # @code_llvm f1(4.,5.)
# # # ff(x,y) = 2*x + y
# # # @code_llvm ff(4.,5.)


# # # -> return types not instances!
