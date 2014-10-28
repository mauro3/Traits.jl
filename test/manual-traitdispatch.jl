## Testing using traits for dispatch, hand-coded
################################################

## the definitions:
# @traitfn f2{X,Y<:Integer; D1{Y}, D4{X,Y}}(x::X,y::Y) = x + sin(y)
# @traitfn f2{S,T<:Integer; D1{S}, D1{T}  }(s::S,t::T) = sin(s) - sin(t)
# @traitfn f2{X,Y<:FloatingPoint; D1{X}, D1{Y}  }(x::X,y::Y) = cos(x) - cos(y)

immutable TraitType end

# should be turned into:
f2{X,Y<:Integer}(x::X, y::Y)       = f2(x, y, f2(TraitType(), x,y) )
f2{S,T<:Integer}(s::S, t::T)       = f2(s, t, f2(TraitType(), s,t) )
f2{X,Y<:FloatingPoint}(x::X, y::Y) = f2(x, y, f2(TraitType(), x,y) )
# (just (re-)define f2(x,y) for all traitdef's.  Also use normal-types
# so that it doesn't occupy f2(x::Any, y::Any)

# the logic is:
@inline f2{X,Y<:Integer}(x::X, y::Y, ::Type{(D1{Y}, D4{X,Y})}) = x + sin(y)
@inline f2{S,T<:Integer}(s::S, t::T, ::Type{(D1{S}, D1{T})}) = sin(s) - sin(t)
@inline f2{X,Y<:FloatingPoint}(x::X, y::Y, ::Type{(D1{X}, D1{Y})}) = cos(x) - cos(y)

# the trait dispatch.  Here I rename all the arguments and type parameters.

function f2{X1,X2<:FloatingPoint}(::TraitType, ::Type{X1}, ::Type{X2})
    # This method serves as a storage for the trait-types of a certain
    # normal-type signature.
    [(D1{X1}, D1{X2})], Any[:(D1{X1}, D1{X2})]
end
stagedfunction f2{X1,X2<:FloatingPoint}(::TraitType, x1::X1, x2::X2)
    traittypes = f2(TraitType(), x1, x2)[1]

    poss = Any[] # list of satisfied trait-types
    for Tr in traittypes
        if istrait(Tr)
            push!(poss, Tr)
        end
    end
    if length(poss)==0
        throw(Traits.TraitException("No matching trait found for function f2"))
    elseif length(poss)>1
        throw(Traits.TraitException("Several matching traits found for function f2"))
    end
    # construct function from poss[1]
    out = :(())
    for s in poss[1]
        push!(out.args, :($s))
    end
    return out
end


function f2{X1,X2<:Integer}(::TraitType, ::Type{X1}, ::Type{X2})
    [(D1{X2}, D4{X1,X2}), (D1{X1}, D1{X2})], Any[:(D1{X2}, D4{X1,X2}), :(D1{X1}, D1{X2})]
end
stagedfunction f2{X1,X2<:Integer}(::TraitType, x1::X1, x2::X2)
    traittypes = f2(TraitType(), x1, x2)[1]

    poss = Any[]
    for Tr in traittypes
        if istrait(Tr)
            push!(poss, Tr)
        end
    end
    if length(poss)==0
        throw(Traits.TraitException("No matching trait found for function f2"))
    elseif length(poss)>1
        throw(Traits.TraitException("Several matching traits found for function f2"))
    end

    out = :(())
    for s in poss[1]
        push!(out.args, :($s))
    end
    return out
end

# @traitfn f2{X,Y<:Integer; D1{Y}, D4{X,Y}}(x::X,y::Y) = sin(x) + y
# @traitfn f2{S,T<:Integer; D1{S}, D1{T}  }(s::S,t::T) = sin(s) - sin(t)
# @traitfn f2{X,Y<:FloatingPoint; D1{X}, D1{Y}  }(x::X,y::Y) = cos(x) - cos(y)

type MTT1  # is in D1 but not D4
    f::Float64
end
Base.sin(x::MTT1) = sin(x.f)
Base.cos(x::MTT1) = cos(x.f)

type MTT2  # is in D4 but not D1
    f::Float64
end
+(y::MTT2, x::Number) = x+y.f
-(y::MTT2, x::Number) = x-y.f


@test_throws Traits.TraitException f2(4,5) # ambiguous trait-type
@test f2(4,5.) == cos(4)-cos(5.)

@test f2(MTT1(5),4) == sin(5)-sin(4)
@test f2(MTT2(5),3) == sin(3)+5


#####################################################

#######
# This is the approach I took before above example.  It's as above but
# with using _trait_f and _trait_type_f to hold the logic and dispatch
# function.  This approach has problems when the function is defined
# in several modules.
#######

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
        if istrait(Tr)
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
        if istrait(Tr)
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
    f::Float64
end
Base.sin(x::MT1) = sin(x.f)
Base.cos(x::MT1) = cos(x.f)

type MT2  # is in D4 but not D1
    f::Float64
end
+(y::MT2, x::Number) = x+y.f
-(y::MT2, x::Number) = x-y.f


@test_throws Traits.TraitException f1(4,5) # ambiguous trait-type
@test f1(4,5.) == cos(4)-cos(5.)

@test f1(MT1(5),4) == sin(5)-sin(4)
@test f1(MT2(5),3) == sin(3)+5

