module Traits
# Traits are
# - contracts on one type or between several types.  The contract can
#   contain required methods but also other assertions and just
#   belonging to a group (i.e. the trait).
# - they are structural types: i.e. they needn't be declared explicitly

export istrait, issubtrait, traitcheck, traitgetsuper, traitgetpara, traitmethods, @traitdef, @traitfn, TraitException

if !(VERSION>v"0.4-")
    error("Traits.jl needs Julia version 0.4.-")
end

# Let all traits be direct decedents of Trait.  The type parameter
# SUPER of Trait is needed to specify super-traits.
abstract Trait{SUPER}
# SUPER - is a tuple of required super traits

# Use as return when types are not part of a trait:
immutable NoTrait <: Trait end

# General exception
type TraitException <: Exception 
    msg::String
end

# tests whether a DataType is a trait.  (But only traitcheck checks
# whether it's actually full-filled)
istrait(x) = false
istrait(x::Type{NoTrait}) = false
istrait{T<:Trait}(x::Type{T}) = true
istrait(x::Tuple) = mapreduce(istrait, &, x)

# a concrete trait type has the form 
## Tr{X,Y,Z} <: Trait{(ST1{X,Y},ST2{Z})}
# 
# immutable Tr{X,Y,Z} <: Trait{(ST1{X,Y},ST2{Z})}
#   fns
#   Tr() = new(fns_made_in_macro)
# end
#
# where fns holds the function signatures.

# A Trait Tr is defined for some parameters if 
traitcheck{T<:NoTrait}(Tr::Type{T}; tmp...) = false
function traitcheck{T<:Trait}(Tr::Type{T}; verbose=false)
    # check supertraits
    traitcheck(traitgetsuper(Tr); verbose=verbose) || return false
    # check definitions
    for (fn,sig) in Tr().fns
        checks = length(methods(fn, sig[1]))>0
        if !checks
            if verbose
                println("Function $fn with signature $sig not defined for $T")
            end
            return false
        end
    end
    return true
end
# check a tuple of traits against a signature
function traitcheck(Trs::Tuple; verbose=false)
    for Tr in Trs
        traitcheck(Tr; verbose=verbose) || return false
    end
    return true
end

traitassert(Tr::Trait) = @assert traitcheck(Tr)

traitgetsuper{T<:Trait}(t::Type{T}) =  t.super.parameters[1]::Tuple
traitgetpara{T<:Trait}(t::Type{T}) =  t.parameters

function issubtrait{T1<:Trait,T2<:Trait}(t1::Type{T1}, t2::Type{T2})
    if t2 in traitgetsuper(t1)
        return true
    end
    for t in traitgetsuper(t1)
        issubtrait(t, t2) && return true
    end
    return false
end

## Trait definition
include("traitdef.jl")

# Trait implementation
include("traitimpl.jl") # TODO: not implemented yet

# Trait functions
include("traitfns.jl")

## Common traits
include("commontraits.jl")

end # module
