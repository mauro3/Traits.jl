module Traits
# Traits are
# - contracts on one type or between several types.  The contract can
#   contain required methods but also other assertions and just
#   belonging to a group (i.e. the trait).
# - they are structural types: i.e. they needn't be declared explicitly

export istrait, istraittype, issubtrait,
       traitgetsuper, traitgetpara, traitmethods, 
       @traitdef, @traitimpl, @traitfn, TraitException

if !(VERSION>v"0.4-")
    error("Traits.jl needs Julia version 0.4.-")
end

# Let all traits be direct decedents of Trait.  The type parameter
# SUPER of Trait is needed to specify super-traits.
abstract Trait{SUPER}
# SUPER - is a tuple of required super traits

# A concrete trait type has the form 
## Tr{X,Y,Z} <: Trait{(ST1{X,Y},ST2{Z})}
# 
# immutable Tr{X,Y,Z} <: Trait{(ST1{X,Y},ST2{Z})}
#   methods
#   Tr() = new(methods_made_in_macro)
# end
#
# where methods holds the function signatures, like so:
# Dict{Function,Any} with 3 entries:
#   next  => ((Int64,Any),(Any...,))
#   done  => ((Int64,Any),(Bool,))
#   start => ((Int64,),(Any...,))

# used to dispatch to helper methods
immutable _TraitDispatch  end
immutable _TraitStorage end

# General trait exception
type TraitException <: Exception 
    msg::String
end

# tests whether a DataType is a trait.  (But only istrait checks
# whether it's actually full-filled)
istraittype(x) = false
istraittype{T<:Trait}(x::Type{T}) = true
istraittype(x::Tuple) = mapreduce(istraittype, &, x)

# A Trait Tr is defined for some parameters if
function istrait{T<:Trait}(Tr::Type{T}; verbose=false)
    # check supertraits
    istrait(traitgetsuper(Tr); verbose=verbose) || return false
    # check methods definitions
    try 
        Tr()
    catch
        if verbose
            println("""Not all generic functions of trait $Tr are defined.  
                       Define them before using $Tr""")
        end
        return false
    end
    out = true
    for (meth,sig) in Tr().methods
        checks = length(methods(meth, sig[1]))>0
        if !checks
            if verbose
                println("Method $meth with signature $sig not defined for $T")
            end
            out = false
        end
    end
    return out
end
# check a tuple of traits against a signature
function istrait(Trs::Tuple; verbose=false)
    for Tr in Trs
        istrait(Tr; verbose=verbose) || return false
    end
    return true
end

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

## common helper functions
include("helpers.jl")

## Trait definition
include("traitdef.jl")

# Trait implementation
include("traitimpl.jl")

# Trait functions
include("traitfns.jl")

## Common traits
include("commontraits.jl")

end # module
