module Traits
# Traits are
# - contracts on one type or between several types.  The contract can
#   contain required methods but also other assertions and just
#   belonging to a group (i.e. the trait).
# - they are structural types: i.e. they needn't be declared explicitly

export istrait, istraittype, issubtrait,
       traitgetsuper, traitgetpara, traitmethods, 
       @traitdef, @traitimpl, @traitfn, TraitException, All

if !(VERSION>v"0.4-")
    error("Traits.jl needs Julia version 0.4.-")
end

# Flags: by setting them in Main before using, they can be turned on
# or off.
if isdefined(Main, :Traits_check_return_types)
    println("Traits.jl: not using return types of @traitdef functions")
    flag_check_return_types = Main.Traits_check_return_types
else
    flag_check_return_types = true
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

# Type All is to denote that any type goes in type signatures.  This
# is a bit awkward:
# - method_exists(f, s) returns true if there is a method of f with
#   signature sig such that s<:sig.  Thus All<->Union()
# - Base.return_types works the other way around, there All<->Any
abstract All

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
# istrait(UU{TT})==true => for all T such that T<:TT => istrait(UU{T})==true
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
    # check call signature of methods:
    for (meth,sig) in Tr().methods
        # instead of:
        ## checks = length(methods(meth, sig[1]))>0
        # Now using method_exists.  But see bug
        # https://github.com/JuliaLang/julia/issues/8959

        sigg = map(x->x===All ? Union() : x, sig[1])
        if !method_exists(meth, sigg) # I think this does the right thing.
            if verbose
                println("Method $meth with signature $sig not defined for $T")
            end
            out = false
        end
    end
    # check return-type
    if flag_check_return_types && out # only check if all methods were defined
        for (meth,sig) in Tr().methods
            # replace All in sig[1] with Any
            sigg = map(x->x===All ? Any : x, sig[1])
            tmp = Base.return_types(meth, sigg)
            if length(tmp)==0
                rettype = []
                out = false
                if verbose
                    println("Method `$meth` with signature $sigg->$(sig[2]) has an empty return signature!")
                end
            else
                rettype = tmp[1]
                if !isa(rettype, Tuple)
                    rettype = (rettype,)
                end
                if !(rettype<:sig[2])
                    out = false
                    if verbose
                        println("Method `$meth` with signature $sigg->$(sig[2]) has wrong return type: $rettype")
                    end
                end
            end
        end
    end
    # check constraints
    if !all(Tr().constraints)
        if verbose
            println("Not all constraints are satisfied for $T")
        end
        return false
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

function issubtrait{T1<:Trait}(t1::Type{T1}, t2::Tuple)
    if t2==()
        # the empty trait is the super-trait of all traits
        true
    else
        error("")
    end
end

function issubtrait(t1::Tuple, t2::Tuple)
    if length(t1)!=length(t2)
        return false
    end
    checks = true
    for (p1,p2) in zip(t1, t2)
        checks = checks && issubtrait(p1,p2)
    end
    return checks
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
