module Traits
@doc """This package provides an implementation of traits, aka interfaces or type-classes.
     It is based on the premises that traits are:

     - contracts on one type or between several types.  The contract can
        contain required methods but also other assertions and just
        belonging to a group (i.e. the trait).
     - they are structural types: i.e. they needn't be declared explicitly
        """ -> current_module()

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
@doc "Flag to select whether return types in @traitdef's are checked" flag_check_return_types

@doc """`abstract Trait{SUPER}`

         All traits are direct decedents of abstract type Trait.  The type parameter
         SUPER of Trait is needed to specify super-traits (a tuple).""" ->
abstract Trait{SUPER}

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

@doc """Type All is to denote that any type goes in type signatures in
    @traitdef.  This is a bit awkward:

    - method_exists(f, s) returns true if there is a method of f with
      signature sig such that s<:sig.  Thus All<->Union()
    - Base.return_types works the other way around, there All<->Any

    See also https://github.com/JuliaLang/julia/issues/8974"""->
abstract All

# General trait exception
type TraitException <: Exception
    msg::String
end

@doc """Tests whether a DataType is a trait.  (But only istrait checks
        whether it's actually full-filled)""" ->
istraittype(x) = false
istraittype{T<:Trait}(x::Type{T}) = true
istraittype(x::Tuple) = mapreduce(istraittype, &, x)

@doc """Tests whether a set of types fulfill a trait.
        A Trait Tr is defined for some parameters if:

        - all the functions of a trait are defined for them
        - all the trait constraints are fulfilled

        Example:

        `istrait(Tr{Int, Float64})`

        or with a tuple of traits:

        `istrait( (Tr1{Int, Float64}, Tr2{Int}) )`
            """ ->
function istrait{T<:Trait}(Tr::Type{T}; verbose=false)
    if !hasparameters(Tr)
        throw(TraitException("Trait $Tr has no type parameters."))
    end
    # check supertraits
    !istrait(traitgetsuper(Tr); verbose=verbose) && return false
    # check methods definitions
    local tr::T
    try
        tr=Tr()
    catch
        if verbose
            println("""Not all generic functions of trait $Tr are defined.
                       Define them before using $Tr""")
        end
        return false
    end
    out = true
    function testanytypevars( at )
        if typeof(at) == TypeVar
            return true
        elseif typeof(at) == DataType
            return( any( y->typeof(y) == TypeVar, at.parameters ) )
        elseif typeof(at) <: Tuple
            for sat in at
                if testanytypevars(sat)
                    return true
                end
            end
            return false
        else
            println( "unknown type in signature " * string( typeof( at ) ) * " val: " * string(at) )
        end
    end
    anytypevars = false
    # check call signature of methods:
    for (meth,sig) in tr.methods
        # instead of:
        ## checks = length(methods(meth, sig[1]))>0
        # Now using method_exists.  But see bug
        # https://github.com/JuliaLang/julia/issues/8959

        sigg = map(x->x===All ? Union() : x, sig[1])
        anytypevars = testanytypevars( sig[1] )

        if isa(meth, Function)
            out = !anytypevars ? method_exists(meth,sigg) : method_exists_tvars( meth,sigg,verbose )
            if !out && verbose
                println("Method $meth with call signature $(sig[1]) not defined for $T")
            end
        elseif isa(meth, DataType) # a constructor, presumably.
            # But discard the catch all to convert, i.e. this means
            # method_exists(call, (Type{meth}, sigg...))==true for all types
            chatch_all =  methods(call, (Type{Array},))
            if methods(call, (Type{meth}, sigg...))==chatch_all
                if verbose
                    println("Datatype constructor $meth with signature $sig not defined for $T")
                end
                out = false
            end
        else
            throw(TraitException("Trait $Tr has something funny in its method dictionary: $meth."))
        end
    end
    # check return-type
    # unfortunately if the sig has TypeVar in them it doesn't seem possible to check
    # the return type
    if !anytypevars && flag_check_return_types && out # only check if all methods were defined
        for (meth,sig) in tr.methods
            # replace All in sig[1] with Any
            sigg = map(x->x===All ? Any : x, sig[1])
            tmp = Base.return_types(meth, sigg)
            if length(tmp)==0
                rettype = []
                out = false
                if verbose
                    println("Method `$meth` with signature $sigg->$(sig[2]) has an empty return signature!")
                end
            else#if length(tmp)==1
                rettype = tmp[1]
                if !(rettype<:sig[2])
                    out = false
                    if verbose
                        println("Method `$meth` with signature $sigg->$(sig[2]) has wrong return type: $rettype")
                    end
                end
            # else
            #     out = false
            #     if verbose
            #         println("Method `$meth` with signature $sigg->$(sig[2]) has more than one return type!")
            #     end
            end
        end
    end
    # check constraints
    if !all(tr.constraints)
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

@doc """Returns the super traits""" ->
traitgetsuper{T<:Trait}(t::Type{T}) =  t.super.parameters[1]::Tuple
traitgetpara{T<:Trait}(t::Type{T}) =  t.parameters

@doc """Checks whether a trait, or a tuple of them, is a subtrait of
        the second argument.""" ->
function issubtrait{T1<:Trait,T2<:Trait}(t1::Type{T1}, t2::Type{T2})
    if t1==t2
        return true
    end
    if t2 in traitgetsuper(t1)
        return true
    end
    for t in traitgetsuper(t1)
        issubtrait(t, t2) && return true
    end
    return false
end

# TODO: think about how to handle tuple traits and empty traits
function issubtrait{T1<:Trait}(t1::Type{T1}, t2::Tuple)
    if t2==()
        # the empty trait is the super-trait of all traits
        true
    else
        error("")
    end
end

# traits in a tuple have no order, really, this should reflected.
# Maybe use a set instead?  Subtrait if it is a subset?
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

## patches for bugs in base
include("base_fixes.jl")

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
