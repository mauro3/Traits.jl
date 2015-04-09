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

## patches for bugs in base
include("base_fixes.jl")

## common helper functions
include("helpers.jl")

#######
# Flags
#######
# By setting them in Main before using, they can be turned on or off.
# TODO: update to use functions.
if isdefined(Main, :Traits_check_return_types)
    println("Traits.jl: not using return types of @traitdef functions")
    flag_check_return_types = Main.Traits_check_return_types
else
    flag_check_return_types = true
end
@doc "Flag to select whether return types in @traitdef's are checked" flag_check_return_types

#######
# Types
#######
@doc """`abstract Trait{SUPER}`

     All traits are direct decedents of abstract type Trait.  The type parameter
     SUPER of Trait is needed to specify super-traits (a tuple).""" ->
abstract Trait{SUPER}

# Type of methods field of concrete traits:
typealias FDict Dict{Union(Function,DataType),Function}

# A concrete trait type has the form 
## Tr{X,Y,Z} <: Trait{(ST1{X,Y},ST2{Z})}
# 
# immutable Tr1{X1} <: Traits.Trait{()}
#     methods::FDict
#     constraints::Vector{Bool}
#     assoctyps::Vector{Any}
#     Tr1() = new(FDict(methods_defined), Bool[], [])
# end
#
# where methods field holds the function signatures, like so:
# Dict{Function,Any} with 3 entries:
#   start => _start(Int64) = (Any...,)
#   next  => _next(Int64,Any) = (Any...,)
#   done  => _done(Int64,Any) = Bool

# used to dispatch to helper methods
immutable _TraitDispatch end
immutable _TraitStorage end

# @doc """Type All is to denote that any type goes in type signatures in
#      @traitdef.  This is a bit awkward:

#      - method_exists(f, s) returns true if there is a method of f with
#        signature sig such that s<:sig.  Thus All<->Union()
#      - Base.return_types works the other way around, there All<->Any

#      See also https://github.com/JuliaLang/julia/issues/8974"""->
# abstract All

# General trait exception
type TraitException <: Exception 
    msg::String
end

# A helper type used in istrait below
immutable _TestType{T} end

#########
# istrait, one of the core functions
#########

# Update after PR #10380
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
    println_verb = verbose ? println : x->x

    if !hasparameters(Tr)
        throw(TraitException("Trait $Tr has no type parameters."))
    end
    # check supertraits
    !istrait(traitgetsuper(Tr); verbose=verbose) && return false

    # check instantiating
    tr = nothing
    try 
        tr = Tr()
    catch err
        println_verb("""Could not instantiate instance for type encoding the trait $Tr.  
                     This usually indicates that something is amiss with the @traitdef
                     or that one of the generic functions is not defined.
                     The error was: $err""")
        return false
    end

    # check constraints
    if !all(tr.constraints)
        println_verb("Not all constraints are satisfied for $T")
        return false
    end

    # check call signature of methods:
    for (gf,_gf) in tr.methods # loop over all generic functions in traitdef
        for tm in methods(_gf) # loop over all methods defined for each function in traitdef
            checks = false
            for fm in methods(gf, NTuple{length(tm.sig),Any}) # only loop over methods which have
                                                              # the right number of arguments
                if isfitting(tm, fm, verbose=verbose)
                    checks = true
                    break
                end
            end
            if !checks # if check==false no fitting method was found
                println_verb("""No method of the generic function $gf matched the 
                             trait specification: $tm""")
                return false
            end
        end
    end

    # for (meth,sig) in tr
    #     # instead of:
    #     ## checks = length(methods(meth, sig[1]))>0
    #     # Now using method_exists.  But see bug
    #     # https://github.com/JuliaLang/julia/issues/8959

    #     sigg = map(x->x===All ? Union() : x, sig[1])
    #     if isa(meth, Function)
    #         if !method_exists(meth, sigg) # I think this does the right thing.
    #             println_verb("Method $meth with call signature $(sig[1]) not defined for $T")
    #             checks = false
    #         end
    #     elseif isa(meth, DataType) # a constructor, presumably.
    #         # But discard the catch all to convert, i.e. this means
    #         # method_exists(call, (Type{meth}, sigg...))==true for all types
    #         chatch_all =  methods(call, (Type{Array},))
    #         if methods(call, tuple(Type{meth}, sigg...))==chatch_all
    #             println_verb("Datatype constructor $meth with call signature $sigg not defined for trait $T")
    #             checks = false
    #         end
    #     else
    #         throw(TraitException("Trait $Tr contains a funny entry in its method dictionary: $meth."))
    #     end
    # end
    
    # check return-type
    if flag_check_return_types
        for (gf,_gf) in tr.methods
            for tm in methods(_gf) # loop over all methods defined for each function in traitdef
                @show tret_typ = Base.return_types(_gf, tm.sig)
                if length(tret_typ)!=1
                    throw(TraitException("Querying the return type of the trait-method $tm did not return exactly one return type: $tret_typ"))
                end
                tret_typ = tret_typ[1]
                @show fret_typ = Base.return_types(gf, tm.sig)
                for fr in fret_typ
                    if !(fr<:tret_typ)
                        println_verb("")
                        return false
                    end
                end
            # # replace All in sig[1] with Any
            # sigg = map(x->x===All ? Any : x, sig[1])
            # tmp = Base.return_types(meth, sigg)
            # if length(tmp)==0
            #     rettype = []
            #     checks = false
            #     println_verb("Method `$meth` with signature $sigg->$(sig[2]) has an empty return signature!")
            # else#if length(tmp)==1
            #     rettype = tmp[1]
            #     if !(rettype<:sig[2])
            #         checks = false
            #         println_verb("Method `$meth` with signature $sigg->$(sig[2]) has wrong return type: $rettype")
            #     end
            # # else
            # #     checks = false
            # #     if verbose
            # #         println("Method `$meth` with signature $sigg->$(sig[2]) has more than one return type!")
            # #     end
                # end
            end
        end
    end
    return true
end
# check a tuple of traits against a signature
function istrait(Trs::Tuple; verbose=false)
    for Tr in Trs
        istrait(Tr; verbose=verbose) || return false
    end
    return true
end

## Helpers for istrait
@doc """isfitting checks whether a method `tm` specified in the trait definition 
     is fulfilled by a method `fm` of the corresponding generic function.  One of the
     core functions of istraits.

     Checks that tm.sig<:fm.sig and that the parametric constraints on
     fm and tm are equal.  Lets call this relation tm<<:fm.

     So, summarizing, for a trait-signature to be satisfied (fitting) the following
     condition need to hold:
     A) `tsig<:sig` for just the types themselves (sans parametric constraints)
     B) The constraints on `sig` and `tsig` need to be equal.

     Examples, left trait-method, right implementation-method:
     {T<:Real, S}(a::T, b::Array{T,1}, c::S, d::S) <<: {T<:Number, S}(a::T, b::AbstractArray{T,1}, c::S, d::S)
     -> true

     {T<:Integer}(T, T, Integer) <<: {T<:Integer}(T, T, T)
     -> false as parametric constraints are not equal
     """ ->
function isfitting(tm::Method, fm::Method; verbose=false) # tm=trait-method, fm=function-method
    println_verb = verbose ? println : x->x

    # No Vararg methods implement yet
    if tm.va || fm.va
        println_verb("Vararg methods not currently supported.  Returning false.")
        return false
    end
    ## Check condition A:
    # If there are no type-vars then just compare the signatures:
    if tm.tvars==()
        if !(fm.tvars==())
            # If there are parameter constraints affecting more than
            # one argument, then return false.
            fmtvars = isa(fm.tvars,TypeVar) ? (fm.tvars,) : fm.tvars
            for ftv in fmtvars
                if sum(find_tvar(fm.sig, ftv))>1
                    println_verb("Reason fail: no tvars-constraints in trait-method but in function-method.")
                    return false
                end
            end
        end
        println_verb("Reason fail/pass: no tvars in trait-method")
        return tm.sig<:fm.sig
    end
    # If !(tm.sig<:fm.sig) then tm<<:fm is false
    # but the converse is not true:
    if !(tm.sig<:fm.sig)
        println_verb("Reason fail: !(tm.sig<:fm.sig)")
        return false
    end
    # False if there are not the same number of arguments: (I don't
    # think this test is necessary as it is tested above.)
    if length(tm.sig)!=length(fm.sig)!
        println_verb("Reason fail: wrong length")
        return false
    end
    # Getting to here means that that condition (A) is fulfilled.

    ## Check condition B:
    # If there is only one argument then we're done as parametric
    # constraints play no role:
    if length(tm.sig)==1
        println_verb("Reason pass: length(tm.sig)==1")
        return true
    end

    # Strategy: go through constraints on trait-method and check
    # whether they are fulfilled in function-method.
    tmtvars = isa(tm.tvars,Tuple) ? tm.tvars : (tm.tvars,)
    tvars = isa(tm.tvars,TypeVar) ? (tm.tvars,) : tm.tvars
    for tv in tvars
        # find all occurrences in the signature
        locs = find_tvar(tm.sig, tv)
        if !any(locs)
            error("Bad: the type variable should feature in at least on location.")
        end
        # Find the tvar in fm which corresponds to tv.  It's ok if ftv
        # constrains more arguments than tv!
        ftvs = Any[]
        fmtvars = isa(fm.tvars,TypeVar) ? (fm.tvars,) : fm.tvars
        for ftv in fmtvars
            flocs = find_tvar(fm.sig, ftv)
            if all(flocs[find(locs)])
                push!(ftvs,ftv)
            end
        end
        if ftvs==Any[]
            println_verb("Reason fail: parametric constraints on function method not as severe as on trait-method.")
            return false
        end
        if length(ftvs)>1
            error("""Not supported if two or more TypeVar appear in the same arguments.
                  Example f{K,V}(::Dict{K,V}, ::Dict{V,K})""")
        end
        
        # Check that they constrain the same thing in each argument.
        # E.g. this should fail: {K,V}(::Dict{K,V}, T) <<: {T}(::Dict{V,K}, T).
        # Do this by substituting a concrete type into the respective
        # TypeVars and check that arg(tv')<:arg(ftv')
        for i in find(locs)
            targ = subs_tvar(tv,      tm.sig[i], _TestType{i})
            farg = subs_tvar(ftvs[1], fm.sig[i], _TestType{i})
            if !(targ<:farg)
                println_verb("Reason fail: parametric constraints on args $(tm.sig[i]) and $(fm.sig[i]) on different TypeVar locations!")
                return false
            end
        end
    end

    println_verb("Reason pass: all checks passed")
    return true
end

# helpers for isfitting
function subs_tvar{T<:_TestType}(tv::TypeVar, arg, TestT::Type{T})
    # Substitute `TestT` for a particular TypeVar `tv` in an argument `arg`.
    #
    # Example:
    # Array{I<:Int64,N} -> Array{_TestType{23},N}
    if isa(arg, DataType) && (isleaftype(arg) || length(arg.parameters)==0) # concrete type or abstract type with no parameters
        return arg
    elseif isa(arg,TypeVar)
        if tv===arg  # note === this it essential!
            return TestT # replace
        else
            return arg
        end
    else # It's a parameterized type do substitution on all parameters:
        pa = [ subs_tvar(tv, arg.parameters[i], TestT) for i=1:length(arg.parameters) ]
        typ = deparameterize_type(arg)
        return typ{pa...}
    end
end

# find_tvar finds index of arguments in a function signature `sig` where a
# particular TypeVar `tv` features. Example:
#
# find_tvar( (T, Int, Array{T}) -> [true, false, true]
function find_tvar(sig::Tuple, tv)
    ns = length(sig)
    out = falses(ns)
    for i = 1:ns
        out[i] = any(find_tvar(sig[i], tv))
    end
    return out
end
find_tvar(sig::TypeVar, tv) = sig===tv ? [true] : [false]   # note ===, this it essential!
function find_tvar(sig::DataType, tv)
    isleaftype(sig) && return [false]
    ns = length(sig.parameters)
    out = false
    for i=1:ns
        out = out || any(find_tvar(sig.parameters[i], tv))
    end
    return [out]
end
find_tvar(sig, tv) = [false]

######################
# Sub and supertraits:
######################
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

## Trait definition
include("traitdef.jl")

# Trait implementation
include("traitimpl.jl")

# Trait functions
include("traitfns.jl")

## Common traits
include("commontraits.jl")

end # module
