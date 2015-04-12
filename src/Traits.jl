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

# A helper dummy types used in istrait below
abstract _TestType{T}
immutable _TestTvar{T}<:_TestType{T} end # used for TypeVar testing
immutable _TestTraitPara{T}<:_TestType{T} end # used as dummy Trait parameters
immutable _TestAssoc{T}<:_TestType{T} end # used as dummy Associated types
Base.show{T<:_TestType}(io::IO, x::Type{T}) = print(io, string(x.parameters[1])*"_")

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

    # Need a trick to check parameterized methods, see isfitting for details:
    testtr = tr #make_testtrait(Tr)
    # Loop over all generic functions in traitdef:
    for ((gf,_gf), (tgf,_tgf)) in zip(tr.methods, testtr.methods)
        # Loop over all methods defined for each function in traitdef
        for (i,(tm,ttm)) in enumerate(zip(methods(_gf),methods(_tgf))) 
            checks = false
            # Only loop over methods which have the right number of arguments:
            for fm in methods(gf, NTuple{length(tm.sig),Any}) 
                if isfitting(tm, fm, ttm, verbose=verbose)
                    checks = true
                    break
                end
            end
            if !checks # if check==false no fitting method was found
                println_verb("""No method of the generic function/call-overloaded $gf matched the 
                             trait specification: $tm""")
                return false
            end
        end
    end

    # check return-type.  Specifed return type tret and return-type of
    # the methods frets should fret<:tret.  This is backwards to
    # argument types...
        if flag_check_return_types
        for (gf,_gf) in tr.methods
            for tm in methods(_gf) # loop over all methods defined for each function in traitdef
                @show tret_typ = Base.return_types(_gf, tm.sig) # trait-defined return type
                if length(tret_typ)!=1
                    throw(TraitException("Querying the return type of the trait-method $tm did not return exactly one return type: $tret_typ"))
                end
                tret_typ = tret_typ[1]
                @show fret_typ = Base.return_types(gf, tm.sig)
                # at least one of the return types need to be a subtype of tret_typ
                checks = false
                for fr in fret_typ
                    if fr<:tret_typ
                        checks = true
                    end
                end
                if !checks
                    println_verb("""For function $gf: no return types found which are subtypes of the specified return type:
                                 $tret_typ
                                 List of found return types:
                                 $fret_typ
                                 """)
                    return false
                end
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

@doc """make_testtrait makes a trait containing test types instead of
     'real' types for the trait-parameters and the associated types.""" ->
function make_testtrait{T<:Trait}(Tr::Type{T})
    TTr = deparameterize_type(Tr)
    TestTypes = [_TestTraitPara{TTr.parameters[i].name} for i=1:length(TTr.parameters) ]
    testtr = TTr{TestTypes...}(_TestTraitPara) # note the _TestTraitPara argument
end

immutable FakeMethod
    sig::(Any...,)
    tvars::(Any...,)
    va::Bool
end
@doc """isfitting checks whether a method `tm` specified in the trait definition 
     is fulfilled by a method `fm` of the corresponding generic function.  The
     core function called by istraits.

     Checks that tm.sig<:fm.sig and that the parametric constraints on
     fm and tm are equal.  Lets call this relation tm<<:fm.

     So, summarizing, for a trait-signature to be satisfied (fitting) the following
     condition need to hold:
     A) `tsig<:sig` for just the types themselves (sans parametric constraints)
     B) The parametric constraints on `sig` and `tsig` need to be equal.

     Examples, left trait-method, right implementation-method:
     {T<:Real, S}(a::T, b::Array{T,1}, c::S, d::S) <<: {T<:Number, S}(a::T, b::AbstractArray{T,1}, c::S, d::S)
     -> true

     {T<:Integer}(T, T, Integer) <<: {T<:Integer}(T, T, T)
     -> false as parametric constraints are not equal
     """ ->
function isfitting(tmm::Method, fm::Method, testtm::Method; verbose=false) # tm=trait-method, fm=function-method
    println_verb = verbose ? println : x->x

    # Make a "copy" of tmm as it may get updated:
    tm = FakeMethod(tmm.sig, tmm.tvars, tmm.va)

    # Special casing for call-overloading. 
    if fm.func.code.name==:call && tmm.func.code.name!=:call # true if only fm is call-overloaded
        # prepend ::Type{...} to signature
        tm = FakeMethod(tuple(fm.sig[1], tm.sig...), tm.tvars, tm.va)
        # check whether there are method parameters too:
        fmtvars = isa(fm.tvars,TypeVar) ? (fm.tvars,) : fm.tvars # make sure it's a tuple of TypeVars
        for ftv in fmtvars
            flocs = find_tvar(fm.sig, ftv)
            if flocs[1] # yep, has a constraint like call{T}(::Type{Array{T}},...)
                if sum(flocs)==1
                    tm = FakeMethod(tm.sig, tuple(ftv, tm.tvars...) , tm.va)
                else
                    println_verb("This check is not implemented, returning false.")
                    # Note that less than none of the 1000 methods
                    # of call in Base end up here.
                    return false
                end
            end
        end
        # There is a strange bug which is prevented by this never
        # executing @show.  I'll try and investigate this in branch
        # m3/heisenbug
        if length(tm.sig)==-10
            @show tm
            error("This is not possible")
        end
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
        println_verb("Reason fail/pass: no tvars in trait-method. Result: $(tm.sig<:fm.sig)")
        return tm.sig<:fm.sig
    end
    # If !(tm.sig<:fm.sig) then tm<<:fm is false
    # but the converse is not true:
    if !(tm.sig<:fm.sig)
        @show typeof(tm.sig[1])
        println_verb("""Reason fail: !(tm.sig<:fm.sig)
                     tm.sig = $(tm.sig)
                     fm.sig = $(fm.sig)""")
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

    # # No Vararg methods with type constraints implement yet
    # if tm.va || fm.va
    #     # runtests.jl flag: varag_not_supported_bug
    #     #
    #     # What do varargs mean?
    #     println_verb("Vararg methods not currently supported.  Returning false.")
    #     return false
    # end


    # Strategy: go through constraints on trait-method and check
    # whether they are fulfilled in function-method.
    tmtvars = isa(tm.tvars,Tuple) ? tm.tvars : (tm.tvars,)
    tvars = isa(tm.tvars,TypeVar) ? (tm.tvars,) : tm.tvars
    # However, if one of the tm.tvars is a leaftype, then relax
    # requirement on fm.tvars to be of the same specific type.  This
    # is done using testtm.
    testtvars = isa(testtm.tvars,TypeVar) ? (testtm.tvars,) : testtm.tvars
    for (i,tv) in enumerate(tvars)
        # find all occurrences in the signature
        locs = find_tvar(tm.sig, tv)
        if !any(locs)
            throw(TraitException("The type variable should feature in at least on location."))
        end
        # Find the tvar in fm which corresponds to tv. 
        ftvs = Any[]
        fmtvars = isa(fm.tvars,TypeVar) ? (fm.tvars,) : fm.tvars # make sure it's a tuple of TypeVar
        for ftv in fmtvars
            flocs = find_tvar(fm.sig, ftv)
            if all(flocs[find(locs)])
                push!(ftvs,ftv)
            end
        end

        if length(ftvs)==0
            @show tv, locs
            ## This should pass, because the trait-parameter is a leaftype:
            # @traitdef Tr01{X} begin
            #     g01{T<:X}(T, T) -> T
            # end
            # g01(::Int, ::Int) = Int
            # @assert istrait(Tr01{Int}, verbose=true)

            # find X
            @show isleaftype(tv.ub)
            if isleaftype(tv.ub)
                @show ltype = tv.ub
                # Now check if the method definition of fm
                # has the same leaftypes in the same location.
                @show fm.sig[locs]
                @show mapreduce(x -> x==ltype, &, true, fm.sig[locs])
                if mapreduce(x -> x==ltype, &, true, fm.sig[locs])
                    println_verb("Reason pass: parametric constraints only on leaftypes.")
                    return true
                end
            end
            println_verb("Reason fail: parametric constraints on function method not as severe as on trait-method.")
            return false
        end
        if length(ftvs)>1
            # TODO: this should be able to pass
            throw(TraitException("""Not supported yet if two or more TypeVar appear in the same arguments.
                  Example f{K,V}(::Dict{K,V}, ::Dict{V,K})"""))
        end
        
        # Check that they constrain the same thing in each argument.
        # E.g. this should fail: {K,V}(::Dict{K,V}, T) <<: {T}(::Dict{V,K}, T).
        # Do this by substituting a concrete type into the respective
        # TypeVars and check that arg(tv')<:arg(ftv')
        for i in find(locs)
            targ = subs_tvar(tv,      tm.sig[i], _TestTvar{i})
            farg = subs_tvar(ftvs[1], fm.sig[i], _TestTvar{i})
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
function subs_tvar{T<:_TestTvar}(tv::TypeVar, arg::DataType, TestT::Type{T})
    # Substitute `TestT` for a particular TypeVar `tv` in an argument `arg`.
    #
    # Example:
    # Array{I<:Int64,N} -> Array{_TestTvar{23},N}
    if isleaftype(arg) || length(arg.parameters)==0 # concrete type or abstract type with no parameters
        return arg
    else # It's a parameterized type: do substitution on all parameters:
        pa = [ subs_tvar(tv, arg.parameters[i], TestT) for i=1:length(arg.parameters) ]
        typ = deparameterize_type(arg)
        return typ{pa...}
    end
end
subs_tvar{T<:_TestTvar}(tv::TypeVar, arg::TypeVar, TestT::Type{T}) = tv===arg ? TestT : arg  # note === this it essential!
subs_tvar{T<:_TestTvar}(tv::TypeVar, arg, TestT::Type{T}) = arg # for anything else

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
        throw(TraitException(""))
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
