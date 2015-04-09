# # This uses methods to encode the methods...


# @traitdef Pr2{X} begin
#     fn77{Y<:Number}(X,Y,Y) -> Y
# end


# using Traits
# immutable Pr2{X} <: Traits.Trait{()} # /home/mauro/.julia/v0.4/Traits/src/traitdef.jl, line 277:
#     methods::Dict{Union(Function,DataType),Function} # line 278:
#     constraints::Vector{Bool} # line 279:
#     assoctyps::Vector{Any} # line 280:
#     function Pr2() # /home/mauro/.julia/v0.4/Traits/src/traitdef.jl, line 281:
#         assoctyps = Any[]
#         _fn77{Y<:Number}(::X,::Y,::Y) = Y
#         _fn77{Y<:Number}(::Type{X},::Type{Y},::Type{Y}) = Y # arithmetic on types: https://github.com/JuliaLang/julia/issues/8027#issuecomment-52519612
#         new(Dict(
#                  fn77 => _fn77
#                  ),
#             Bool[],
#             assoctyps)
#     end
# end

# fn77(a::Array,b::Int, c::Float64) = a[1] # this is no good
# p2 = Pr2{Array}()

# # # checks full-filled
# # function istrait_{T<:Traits.Trait}(t::Type{T})
# #     t = t()
# #     check_meths = methods(t.methods[fn77], (Array,Any...))  # how to construct (Array,Any...)?
# #     # assume just one for now (but https://github.com/mauro3/Traits.jl/issues/8)
# #     cm = collect(check_meths)[1]
# #     checks = false
# #     @show cm.sig
# #     for m in methods(fn77, (Array,Any...))
# #         @show m.sig
# #         if m.sig==cm.sig # too restrictive and m.sig<:cm.sig is too permissive
# #             checks = true
# #             break
# #         end
# #     end
# #     checks
# # end
# # @show istrait_(Pr2{Array})

# # # add good method
# # fn77(a::Array,b::Int, c::Int) = a[1] # this is good
# # @show istrait_(Pr2{Array}) # does not work...:
# # # julia> istrait_(Pr2{Array}) # does not work...:
# # # cm.sig = (Array{T,N},Y<:Number,Y<:Number)
# # # m.sig = (Array{T,N},Int64,Float64)
# # # m.sig = (Array{T,N},Int64,Int64)
# # # false

# ## # checks full-filled
# function istrait_v2{T<:Traits.Trait}(t::Type{T})
#     # Need to check for each method mt in t.methods whether at least
#     # one m.sig<:mt.sig, where m runs over all methods of the generic
#     # function in question.
    
#     X = t.parameters[1]
#     t = t()
#     # assume just one for now (but https://github.com/mauro3/Traits.jl/issues/8)


#     check_meths = methods(t.methods[fn77], (X,Any...))  # how to construct (Array,Any...)?
#     # assume just one method for now (but https://github.com/mauro3/Traits.jl/issues/8)
#     cm = collect(check_meths)[1]

#     checks = false
#     ret_typ = ()
#     for m in methods(fn77)
#         @show m.sig
#         if m.sig==cm.sig # too restrictive and m.sig<:cm.sig is too permissive
#             checks = true
#             break
#         end

#         try
#             @show applicable(t.methods[fn77], m.sig...)
#             @show ret_typ = t.methods[fn77](m.sig...)
#             checks = true
#             break
#         catch err
#             @show err
#         end
#     end
#     return checks, ret_typ
# end
# @show istrait_v2(Pr2{Array})

# # add good method
# fn77(a::Array,b::Int, c::Int) = a[1] # this is good
# istrait_v2(Pr2{Array}) 

# # try parametric method
# fn77{T<:Number}(a::String, b::T, c::T) = 5
# istrait_v2(Pr2{String})
# istrait_v2(Pr2{ASCIIString})  # doesn't work because of typevar

# fn77{S<:Integer}(a::S, b::Int, c::Int) = 5
# istrait_v2(Pr2{Integer})  # doesn't work because of typevar


# fn77{S<:Integer}(a::S, b::Int, c::Int) = 5
# istrait_v2(Pr2{Integer})  # doesn't work because of typevar

# # so what to do?



######## another try

using Traits

immutable TestType{T} end
# helpers for isfitting
function subs_tvar(tv::TypeVar, arg, TestT)
    # Substitute a particular TypeVar in an argument with a test-type.
    # Example:
    # Array{I<:Int64,N} -> Array{TestType{23},N}
    # println(" ")
    # @show (tv, arg, TestT)
    # @show isa(arg, DataType)
    # @show isleaftype(arg) 
    # @show isa(arg, DataType) && ( isleaftype(arg) || length(arg.parameters)==0 )
    # @show isa(arg,TypeVar)
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
@assert subs_tvar(TypeVar(:I), Array{TypeVar(:I,Int64)}, TestType{1})==Array{TypeVar(:I,Int64)}
@assert subs_tvar(TypeVar(:I,Int64), Array{TypeVar(:I,Int64)}, TestType{1})==Array{TestType{1}}
@assert subs_tvar(TypeVar(:T), Array, TestType{1})==Array{TestType{1}}  # this is kinda bad
f8576{T}(a::Array, b::T) = T
other_T = f8576.env.defs.tvars
@assert subs_tvar(other_T, Array, TestType{1})==Array # f8576.env.defs.tvars looks like TypeVar(:T) but is different!

function find_tvar(sig::Tuple, tv)
    # Finds index of arguments in a function signature where a
    # particular TypeVar features. Example:
    #
    # find_tvar( (T, Int, Array{T}) -> [true, false, true]
    @show "tuple", sig, tv
    ns = length(sig)
    out = falses(ns)
    for i = 1:ns
        out[i] = any(find_tvar(sig[i], tv))
    end
    return out
end
find_tvar(sig::TypeVar, tv) = (    @show "TypeVar", sig, tv; sig===tv ? [true] : [false])   # note === this it essential!
function find_tvar(sig::DataType, tv)
    @show "DataType", sig, tv
    isleaftype(sig) && return [false]
    ns = length(sig.parameters)
    out = false
    for i=1:ns
        out = out || any(find_tvar(sig.parameters[i], tv))
    end
    return [out]
end
find_tvar(sig, tv) = [false]

@assert find_tvar( (Array, ), TypeVar(:T))==[true]
@assert find_tvar( (Array, ), other_T)==[false]
@assert find_tvar( (Int, Array, ), TypeVar(:T))==[false,true]

## # checks full-filled
function isfitting(tm::Method, fm::Method; verbose=false) # tm=trait-method, fm=function-method
    # Checks tm.sig<:fm.sig and that the parametric constraints on fm
    # are tm are equal.  Lets call this relation tm<<:fm
    #
    # So, summarizing, for a trait-signature to be satisfied (fitting) the following
    # condition need to hold:
    # A) `tsig<:sig` for just the types themselves (sans parametric constraints)
    # B) The constraints on `sig` and `tsig` need to be equal.
    #
    # Examples, left trait-method, right implementation-method:
    # {T<:Real, S}(a::T, b::Array{T,1}, c::S, d::S) <<: {T<:Number, S}(a::T, b::AbstractArray{T,1}, c::S, d::S)
    # -> true
    #
    # {T<:Integer}(T, T, Integer) <<: {T<:Integer}(T, T, T)
    # -> false as parametric constraints are not equal

    printfn = verbose ? println : x->x

    # No Vararg methods
    if tm.va || fm.va
        warning("Vararg methods not currently supported.  Returning false.")
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
                    printfn("Reason fail: no tvars-constraints in trait-method but in function-method.")
                    return false
                end
            end
        end
        printfn("Reason fail/pass: no tvars in trait-method")
        return tm.sig<:fm.sig
    end
    # If !(tm.sig<:fm.sig) then tm<<:fm is false
    # but the converse is not true:
    if !(tm.sig<:fm.sig)
        printfn("Reason fail: !(tm.sig<:fm.sig)")
        return false
    end
    # False if there are not the same number of arguments: (I don't
    # think this test is necessary as it is tested above.)
    if length(tm.sig)!=length(fm.sig)!
        printfn("Reason fail: wrong length")
        return false
    end
    # Getting to here means that that condition (A) is fulfilled.

    ## Check condition B:
    # If there is only one argument then we're done as parametric
    # constraints play no role:
    if length(tm.sig)==1
        printfn("Reason pass: length(tm.sig)==1")
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
            printfn("Reason fail: parametric constraints on function method not as severe as on trait-method.")
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
            targ = subs_tvar(tv,      tm.sig[i], TestType{i})
            farg = subs_tvar(ftvs[1], fm.sig[i], TestType{i})
            if !(targ<:farg)
                printfn("Reason fail: parametric constraints on args $(tm.sig[i]) and $(fm.sig[i]) on different TypeVar locations!")
                return false
            end
        end
    end

    printfn("Reason pass: all checks passed")
    return true
end
    

function istrait_v3{T<:Traits.Trait}(Tr::Type{T}; verbose=false)
    # Need to check for each method mt in t.methods whether at least
    # one m.sig<:mt.sig, where m runs over all methods of the generic
    # function in question.
    t = 1
    try
        t = Tr()
    catch err
        println("Error occured when instatiating type: $err")
        return false
    end
    ret_typ = ()

    # check method signature
    checks = true
    for (gf,_gf) in t.methods # loop over all generic functions in traitdef
        @show gf
        checks = false
        for tm in methods(_gf) # loop over all methods defined for each function in traitdef
            checks = false
            for fm in methods(gf, NTuple{length(tm.sig),Any}) # only loop over methods which have
                                                             # the right number of arguments
                if isfitting(tm, fm, verbose=verbose)
                    checks = true
                    break
                end
            end
            if !checks
                return false
            end
        end
    end
    return checks
end



# @traitdef Pr2{X} begin
#     fn77{Y<:Number}(X,Y,Y) -> Y
# end

immutable Pr2{X} <: Traits.Trait{()} # /home/mauro/.julia/v0.4/Traits/src/traitdef.jl, line 277:
    methods::Dict{Union(Function,DataType),Function} # line 278:
    constraints::Vector{Bool} # line 279:
    assoctyps::Vector{Any} # line 280:
    function Pr2() # /home/mauro/.julia/v0.4/Traits/src/traitdef.jl, line 281:
        assoctyps = Any[]
        _fn77{Y<:Number}(::X,::Y,::Y) = Y # note, this will make separate generic functions for different X, which is good.
        # _fn77{Y<:Number}(::Type{X},::Type{Y},::Type{Y}) = Y
        #_fn77 = (X,Y,Z) -> 
        new(Dict(
                 fn77 => _fn77
                 ),
            Bool[],
            assoctyps)
    end
end

@show istrait_v3(Pr2{Array})

fn77(a::Array,b::Int, c::Int) = a[1] # this is no good, not general enough
@assert !istrait_v3(Pr2{Array}) 

# try parametric method
fn77{T<:Number}(a::String, b::T, c::T) = 5
@assert istrait_v3(Pr2{String})
@assert istrait_v3(Pr2{ASCIIString})

fn77{S<:Integer}(a::S, b::Int, c::Int) = 5  # this is no good, not general enough
@assert !istrait_v3(Pr2{Integer})  

fn77{S<:Integer, N}(a::S, b::N, c::N) = 5
@assert istrait_v3(Pr2{Integer})

# ############
# # Test cases for later
# ############
# using Base.Test

# bug_ret_type1 = true

# @traitdef Tr01{X} begin
#     g01{T<:X}(T, T) -> T
# end
# g01(::Int, ::Int) = Int
# @test istrait(Tr01{Int}) # == true
# @test_throws istrait(Tr01{Integer})
# g01{I<:Integer}(::I, ::I) = I
# @test istrait(Tr01{Integer}) # == true

# @traitdef Tr02{X} begin
#     g02{T<:X}(T, T) -> T
# end
# g02{I<:Integer}(::I, ::I) = Integer
# # By using Base.return_types it is not possible to figure out whether
# # the returned value is constrained or not by I:
# if bug_ret_type1
#     @test istrait(Tr02{Integer})
#     # or throw an error/warning here saying parametric return types
#     # are only supported for leaftypes
# else
#     @test_throws istrait(Tr02{Integer}) # if function types get implemented this should be possible to catch
# end
# @test istrait(Tr02{Int}) # == true

# @traitdef Tr03{X} begin
#     g03{T<:X}(T, Vector{T})
# end
# g03{I<:Integer}(::I, ::Vector{I}) = 1
# @test istrait(Tr03{Integer})
# @test istrait(Tr03{Int})
