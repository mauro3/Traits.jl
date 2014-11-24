# helpers useful for users
export deparameterize_type

@doc """Removes type parameters from types, e.g. Array{Int}->Array.
     
     It is often useful to make an associated type with this to match
     against methods which do not specialize on the type parameters.
     """ ->
deparameterize_type(A::Type) = eval(A.name.module, A.name.name)::DataType

# Internal helpers
##################
function eval_curmod(expr::Union(Symbol,Expr,QuoteNode))
    # evaluates a symbol or expression in the current module.
    # I.e. the one where the macro definition is.
    return eval(current_module(),expr)
end

# To iterate over code blocks dropping the line-number bits:
immutable Lines
    block::Expr
end
Base.start(lns::Lines) = 1
function Base.next(lns::Lines, nr)
    for i=nr:length(lns.block.args)
        if isa(lns.block.args[i], Expr) && !(lns.block.args[i].head==:line)
            return lns.block.args[i], i+1
        end
    end
    return -1
end
function Base.done(lns::Lines, nr)
    if next(lns::Lines, nr)==-1
        true
    else
        false
    end
end


## Parsing
####
# translate a symbol in a array of expressions and/or symbols
function translate!(ex::Vector{Any}, di::Dict)
    for (i,e) in enumerate(ex)
        if isa(e, Symbol)
            ex[i] = get(di, e, e)
        else
            translate!(e.args, di)
        end
    end
    nothing
end

# expressions like :(I<:Int) are not parsed into TypeVar expressions
# but subtype comparisons.  This function translates this
function subt2tvar!(exs::Vector{Any})
    for (i,ex) in enumerate(exs)
        if isa(ex, Symbol)
            # do nothing
        elseif ex.head==:comparison
            exs[i] = Expr(:<:, ex.args[1], ex.args[3])
        else
            subt2tvar!(ex.args)
        end
    end
    nothing
end

function tvar2tvar!(exs::Vector{Any})
    # tranlates x<:Int -> TypeVar(:X, Int)
    for (i,ex) in enumerate(exs)
        if isa(ex, Symbol)
            # do nothing
        elseif ex.head==:<:
            var = ex.args[1]
            exs[i] = :(TypeVar(symbol($(string(var))), $(ex.args[2])))
        else
            tvar2tvar!(ex.args)
        end
    end
    nothing
end

# function return_types_v2(f::Function, typs::ANY)
#     # for some reason this function take forever to JIT. (about 4 secs!)
#     # see https://github.com/JuliaLang/julia/issues/9131
#     a = code_typed(f, typs)
#     if length(a)>1
#         error("several return types")
#     elseif length(a)==0
#         error("no return types")
#     end
#     a[1].args[end].typ
# end

# check whether a type is parameterized
isparameterized(t::DataType) = length(t.parameters)==0 ? false : true

function hasparameters(t::DataType)
    if isparameterized(t)
        if all([map(typeof,t.parameters)...].==DataType)
            return true  # i.e. return true for Dict{Int,Int}
        else
            return false # but not for Dict{Int}
        end
    else
        return false
    end
end

# # check whether a function is parameterized
# function isparameterized(m::Method)
#     if isa(m.tvars, Tuple)
#         return length(m.tvars)==0 ? false : true
#     else
#         return true
#     end
# end

# Base.subtypes(x::(ANY...)) = Base.subtypes(Main, x::(ANY...))
# function Base.subtypes(m::Module, ts::(ANY...); maxout=100_000)
#     # Extends subtypes to work with tuple-types.
#     # Returns at most maxout (default=100_000) types.
#     slots = Array(Tuple, length(ts))
#     allempty = true
#     for (i,t) in enumerate(ts)
#         st = subtypes(t)
#         if length(st)==0
#             slots[i] = (t,) # if a slot is empty use t
#         else
#             slots[i] = tuple(st...)
#             allempty = false
#         end
#     end
#     allempty && return Any[]
#     # Make all possible combinations (could be many!)
#     out = combos(slots, maxout)
# end

# function combos{T<:Tuple}(typs::Vector{T}, maxout; verbose=true)
#     # makes all possible combinations.  See test/helpers.jl
#     len = length(typs)
#     lens = map(length, typs)
#     n = min(prod(lens), maxout)
#     if n==maxout && verbose
#         println("subtypes: not returning all $(prod(lens)) type tuples, only $maxout")
#     end
#     inds = ones(Int,len)
#     out = Array(Tuple, n)
#     for j=1:n
#         try 
#             out[j] = tuple([typs[ii][i] for (ii,i) in enumerate(inds)]...)
#         catch e
#             @show j, inds
#         end
#         # updated inds
#         inds[1] += 1        
#         for i=1:len-1
#             if inds[i]>lens[i]
#                 inds[i] = 1
#                 inds[i+1] += 1
#             end
#         end
#     end
#     return out
# end

# # Does the same as method_exists except that it also works for
# # abstract/parameterized types by considering whether method_exists
# # for all concrete subtypes.
# #
# # I.e.
# # method_exists_forall_subtypes(f, ts)==true => for all sts<:ts, f(::ts) works
# baremodule Res
#   const F = 0 # false
#   const T = 1 # true
#   const M = 2 # undecided, maybe, i.e. look further
# end

# # function get_intypes(f, Ts)
# #     # based on Jiahao's: http://nbviewer.ipython.org/gist/jiahao/b0d4279cec83b681d95f
# #     ct = code_typed(f, Ts)
# #     intypes = Array(Any, length(ct))
# #     for (i,method) in enumerate(code_typed(f, Ts))
# #         #Get types of inputs
# #         tmp = [x[2] for x in method.args[2][2][1:length(Ts)]]
# #         for (j,tm) in enumerate(tmp)
# #             if isa(tm,TypeVar)
# #                 tmp[j] = tm.ub
# #             end
# #         end
# #         intypes[i] = tmp
# #     end
# #     return intypes
# # end

# dbg_println(st::String) = verb && println(st)
# const verb = true
# function is_fnparameter_match_inputs(meth_tvars)
#     for i=1:2:length(meth_tvars)
# #        @show meth_tvars[i+1], meth_tvars[i]
#         !( meth_tvars[i+1]<:meth_tvars[i] ) && return false
#     end
#     return true
# end

# # function is_type_parameter_match_inputs(TS, meth_sig)
# #     for i=1:2:length(meth_tvars)
# # #        @show meth_tvars[i+1], meth_tvars[i]
# #         !( meth_tvars[i+1]<:meth_tvars[i] ) && return false
# #     end
# #     return true
# # end

# function method_exists_forthis_type(f::Function, TS::(ANY...), meths)
#     # if the method is defined for TS we're done:
#     #@show meths

#     if method_exists(f, TS) # note this does no match parameterized
#                             # functions when its parameters are not
#                             # specified.
#         dbg_println("A method matches exactly.")
#         return Res.T
#     end

#     # if there is no method, we're done:
#     if length(meths)==0
#         dbg_println("False: No methods for: $TS")
#         return Res.F
#     end

#     # if there are one or more methods, check them
#     for mm in meths
#         if is_fnparameter_match_inputs(mm[2])
# ----------> check this            
#             @show TS, mm[1], TS<:mm[1], mm[1]<:TS
#             if TS<:mm[1]
#                 dbg_println("A parameterized method matches exactly: $TS")
#                 return Res.T
#             else
#                 dbg_println("A parameterized method does not match exactly: $TS, looking further")
#             end
#         else
#             dbg_println("A parameterized cannot match: $TS")
#             return Res.F
#         end
        
#     end
    
#     # if length(meths)==1 && isparameterized(meths[1][3])
#     #     if is_parameter_match_inputs(meths[1][2])
#     #         if Base.typeseq(meths[1][1], TS)
#     #             dbg_println("A parameterized method matches exactly: $TS")
#     #             return Res.T
#     #         else
#     #             dbg_println("A parameterized method does not match exactly: $TS, looking further")
#     #         end
#     #     else
#     #         dbg_println("A parameterized cannot match: $TS")
#     #         return Res.F
#     #     end
#     # end

#     # If it is a leaf-type and we're not done by now, there is no hope:
#     if all(map(isleaftype,TS))  # todo think about parameterized types
#         dbg_println("Leaftype testing false: $TS")
#         return Res.F
#     else # we don't know
#         return Res.M
#     end
# end

# function method_exists_forall_subtypes(f::Function, TS::(ANY...); depth=4, checktop=true)
#     # Checks whether a method of f exists for all subtypes of TS, up
#     # to a specified search depth (default=4, set to -1 for inf depth).
#     #
#     # Return:
#     # - 1 if true
#     # - 0 if false
#     # - 2 if undecided
#     dbg_println("recusing at depth $depth")

#     if depth==0
#         return Res.M
#     end

#     # reflection.jl/_methods.  See video "Introduction to Julia" by Jeff
#     # Internals-osdeT-tWjzk.mp4, minute 29
#     # inputs: function, type, limit of number of matches
#     # outputs:
#     # matchtype-signature, values of method parameters, method object
#     if checktop
#         ms = Base._methods(f, TS, -1)
#         if ms!=false
#             r = method_exists_forthis_type(f, TS, ms)
#             if r==Res.T || r==Res.F
#                 return r
#             end # otherwise check further
#         else # no method found
#             dbg_println("No methods found, recursing.")
#         end
#     end

#     # Do a breadth first search in the subtypes:
#     checksTS = Any[]
#     for sTS in subtypes(TS)
#         ms = Base._methods(f, sTS, 2)
#         if ms==false
#             if isleaftype(sTS)
#                 dbg_println("False: one of the subtypes test false: $sTS")
#                 return Res.F
#             else
#                 push!(checksTS, sTS) # needs further checking below
#                 continue
#             end
#         end
#         #@show 2, ms
#         r = method_exists_forthis_type(f, sTS, ms)
#         if r==Res.F # if one subtype does not check-out return false
#             dbg_println("False: one subtype does not check: $sTS")
#             return Res.F
#         end
#         if r==Res.M
#             push!(checksTS, sTS) # needs further checking below
#         end 
#     end
#     # recurse into subtypes where above was not
#     out = 0
#     for (i,sTS) in enumerate(checksTS)
#         r = method_exists_forall_subtypes(f, sTS, depth=(depth-1), checktop=false)
#         if r==Res.F # if one subtype does not check-out return false
#             dbg_println("False: one lower subtype does not check: $sTS")
#             return Res.F
#         end
#         out += r # accumulate undicided 
#     end
    
#     if out==0
#         dbg_println("True: all subtypes test true.")
#         return Res.T
#     else
#         return Res.M
#     end
# end

# # reflection.jl/_methods.  See Introduction to Julia Internals-osdeT-tWjzk.mp4 minute 29
# # 1) inputs: function, type, limit of number of matches
# # 2) inputs: only used in its internal recursion
# #
# # output:
# # matchtype -signature, values of method parameters, method object
