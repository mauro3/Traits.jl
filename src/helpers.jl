# helpers useful for users
export deparameterize_type

@doc """Removes type parameters from types, e.g. Array{Int}->Array.
     
     It is often useful to make an associated type with this to match
     against methods which do not specialize on the type parameters.
     """ ->
deparameterize_type(A::DataType) = A.name.primary #eval(A.name.module, A.name.name)::DataType
deparameterize_type(A::TypeConstructor) = error("TypeConstructor not supported by deparameterize_type.")
  # could do A.body.name.primary but that would remove fixed parameters, like the 1 in Vector

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

@doc "Checks all elements of a collection are equal" ->
allequal(x) = reduce(&, [x[1]==xx for xx in x])

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

# check whether a type is parameterized
isparameterized(t::DataType) = length(t.parameters)==0 ? false : true

function hasparameters(t::DataType)
    if isparameterized(t)
        if reduce(&, [map(typeof,t.parameters)...].==DataType)
            return true  # i.e. return true for Dict{Int,Int}
        else
            return false # but not for Dict{Int}
        end
    else
        return false
    end
end

# debugging
# import Traits: FakeMethod,  replace_concrete_tvars, get_tms_fms, isfitting
function get_tms_fms(Tr, gf::Function, ind=1)
    tm = collect(methods(Tr().methods[gf]))[1]
    fms = collect(methods(gf, NTuple{length(tm.sig),Any}))
    tm, fms
end
