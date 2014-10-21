###############################
# Trait-functions with dispatch
###############################
#
# Provides @traitfn macro which can be used like so:
#
# @traitfn f1{X,Y<:Integer; D1{Y}, D4{X,Y}}(x::X,y::Y) = x + sin(y)
# @traitfn f1{S,T<:Integer; D1{S}, D1{T}  }(s::S,t::T) = sin(s) - sin(t)
# @traitfn f1{X,Y<:FloatingPoint; D1{X}, D1{Y}  }(x::X,y::Y) = cos(x) - cos(y)

# generates: X1, X2,... or x1, x2....
type GenerateTypeVars{CASE}
end
Base.start(::GenerateTypeVars) = 1
Base.next(::GenerateTypeVars{:upcase}, state) = (symbol("X$state"), state+1) # X1,..
Base.next(::GenerateTypeVars{:lcase}, state) = (symbol("x$state"), state+1)  # x1,...
Base.done(::GenerateTypeVars, state) = false

# Type to hold parsed function defs:
type ParsedFn  # (probably should adapt MetaTools.jl...)
    name
    fun
    typs
    sig
    traits
    body
end
function ==(p::ParsedFn, q::ParsedFn) 
    out = true
    for n in names(p)
        out = out && getfield(p,n)==getfield(q,n)
        if !out
            @show n
        end
    end
    out
end

function parsetraitfn_head(head::Expr)
    # Transforms
    # f1{X<:Int,Y; D1{X}, D2{X,Y}}(x::X,y::Y)
    # 
    # into
    # name, fun         , typs          , sig               , traits
    # f1,   f1{X<:Int,Y}, [:(X<:Int),:Y], [:(x::X), :(y::Y)], (D1{X}, D2{X,Y})

    nametyp = head.args[1]
    sig = head.args[2:end]
    name = nametyp.args[1]
    fun = Expr(:curly, deepcopy(nametyp.args[[1,3:end]])...)
    typs = fun.args[2:end]
    traits = nametyp.args[2].args
    return ParsedFn(name, fun, typs, sig, traits, :())
end

function translate_head(fn::ParsedFn)
    # Takes output from parsetraitfn_head and 
    # renames sig and TypeVar:
    # f1{X,Y; D1{X}, D2{X,Y}}(x::X,y::Y)
    # ->
    # f1{X1,X2; D1{X1}, D2{X1,X2}}(x::X1,y::X2)
    #
    # Returns:
    # fun_trans,    , typs          , sig_trans       , traits_trans
    # f1{X1<:Int,Y1}, {X1<:Int,Y1}  , (x1::X1, x2::X2), (D1{X1}, D2{X1,X2})
    
    function make_trans(sig)
        # makes two dictionaries with keys the old typevars, and 
        # values the lowercase and uppercase variables
        trans_var = Dict{Symbol,Symbol}()
        for (i,si) in enumerate(GenerateTypeVars{:lcase}())
            if length(sig)<i;  break end
            trans_var[sig[i].args[1]] = si
        end

        trans_Tvar = Dict{Symbol,Symbol}()
        for (i,tv) in enumerate(GenerateTypeVars{:upcase}())
            if length(sig)<i;  break end
            trans_Tvar[sig[i].args[2]] = tv
        end
        return trans_var, trans_Tvar
    end
    trans_var, trans_Tvar = make_trans(fn.sig)
    
    # do the translations:
    fnt = deepcopy(fn)
    for i in 2:length(fnt.fun.args)
        tv = fnt.fun.args[i]
        if isa(tv, Expr)
            # :(X<:Int)
            tv.args[1] = trans_Tvar[tv.args[1]]
        else
            # :X
            fnt.fun.args[i] = trans_Tvar[tv]
        end
    end
    fnt.typs = fnt.fun.args[2:end]
    for s in fnt.sig
        s.args[1] = trans_var[s.args[1]]
        if isa(s, Expr)
            s.args[2] = trans_Tvar[s.args[2]]
        end
    end
    for t in fnt.traits
        t.args[2:end] = map(x->trans_Tvar[x], t.args[2:end])
    end
    
    return fnt
end

function parsetraitfn(fndef::Expr)
    # Transforms
    # f1{X,Y; D1{X}, D2{X,Y}}(x::X,y::Y) = sin(x) + y
    #
    # into a ParsedFn

    # checks
    length(fndef.args)==2 || throw(TraitException("Something is wrong with $fndef"))

    # parse
    head = fndef.args[1]
    body = fndef.args[2]
    fn = parsetraitfn_head(head)
    fn.body = body
    fnt = translate_head(fn)

    return fn, fnt
end

function isdefined_fn(fname, _trait_fname, sig)
    # Warns if overwriting an existing method
    if !isdefined(fname)
        return false
    end
    if !isdefined(_trait_fname)
        return false
    end

    sig_fn = tuple([Any for i=1:length(sig)]...)
    for meth in methods(eval(fname), sig_fn)
        if meth.sig==sig_fn
            warn("""Function $fname already defined with signature: $sig_fn.
                     Creating the @traitfn overwrites this function!""")
        end
    end
    return true
end

function makefnhead(fname, typs, sig)
    # Makes a Expr for a function head:
    # f1, {X,Y}, (x::X, r::Y) -> f1{X,Y}(x::X, r::Y)
    fntyp = Expr(:curly)
    push!(fntyp.args, fname)
    append!(fntyp.args, typs)
    outfn = Expr(:call)
    push!(outfn.args, fntyp)
    append!(outfn.args, sig)
    return outfn
end
function strip_typeasserts(sig)
    # [:(x::X), :(r::R)] -> [:x,:r]
    sig = deepcopy(sig)
    for i = 1:length(sig)
        if isa(sig[i], Expr)
            # :(x::X)
            sig[i] = sig[i].args[1]
        end
    end
    return sig
end
function makefncall(fname, sig)
    # Makes an Expr to call function fname:
    # f1, (x::X, r::R) -> f1(x, r)
    # (i.e. strips the extraneous type-assertions)
    sig = strip_typeasserts(sig)
    outfn = Expr(:call)
    push!(outfn.args, fname)
    append!(outfn.args, sig)
    return outfn
end
function get_concrete_type(typs; asTypetuple=false)
    # Returns the most general type satisfying typs:
    # [:(X<:Int), :Y] -> [:Int, :Any]
    #              or -> (Int, Any) 
    out = Any[]
    for t in typs
        if isa(t, Symbol)
            push!(out, :Any)
        else
            push!(out, t.args[2])
        end
    end
    if asTypetuple
        for i in 1:length(out)
            out[i] = Type{eval(current_module(), out[i])}
        end
        return tuple(out...)
    else
        return out
    end
end
function make_Type_sig(typs)
    # [:(X<:Int), :Y] -> [:(::Type{X}), :(::Type{Y})]
    out = Any[]
    for t in typs
        if isa(t, Symbol)
            push!(out, :(::Type{$t}))
        else
            push!(out, :(::Type{$(t.args[1])}))
        end
    end
    return out
end

function hidden_fn_names(fname)
    symbol("_trait_$(string(fname))"), symbol("_trait_type_$(string(fname))")
end

function traitdispatch(traittypes, fname)
    poss = Any[]
    for Tr in traittypes
        if traitcheck(Tr)
            push!(poss, Tr)
        end
    end
    # try to discriminate using subtraits
    if length(poss)>1
        topurge = []
        for (i,p1) in enumerate(poss)
            for p2 in poss
                if length(p1)==length(p2)
                    checks = true
                    for j=1:length(p1)
                        checks = checks && issubtrait(p2[j], p1[j])
                    end
                    if checks
                        push!(topurge, i)
                    end
                end
            end
        end
        topurge = sort(unique(topurge))
        deleteat!(poss, topurge)
    end
    # check we got a unique trait for dispatch
    if length(poss)==0
        throw(TraitException("No matching trait found for function $(string(fname))"))
    elseif length(poss)>1
        throw(TraitException("Several matching traits found for function $(string(fname))"))
    end
    return poss[1]
end

# Finally:
macro traitfn(fndef)
    fn, fnt = parsetraitfn(fndef)
    # the names of the helper functions:
    _trait_fname, _trait_type_fname = hidden_fn_names(fn.name)

    ## make primary function: f
    ####
    # (just overwrite defs if they exists already)
    
    # definition head: fn{X,Y}(x::X,y::Y)
    f = makefnhead(fn.name, fn.typs, fn.sig)
    # definition body: _trait_fn(x, y, _trait_type_f1(x,y) )
    body = makefncall(_trait_fname, fn.sig)
    push!(body.args, makefncall(_trait_type_fname, fn.sig))
    f = :($f = $body)
    
    ## make function containing the logic: _trait_f
    ####
    _trait_f = makefnhead(_trait_fname, fn.typs, fn.sig)
    # make the traits-type 
    trait_typ = Expr(:tuple)
    append!(trait_typ.args, fn.traits)
    push!(_trait_f.args, :(::Type{$trait_typ}))
    _trait_f = :($_trait_f = $(fn.body))
    # add @inline
    _trait_fn = Expr(:macrocall, symbol("@inline"), _trait_f)

    ## make function storing the trait-types
    ####
    ## 1) Get the existing traits out of the _trait_type_f:
    #    These can be retrieved with the call:
    #    _trait_type_f(::Type{X}, ::Type{Y}...) for suitable X and Y
    if isdefined(_trait_type_fname) && 1==length(methods(eval(current_module(),_trait_type_fname), get_concrete_type(fn.typs; asTypetuple=true)))
        _trait_type_f_store_call = makefncall(_trait_type_fname, get_concrete_type(fn.typs))
        traittypes = eval(current_module(), _trait_type_f_store_call)[2] 
    else
        traittypes = Any[]
    end
    
    ## 2) update old_traittypes with the new ones TODO
    newtrait = Expr(:tuple, fnt.traits...)
    if !(newtrait in traittypes)
        push!(traittypes, newtrait)
    end
    
    ## 3) make new trait-type storage function _trait_type_f(::Type{X}, ::Type{Y}...)
    _trait_type_f_store_head = makefnhead(_trait_type_fname, 
                                          fnt.typs, make_Type_sig(fnt.typs))
    _trait_type_f_store = :(
                            $_trait_type_f_store_head = (Any[$(traittypes...)], )
                            )
    push!(_trait_type_f_store.args[2].args, traittypes)

    ## make trait-dispatch stagedfunction: _trait_type_f
    ####

    _trait_type_f = Expr(:stagedfunction, makefnhead(_trait_type_fname, fnt.typs, fnt.sig))
    body = quote
        traittypes = $(makefncall(_trait_type_fname, fnt.sig))[1]

        traittyp = Traits.traitdispatch(traittypes, $(fn.name))
        # construct function from traittyp
        out = :(())
        for s in traittyp
            push!(out.args, s)
        end
        return out
    end
    push!(_trait_type_f.args, body)
    
    ## now put all together
    ####
    out = quote end
    push!(out.args, _trait_fn)
    push!(out.args, _trait_type_f_store)
    push!(out.args, _trait_type_f)
    push!(out.args, f)
    # @show f
    # @show _trait_fn
    # @show _trait_type_f
    # @show _trait_type_f_store
    return esc(out)
end

##########
# Helper functions
##########

function traitmethods(f::Function)
    # needs some work...
    _trait_type_fname = hidden_fn_names(f.env.name)[2]
    _trait_type_f = eval(current_module(), _trait_type_fname)
    out = Any[]
    if !isdefined(_trait_type_fname)
        return out
    end
    for m in methods(_trait_type_f) # loop through methods
        if isa(m.sig[1],DataType)
            append!(out, _trait_type_f([t.ub for t in m.tvars]...)[2])
        end
    end
    return out
end
