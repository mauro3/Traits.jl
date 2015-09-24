###############################
# Trait-functions with dispatch
###############################
#
# Provides @traitfn macro which can be used like so:
#
# @traitfn f1{X,Y<:Integer; D1{Y}, D4{X,Y}}(x::X,y::Y) = x + sin(y)
# @traitfn f1{S,T<:Integer; D1{S}, D1{T}  }(s::S,t::T) = sin(s) - sin(t)
# @traitfn f1{X,Y<:AbstractFloat; D1{X}, D1{Y}  }(x::X,y::Y) = cos(x) - cos(y)

typealias FName Union{Symbol,Expr}

# generates: X1, X2,... or x1, x2.... (just symbols not actual TypeVar)
type GenerateTypeVars{CASE}
end
Base.start(::GenerateTypeVars) = 1
Base.next(::GenerateTypeVars{:upcase}, state) = (symbol("X$state"), state+1) # X1,..
Base.next(::GenerateTypeVars{:lcase}, state) = (symbol("x$state"), state+1)  # x1,...
Base.done(::GenerateTypeVars, state) = false

# Type to hold parsed function defs:
type ParsedFn  # (probably should adapt MetaTools.jl...)
    name::FName  # f1
    fun::Expr # :(f1{X<:Int,Y})
    typs::Vector{Any} # [:(X<:Int),:Y]
    sig::Vector{Any} # [:(x::X), :(y::Y)]
    traits::Vector{Any} # [D1{X}, D2{X,Y}]
    body::Expr # quote ... end
end

# Parsing:
function parsetraitfn_head(head::Expr)
    # Transforms
    # f1{X<:Int,Y; D1{X}, D2{X,Y}}(x::X,y::Y)
    #
    # into a ParsedFn

    nametyp = head.args[1]
    sig = head.args[2:end]
    name = nametyp.args[1]
    fun = Expr(:curly, deepcopy(nametyp.args[[1;3:end]])...)
    typs = fun.args[2:end]
    traits = nametyp.args[2].args
    return ParsedFn(name, fun, typs, sig, traits, :())
end

gettypesymbol(x::Expr) = x.args[1] # :(X1<:Int)
gettypesymbol(x::Symbol) = x
function translate_head(fn::ParsedFn)
    # Takes output from parsetraitfn_head and
    # renames sig and TypeVar:
    # f1{X,Y; D1{X}, D2{X,Y}}(x::X,y::Y)
    # ->
    # f1{X1,X2; D1{X1}, D2{X1,X2}}(x::X1,y::X2)
    #
    # Returns translated ParsedFn

    function make_trans(sig, typs)
        # makes two dictionaries with keys the old typevars, and
        # values the lowercase and uppercase variables

        # make variable-symbol translation map:
        trans_var = Dict{Symbol,Symbol}()
        for (i,si) in enumerate(GenerateTypeVars{:lcase}())
            if length(sig)<i;  break end
            trans_var[sig[i].args[1]] = si
        end

        trans_Tvar = Dict{Symbol,Symbol}()
        # make type-symbol translation map:
        for (i,tv) in enumerate(GenerateTypeVars{:upcase}())
            if length(typs)<i;  break end
            trans_Tvar[gettypesymbol(typs[i])] = tv
        end
        return trans_var, trans_Tvar
    end
    trans_var, trans_Tvar = make_trans(fn.sig, fn.typs)

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
            t = s.args[2]
            if isa(t, Symbol)
                if haskey(trans_Tvar, s.args[2])
                    s.args[2] = trans_Tvar[s.args[2]]
                end
            elseif t.head==:curly
                for i=2:length(t.args)
                    if haskey(trans_Tvar, t.args[i])
                        t.args[i] = trans_Tvar[t.args[i]]
                    end
                end
            else
                error("Cannot parse $t in $(fnt.sig)")
            end
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

# Macro building blocks:
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
        if isa(sig[i], Expr) && sig[i].head==:(::)
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
    return :( $fname( $(strip_typeasserts(sig)...) ) )
end
function get_concrete_type_symb(typs)
    # Returns the most general type satisfying typs as a list of symbols:
    # [:(X<:Int), :Y] -> [:Int, :Any]
    out = Any[]
    for t in typs
        if isa(t, Symbol)
            push!(out, :Any)
        elseif  t.head==:.
            push!(out, t)
        else
            push!(out, t.args[2])
        end
    end
    return out
end
function get_concrete_type_Typetuple(typs)
    # Returns the most general type satisfying typs as a tuple of
    # actual types:
    # [:(X<:Int), :Y] -> Tuple{Int, Any}
    out = get_concrete_type_symb(typs)
    for i in 1:length(out)
        out[i] = Type{eval_curmod(out[i])}
    end
    return Tuple{out...}
end

function make_Type_sig(typs)
    # [:(X<:Int), :Y] -> [:(::Type{X}), :(::Type{Y})]
    out = Any[]
    for t in typs
        push!(out, :(::Type{$t}))
        # if isa(t, Symbol) || t.head==:.

        # else
        #     push!(out, :(::Type{$(t.args[1])}))
        # end
    end
    return out
end
function has_only_one_method(fname::Symbol, typs)
    # Checks whether fname has one and only one method for types in
    # typs.
    if isdefined(current_module(), fname)
        1 == length( methods(eval_curmod(fname),
                             get_concrete_type_Typetuple(typs)) )
    else
        false
    end
end
function has_only_one_method(fname::Expr, typs)
    # Checks whether fname has one and only one method for types in
    # typs.
    if isdefined(eval_curmod(fname.args[1]), fname.args[2].args[1])
        1 == length( methods(eval_curmod(fname),
                             get_concrete_type_Typetuple(typs)) )
    else
        false
    end
end


@doc """The heart, the trait-dispatch function.

     Trait-function (TF) dispatch works like:

     - first dispatch on the normal types

     Then dispatch on traits using the following rules, terminating
     when only one or zero possibilities are left

     - find all matching traits
     - discriminate using subtraits, i.e. a subtrait will win over its supertrait
     - score all traits according to:
       1 point for all single parameter traits,
       2 points for all two parameter traits,
       etc.
       Now pick the highest scoring method.
     - if still ambiguous throw an error
     """->
function traitdispatch(traittypes, fname)
    poss = Any[]
    for Tr in traittypes
        if istrait(Tr)
            push!(poss, Tr)
        end
    end
    # discriminate using subtraits
    if length(poss)>1
        topurge = []
        for (i,p1) in enumerate(poss)
            for p2 in poss
                if length(p1)==length(p2)
                    checks = true
                    for j=1:length(p1)
                        checks = checks && issubtrait(p2[j], p1[j]) && p2[j]!=p1[j]
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
    # check whether we can discriminate on more specific definitions:
    if length(poss)>1
        # strategy:
        # - each single parameter trait gives 1 point, 2-parameter 2 points, etc
        # - pick method with most points
        #
        # This is not the end of the story but better...
        score = zeros(Int, length(poss))
        for (i,p1) in enumerate(poss)
            for t in p1
                score[i] += length(t.parameters)
            end
        end
        poss = poss[find(maximum(score).==score)]
    end
    # check we got a unique trait for dispatch
    if length(poss)==0
        throw(TraitException("No matching trait found for function $(string(fname))"))
    elseif length(poss)>1
        throw(TraitException("For function $(string(fname)) there are several matching traits:\n $poss"))
    end
    return poss[1]
end


@doc """Functions which also dispatch using traits can be defined with
     `@traitfn`.  Once defined they can be used like ordinary
     functions.  For instance a library user would not need to treat
     them specifically.

     Example continuing from the documentation of `@traitdef` and `@traitimpl`:

     ```
     @traitfn tf{X,Y; MyArith{X,Y}}(x::X, y::Y) = x/y+y
     tf(5, Int8(4)) # -> 5.25
     tf(A(2), AB(4)) # -> A(4.5)
     tf(5, Uint8(4)) # -> ERROR: TraitException("No matching trait found for function tf")
     ```

     - all the arguments of `tf` which participate in trait-dispatch
       need to be parameterized.
     - trait-methods and non-trait methods can be mixed.
     - for details on trait dispatch see doc of Traits.traitdispatch
     """ ->
macro traitfn(fndef)
    fn, fnt = parsetraitfn(fndef)
    if length(unique(fnt.traits))!=length(fnt.traits)
        throw(TraitException(
          "There are repeated traits in the trait signature of $(fndef.args[1])"))
    end

    ## make primary function: f
    #### tf(x, y)
    # (Just overwrite definitions of f if they exists already,
    # generates warnings though...)

    # definition head: fn{X,Y}(x::X,y::Y)
    f = makefnhead(fn.name, fn.typs, fn.sig)
    # definition body: _trait_fn(_trait_type_f1(x,y) ), x, y)
    args1 = Any[:(Traits._TraitDispatch), fn.sig...]
    args2 = Any[makefncall(fn.name, args1), fn.sig...]
    body = makefncall(fn.name, args2)
    f = :($f = $body)

    ## make function containing the logic: trait_f
    #### tf(::Type{Tuple{Traits...}}, x, y)
    # 1) make the traits-type
    trait_typ = :(Tuple{})
    append!(trait_typ.args, fn.traits)
    trait_typ = :(::Type{$trait_typ})
    args = Any[trait_typ, fn.sig...]
    trait_f = makefnhead(fn.name, fn.typs, args)
    trait_f = :($trait_f = $(fn.body))
    # add @inline
    trait_fn = Expr(:macrocall, symbol("@inline"), trait_f)

    ## Make function storing the trait-types
    #### tf(Traits._TraitStorage, sig...)
    # This function will return all defined Trait-tuples for a certain
    # signature. Example:
    #
    # julia> yt1(Traits._TraitStorage, Int, Int)
    # (Any[ Tuple{Traits.Arith{Int64,Int64}} ], Any[:( Tuple{Arith{X1,X2}} )], Any[:X1,:X2])


    ## 1) Get the existing traits out of the trait_type_f:
    #    These can be retrieved with the call:
    #    trait_type_f(Traits._TraitStorage, ::Type{X}, ::Type{Y}...) for suitable X, Y...
    args1 = Any[:(Traits._TraitStorage), get_concrete_type_symb(fn.typs)...]
    trait_type_f_store_call = makefncall(fn.name, args1)

    args2 = Any[:(Traits._TraitStorage), fn.typs...]
    if has_only_one_method(fn.name, args2)
        traittypes = eval_curmod(trait_type_f_store_call)[2]
    else
        traittypes = Any[]
    end

    ## 2) update old_traittypes with the new ones
    newtrait = :( Tuple{$(fnt.traits...)} )
    if !(newtrait in traittypes)
        push!(traittypes, newtrait)
    end

    ## 3) make new trait-type storage function
    #     tf(::Type{Traits._TraitStorage}, ::Type{X}, ::Type{Y}...)
    sig2typs(sig) = [s.args[2] for s in fnt.sig]
    sig = make_Type_sig([:(Traits._TraitStorage), sig2typs(fnt.sig)...])
    trait_type_f_store_head = makefnhead(fn.name,
                                          fnt.typs, sig)
    trait_type_f_store = :(
                            $trait_type_f_store_head = (Any[$(traittypes...)], )
                            )
    push!(trait_type_f_store.args[2].args, traittypes)
    push!(trait_type_f_store.args[2].args, fnt.typs)

    ## make trait-dispatch stagedfunction: trait_type_f
    #### tf(Traits._TraitDispatch, sig...)

    sig = make_Type_sig([:(Traits._TraitDispatch)])
    append!(sig,fnt.sig)
    trait_type_f = Expr(:stagedfunction, makefnhead(fn.name, fnt.typs, sig))
    args = Any[:(Traits._TraitStorage), fnt.sig...]
    body = quote
        # get the stored trait-types:
        traittypes = $(makefncall(fn.name, args))[1]
        traittyp = Traits.traitdispatch(traittypes, $(fn.name))
        # construct function from traittyp
        out = :(Tuple{})
        for s in traittyp
            push!(out.args, s)
        end
        return out
    end
    push!(trait_type_f.args, body)

    ## now put all together
    ####
    out = quote
        $trait_fn
        $trait_type_f_store
        $trait_type_f
        $f
    end
    return esc(out)
end

##########
# Helper functions
##########

function traitmethods(f::Function, nsig=Tuple{Vararg{Any}}; print=false)
    out = Any[]
    for m in methods(f, Tuple{Any, nsig...})
        if isa(m.sig[1], Type) && m.sig[1].parameters[1]<:Tuple{Vararg{Traits.Trait}}
            push!(out, m)
        end
    end
    if print # pretty print it
        if length(out)>0
            println("Function $f has the following trait methods:")
        else
            println("Function $f has no trait methods.")
            return nothing
        end
        for m in out
            showtraitmethod(m)
        end
        return nothing
    else
        return out
    end
end

# adapted from methodshow.jl:
showtraitmethod(m::Method) = showtraitmethod(STDOUT, m)
function showtraitmethod(io::IO, m::Method)
    print(io, m.func.code.name)
    tv, decls, file, line = Base.arg_decl_parts(m)
    if isempty(tv)
        error("Not a trait-method")
    end
    tv = string(tv)[2:end-1]
    traits = strip(decls[1][2][7:end-2], ',')
    sig = [isempty(d[2]) ? d[1] : d[1]*"::"*d[2] for d in decls[2:end]]
    out = "{$tv; $traits}($(sig...))\n"
    print(io, out)
end
