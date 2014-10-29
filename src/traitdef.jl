####################
## Trait definitions
####################
#
# i.e. implement the @traitdef macro
#
# It looks like
# @traitdef Cmp{X,Y} <: Eq{X,Y} begin
#     isless(x,y) -> Bool
#     @constraints begin
#         X==Y
#     end
# end

# 1) parse the header
###
function parsecurly(def::Expr)
    # parses :(Cmp{x,y})
    # into: :Cmp, [:x,:y], :(Cmp{x,y}), ()
    name = def.args[1]
    paras = Symbol[]
    append!(paras,def.args[2:end])
    trait = def
    return name, paras, trait, :(())
end
function parsecomp(def::Expr)
    # parses :(Cmp{x,y} <: Eq{x,y})
    # into:  :Cmp, [:x,:y], :(Cmp{x,y}), :((Eq{x,y},))
    if  def.args[2]!=:<:
        error("not a <:")
    end
    name, paras, trait = parsecurly(def.args[1])
    supertraits = :()
    push!(supertraits.args, def.args[3])
    return name, paras, trait, supertraits
end

function parsetuple(def::Expr)
    # parses :(Cmp{x,y} <: Eq{x,y}, Sz{x}, Uz{y})
    # into   :Cmp, [:x,:y], :(Cmp{x,y}), :((Eq{x,y},Sz{x},Uz{y}))
    name, paras, trait, supertraits = parsecomp(def.args[1])
    append!(supertraits.args, def.args[2:end])
    return name, paras, trait, supertraits
end

function parsetraithead(def::Expr)
    # Transforms
    # :(Cmp{X,Y} <: Eq{X,Y}, Tr1{X})
    # into
    # trait = :(Cmp{X,Y}
    # supertraits = :(Eq{X,Y}, Tr1{X})
    # paras = [:X,:Y]
    # 
    # Returns:
    # :(immutable Cmp{X,Y} <: Trait{(Eq{X,Y}, Tr1{X})} end)
    
    if def.head==:tuple # contains several parents
        name, paras, trait, supertraits = parsetuple(def)
    elseif def.head==:comparison # contains a <:
        name, paras, trait, supertraits = parsecomp(def)
    elseif def.head==:curly # no parents
        name, paras, trait, supertraits = parsecurly(def)
    else
        error("Interface specification error")
    end
    # check supertraits<:Traits
    for i =1:length(supertraits.args)
        st = supertraits.args[i].args[1]
        eval_curmod(:(@assert istraittype($st)))
    end
    # make :(immutable Cmp{X,Y} <: Trait{(Eq{X,Y}, Tr1{X})} end)
    out = :(immutable $trait <: Traits.Trait{$supertraits} end)
    return out, name
end

# 2) parse the function definitions
###

function parsebody(body::Expr)
    # Transforms:
    # body = quote
    #     f1(X,Y) -> X,Int
    #     f2(Y) -> X
    #     @constraints begin
    #          X==Y
    #     end
    # end
    #
    # into
    # :([f1 => ((X,Y), (Int,Int)),
    #    f2 => ((Y,),  (X,)) ] )
    # :(Bool[X==Y])
    outfns = Expr(:dict)
    constr = Expr(:ref, :Bool)
    for ln in Lines(body)
        if ln.head==:macrocall
            parseconstraints!(ln, constr)
        else
            argtype = :()
            rettype = :()

            if ln.head==:tuple
                # several ret-types:
                # f1(X,Y) -> x,y
                append!(rettype.args, ln.args[2:end])
                ln = ln.args[1]
            end
            
            if ln.head==:(->)
                # f1(X,Y) -> x
                fn =  ln.args[1].args[1]
                append!(argtype.args, ln.args[1].args[2:end])
                tmp = rettype
                rettype = :()
                push!(rettype.args, ln.args[2].args[2])
                append!(rettype.args, tmp.args)
            elseif ln.head==:call
                # f1(X,Y)
                fn =  ln.args[1]
                append!(argtype.args, ln.args[2:end])
                rettype =  :((Any...))
            else
                throw(TraitException(
                                     "Something went wrong parsing the trait definition body:\n $body"))
            end
            push!(outfns.args, :($fn => ($argtype, $rettype)))
        end
        
    end
    return outfns, constr
end

# 2.5) parse constraints
####
# Note, @constraints is not really a macro-call.
function parseconstraints!(block, constr)
    # updates constr=Expr(:ref,...)
    if !(block.args[1]==symbol("@constraints"))
        throw(TraitException(
        "Only @constraints blocks allowed inside trait definition"))
    end
    for ln in Lines(block.args[2])
        push!(constr.args, ln)
    end
end

# 3) piece it together
###

macro traitdef(head, body)
    ## make Trait type
    traithead, name = parsetraithead(head)
    # make the body
    meths, constr = parsebody(body)
    traitbody = quote
        methods::Dict{Function, Tuple}
        constraints::Vector{Bool}
        function $((name))()
            new( $meths, $constr )
        end
    end
    # add body to the type definition
    traithead.args[3] = traitbody
    return esc(traithead)
end
