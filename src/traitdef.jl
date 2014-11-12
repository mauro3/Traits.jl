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
    #     R = g(X)
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
    constr = :(Bool[])
    assoc = quote end
    for ln in Lines(body)
        if ln.head==:macrocall # constraints
            parseconstraints!(constr, ln)
        elseif ln.head==:(=) # associated types
            push!(assoc.args, ln)
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
                # f1(X,Y) or X + Y -> Z
                if isa(ln.args[end], Expr) && ln.args[end].head==:(->) # X + Y -> Z
                    fn =  ln.args[1]
                    append!(argtype.args, ln.args[2:end-1])
                    if isa(ln.args[end].args[1], Expr)
                        append!(argtype.args, ln.args[end].args[1].args)
                    else
                        push!(argtype.args, ln.args[end].args[1])
                    end
                    tmp = rettype
                    rettype = :()
                    push!(rettype.args, ln.args[end].args[2].args[2])
                    append!(rettype.args, tmp.args)
                else # f1(X,Y)
                    fn =  ln.args[1]
                    append!(argtype.args, ln.args[2:end])
                    rettype =  :((Any...))
                end
            else
                throw(TraitException(
                                     "Something went wrong parsing the trait definition body with line:\n$ln"))
            end
            push!(outfns.args, :($fn => ($argtype, $rettype)))
        end
        
    end
    # store associated types:
    tmp = :(TypeVar[])
    for ln in Lines(assoc)
        tvar = ln.args[1]
        stvar = string(tvar)
        push!(tmp.args, :(TypeVar(symbol($stvar) ,$tvar)))
    end
    push!(assoc.args, :(assoctyps = $tmp))
    return outfns, constr, assoc
end

# 2.5) parse constraints
####
# Note, @constraints is not really a macro-call.
function parseconstraints!(constr, block)
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

@doc """The `@traitdef` macro is used to construct a trait.  Example:
      
     ```
     @traitdef MyArith{X,Y} begin
         # associated types
         Z = promote_type(X,Y)
         D = (X,Y)<:(Integer, Integer) ? Float64 : Z

         # method signatures
         +(X,Y) -> Z
         -(X,Y) -> Z
         *(X,Y) -> Z
         /(X,Y) -> D

         # constraints on X,Y
         @constraints begin
             X<:Number
             Y<:Number
         end
     end
     istrait(MyArith{Int, Int8}) # -> true
     ```
     
     - Assignments are for associated types, here `Z,D`.  These are
       types which can be calculated from the input types `X,Y`

     - Function signature definitions which are of the form `fn(X,Y,
       Other-Types) -> Return-Types`.  The return types can be left away

     - Constraints are marked in a block `@constaints`.  The are
       constraints in terms of the input types `X,Y` and are evaluated
       at trait checking.

     Traits can subtrait others:

     ```
     @traitdef MyInv{X}<:MyArith{X,X} begin
         inv(X) -> X
     end
     istrait(MyInv{Int}) # -> false
     istrait(MyInv{Float64}) # -> true
     ```
     """ ->
macro traitdef(head, body)
    ## make Trait type
    traithead, name = parsetraithead(head)
    # make the body
    meths, constr, assoc = parsebody(body)
    traitbody = quote
        methods::Dict{Function, Tuple}
        constraints::Vector{Bool}
        assoctyps::Vector{TypeVar}
        function $((name))()
            $assoc
            new( $meths, $constr, assoctyps)
        end
    end
    # add body to the type definition
    traithead.args[3] = traitbody
    return esc(traithead)
end
