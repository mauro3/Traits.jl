# Some rough definitions of some common traits/interfaces of the
# standard library.  Note that most are single parameter traits.

export Eq, Cmp, Iter, Collection, IterColl, Indexable, Assoc, Arith

@traitdef Eq{X,Y} begin
    # note anything is part of Eq as ==(::Any,::Any) is defined
    ==(X,Y) -> Bool
end

@traitdef Cmp{X,Y} <: Eq{X,Y} begin
    isless(X,Y) -> Bool
    # automatically provides:
    # >, <, >=, <=
end

@traitdef Iter{X}  begin
    start(X)
    next(X, Any)
    done(X, Any) -> Bool
    # automatically provides:
    # zip, enumerated, in, map, reduce, ...
end

# general collections
@traitdef Collection{X} <: Iter{X} begin
    isempty(X) -> Bool
    length(X) -> Integer
    eltype(X)
end

@traitdef IterColl{X} <: Collection{X} begin # iterable collection
    empty!(X) -> X
end

@traitdef Indexable{X} <:Collection{X} begin
    getindex(X, Any)
    setindex!(X, Any, Any)
    length(X) -> Integer
    # automatically provided:
    # size(X) -> Tuple
    # size(X,Integer) -> Integer
end

@traitdef Assoc{X} <: Indexable{X} begin
    # note, ObjectId dict is not part of this interface
    haskey(X, Any)
    get(X, Any, Any)
    get(Function, X, Any)
    get!(X, Any, Any)
    get!(Function, X, Any)
    getkey(X, Any, Any)
    delete!(X, Any) -> X
    pop!(X, Any)
    pop!(X, Any, Any)
    keys(X) -> Iter{X} # would be nice here!
    values(X)
    merge(X, Any...) -> X
    merge!(X, Any...)
end

@traitdef Arith{X,Y} begin
    (+(X,Y)) -> Any # ToDo: need to work on the parser to avoid the
                    # enclosing () paired with -> Any
    -(X,Y)
    *(X,Y)
    /(X,Y)
end

