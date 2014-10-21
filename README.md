# Traits.jl

`Traits.jl` allows to:

- define traits/interfaces with `@traitdef`

- implement interfaces with `@traitimpl` (not done yet...)

- make functions which dispatch on traits with `@traitfn`

It's based on what I think traits should be:

1.  contracts on a type or a tuple of types.  The contract can contain
    required methods but also other assertions (assertions are not
    implemented yet).
    Assertions could be that certain fields are present or that it has
    some storage structure, etc.
   
2.  they needn't be declared explicitly, but can be (explicit
    trait-implementation is not implemented yet).
   
3.  they allow *dispatch* to work with them

Julia's generic functions are very good to set up contracts
as mentioned in (1).  But Julia does not support (2) or (3) yet.  (2)
is fairly easy to implement.  However, dispatch on a "contract" is not
easily possible, but Tim Holy recently came up with
[a trick](https://github.com/JuliaLang/julia/issues/2345#issuecomment-54537633).

`Traits.jl` adds these features to Julia (well, (2) is still missing),
using Tim's trick combined with stagedfunctions.

Example:
```julia
# check some traits implemented in src/commontraits.jl
@assert traitcheck(Cmp{Int,Float64}) 
@assert traitcheck(Cmp{Int,String})==false

# make a new trait and add a type to it:
@traitdef MyTr{X,Y} begin
    foobar(X,Y) -> Bool
end
type A
    a
end
foobar(a::A, b::A) = a.a==b.a
@assert traitcheck(MyTr{A,A})  # true
@assert traitcheck(MyTr{Int,Int})==false

# make a function which dispatches on traits:
@traitfn ft1{X,Y; Cmp{X,Y}}(x::X,y::Y)  = x>y ? 5 : 6
@traitfn ft1{X,Y; MyTr{X,Y}}(x::X,y::Y) = foobar(x,y) ? -99 : -999

ft1(4,5)  # 6
ft1(A(5), A(6)) # -999

ft1("asdf", 6)
# -> ERROR: TraitException("No matching trait found for function ft1")
```

# Syntax

See `src/commontraits.jl`, and `tests/`.

# Inner workings

Julia is already good at specifying function-interfaces for
types/families of types with its ability of multiple dispatch.
However, for a particular type these function-interfaces are
implicitly defined by defining the particular functions needed for
that "interface".  For example, to support iteration a type needs to
implement the functions `start`, `next`, and `done` with a specific
calling convention.  What Julia is lacking is a way to formally
*define and implement an interface*, and, probably more importantly,
to *dispatch on interfaces*.  This package implements both of these
abilities.

Probably easiest to figure out what Traits.jl is doing with its
macros, is to have a look at the files `tests/manual-trait*.jl`.  There
I manually coded what the macros do.


## Dispatch on traits

In Julia dispatch works on types, to extend this to traits I use
@timholy's [trick](https://github.com/JuliaLang/julia/issues/2345#issuecomment-54537633).
His trick uses a function to check whether its input types satisfy
certain conditions (only dependent on their type) and returns one type
or another depending on the outcome.  That check-function is then used
for dispatch in another function.  Example:
```julia
f(x,y) = _f(x,y, checkfn(x,y))
_f(x,y,::Type{Trait1}) = x+y
_f(x,y,::Type{Trait2}) = x-y
_f(x,y,::Type{Trait3}) = x*y
# default
checkfn{T,S}(x::T,y::S) = error("Function f not implemented for type ($T,$S)")
# add types-tuples to checkfn-trait:
checkfn(::Int, ::Int) = Trait1
checkfn(::Int, ::FloatingPoint) = Trait2
checkfn(::FloatingPoint, ::FloatingPoint) = Trait3
```

What does this add compared to what we had before with usual dispatch?
When a new type, say `A`, is created it can made to work with the
function `f` without needing to re-define `f` for that particular
type.  Instead all that is needed is to add it to the `checkfn`, and
choosing the exact behavior of `f` by the type `checkfn` returns:
```julia
checkfn(::A, ::Int) = Trait1()
checkfn(::Int, ::A) = Trait1() 
```

Therefore `checkfn` is in effect a function that groups type-tuples
into different "Traits" (via method definitions) and returns the
appropriate type when called (which is then used inside `f` for
dispatch).

To implement traits in a generic fashion, I automated the definitions
of the `checkfn` functions using staged-functions.  Therefore the
staged part of `checkfn` figures out to what trait or traits-tuple a
type-tuple belongs to and creates a constant method for that type-tuple.
This is essentially doing dispatch on traits.

So for methods definition like so
```julia
@traitfn f1{X,Y<:Integer; D1{Y}, D4{X,Y}}(x::X,y::Y) = x + sin(y)
@traitfn f1{S,T<:Integer; D1{S}, D1{T}  }(s::S,t::T) = sin(s) - sin(t)
@traitfn f1{X,Y<:FloatingPoint; D1{X}, D1{Y}  }(x::X,y::Y) = cos(x) - cos(y)
```
the underlying definitions are:
```julia
f1{X,Y<:Integer}(x::X, y::Y)       = _trait_f1(x, y, _trait_type_f1(x,y) )
f1{S,T<:Integer}(s::S, t::T)       = _trait_f1(s, t, _trait_type_f1(s,t) )
f1{X,Y<:FloatingPoint}(x::X, y::Y) = _trait_f1(x, y, _trait_type_f1(x,y) )

# the logic is:
@inline _trait_f1{X,Y<:Integer}(x::X, y::Y, ::Type{(D1{Y}, D4{X,Y})}) = x + sin(y)
@inline _trait_f1{S,T<:Integer}(s::S, t::T, ::Type{(D1{S}, D1{T})}) = sin(s) - sin(t)
@inline _trait_f1{X,Y<:FloatingPoint}(x::X, y::Y, ::Type{(D1{X}, D1{Y})}) = cos(x) - cos(y)

stagedfunction _trait_type_f1{X1,X2<:Integer}{X1,X2}(x1::X1,x2::X2)
    # figure out which traits match:
    traittypes = [(D1{X2}, D4{X1,X2}), (D1{X1}, D1{X2})

    traittyp = Traits.traitdispatch(traittypes, $(fn.name))

    out = :(())
    for s in poss[1]
        push!(out.args, :($s))
    end
    return out
end
stagedfunction _trait_type_f1{X1,X2<:FloatingPoint}{X1,X2}(x1::X1,x2::X2)
...
end

```

Dispatch, happening in `Traits.traitdispatch` is quite simple but does
take trait-hierarchies into account.  Although, note that it is easily
possible to have unsolvable ambiguities with trait-dispatch as traits
do not have a strict hierarchy like types.

# Previous trait implementations
@pao's https://gist.github.com/pao/2432554

- simple
- no dispatch on trait

https://github.com/JuliaLang/julia/pull/7025
(and https://gist.github.com/tknopp/ed53dc22b61062a2b283)

- @tknopp
- interfaces are just added to types
- no dispatch on interfaces

https://gist.github.com/abe-egnor/503661eb4cc0d66b4489

- @abe-egnor
- no dispatch

https://github.com/abeschneider/TypeTraits.jl

- only does fields of types, as far as I can tell

@timholy's trick
https://github.com/JuliaLang/julia/issues/2345#issuecomment-54537633

- does limited dispatch: a function returns a true/false type
  depending on the input types
- Jeff suggested some additions to it.
