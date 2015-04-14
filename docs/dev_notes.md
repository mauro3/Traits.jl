Development notes
=================

Planned work
------------

- [ ] Making it easy to specify traits for datatype, see issue #1
- [ ] improve dispatch of traitfn, see issue #5

To ponder
---------

-   For many "traits" in Julia, only a few functions need to be
    implemented to provide many more.  For example for comparison only
    `isless` and `==` need to be implemented to automatically get `>`,
    `<`, `>=`, `<=`.  It would be nice to somehow specify or query those
    automatic functions.

-   Are there better ways for trait-dispatch?

-   Sometimes it would be good to get at type parameters, for instance
    for Arrays and the like:
    ```julia
    @traitdef Indexable{X{Y}} begin
        getindex(X, Any) -> Y
        setindex!(X, Y, Any) -> X
    end
    ```
    This problem is similar to triangular dispatch and may be solved
    by: https://github.com/JuliaLang/julia/issues/6984#issuecomment-49751358


Road blocks
-----------

### Expressiveness of types and how to describe method signatures

see branch m3/para-methods

The definition of traits hinges on being able to specify the types of
function arguments.  This is currently not possible because of
parameterized functions:
```julia
f{T}(a::Int, b::T, c::T) = ...
```

The signature of the function cannot be captured in a type-tuple,
instead it needs something like a constrained tuple:
```julia
{T}(a::Int, b::T, c::T)
```

Specifying a trait involves specifying method signatures
```julia
@traitdef Tr1{X} begin
    f{T}(a::X, b::T, c::T) = ...
end
```

which then need to be compared to the signatures of the method of `f`
for a specific `X`: for example, `istrait(Tr1{Int})`
would need to check whether at least one method signature `sig` of `f`
satisfies `tsig<:sig` where `tsig={T}(a::Int, b::T, c::T)` (i.e. for
all allowed types in the trait there is a method which fits).  Syntax
for this is being discussed in issue
[#6984](https://github.com/JuliaLang/julia/issues/6984#issuecomment-49804492).

Actually, I'm not sure whether `tsig<:sig` is right.  For above `Tr1`,
and a method with `sig=(a::Int, b, c)`, then the set of allowed type
signatures by `Tr1` is a subset of `sig`, thus `tsig={T}(a::Int, b::T,
c::T)<:(a::Int, b, c)=sig`.  However, I'm not sure that is what should
be intended by the traitdef of `Tr1`.  Conversely, for non-parametric
types, say `tsig=(a::Integer)` & `sig=(a::Real)` means `tsig<:sig`,
which should mean that a trait would be satisfied.

So, I think, for a trait-signature to be satisfied the following
condition need to hold:

- `tsig<:sig` for just the types themselves (sans parametric constraints)
- `tsig==sig` for the parametric constraints.  I.e. the constraints on `sig`
   need to be identical to `tsig`.

The reason for the second rule is: If the constraints are weaker on
`tsig` then on `sig`, it can happen that a argument tuple is not
accepted by the method with `sig` even though it would be on a method
with `tsig`.  Conversely, if the constrains are weaker on
`sig` then on `tsig`, then not all the trait-constraints are
fulfilled and thus the trait is not fulfilled.

Let's call this `tsig<<:sig`.

Compare this to ordinary method dispatch:
```
julia> g(a,b) = 1
g (generic function with 1 method)

julia> g{T}(a::T,b::T) = 2
g (generic function with 2 methods)

julia> g(5,6) # here the parametric constrained one gets called, as it is more specific
2

julia> g(5,6.)
1
```

This means `{I}(I, I)<:(Any, Any)` but not `(Any, Any)<:{I}(I, I)`,
which does not help for the traits-problem...  Having a trait:

```julia
@traidef Tr{X} begin
    g{T<:X}(T, T)
end
```

I think should mean that defining `g{T}(a::T,b::T) = 2` fulfils `Tr` for all
types but defining `g(a,b) = 1` does not.

How about
```Julia
@traidef Tr{X} begin
    g{T<:X}(T, T, Integer)
end
```
would `g{T<:Integer}(T, T, T)` fulfil `Tr{Integer}`?  Probably yes as
the parametric constraints are stronger than needed.

And what about the special-casing of dispatch of type parameters in
invariant positions?
```
f{T}(y::T, x::A{T}) = 1 # does match f(1, A{Number}())
```
I think there is nothing special about it for traits?



#### Ideas
Instead of using `method_exists` compare a fake method with the
methods at hand.  Also use a Type-variant method to encode the return
type:

Methods-cache may do something similar.
Base._methods (video at 28min)

#### References:
- https://github.com/JuliaLang/julia/issues/9043
-[Add syntactic sugar for covariant actual type parameters #6984](https://github.com/JuliaLang/julia/issues/6984)
- [use { } for tuple types? #8470](https://github.com/JuliaLang/julia/issues/8470)
- [WIP: redesign of tuples and tuple types #10380](https://github.com/JuliaLang/julia/pull/10380)
- Jeff's talk: "Introduction to Julia Internals"


### Return types

[Variance of function types](https://en.wikipedia.org/wiki/Covariance_and_contravariance_%28computer_science%29#Function_types)
suggests that "it is safe to substitute a function `f` instead of a
function `g` if `f` accepts a more general type of arguments and
returns a more specific type than `g`."  I.e. the `->` type constructor
is contravariant in the input type and covariant in the output type.

However, when using muliple dispatch,
[wikipedia says](https://en.wikipedia.org/wiki/Covariance_and_contravariance_%28computer_science%29#Avoiding_the_need_for_covariant_argument_types):
"... types used for runtime method selection are covariant while types
not used for runtime method selection of the method are
contravariant." So, bottom line is that Julia's generic functions are
*covariant* in both *argument types* and *return types*:
```
(X1,Y1)->Z1 <: (X2,Y2)->Z2
=>
(X1,Y1,Z1) <: (X2,Y2,Z2)
```

This means, that return types can be analysed in the same way as
argument types, at least in principle.  However, the devil may be in
the details as the return type of function can only be queried with
`Base.return_types`.  Example:
```julia
@traitdef Tr{X} begin
    g{T<:X}(::T, ::T) = T
end
g(::Int, ::Int) = Int
istrait(Tr{Int}) # == true
```


#### References:

- [adding a `@typeof f(..)`](https://github.com/JuliaLang/julia/issues/8027#issuecomment-52519612)
- [Wikipedia about variance](https://en.wikipedia.org/wiki/Covariance_and_contravariance_%28computer_science%29)

Monotonic return types:

- [julia-dev thread](https://groups.google.com/forum/#!msg/julia-dev/OGTUtAeozVw/cRQyuJQSFFgJ)
- [return type declarations #1090](https://github.com/JuliaLang/julia/issues/1090#issuecomment-35642896)

Ideas
-----
### Merging implicit interfaces specified on types with traits

see branch  m3/type-traits

How can traits be used for what types are used?  Say what is the
interface for an `AbstractArray`?  Say it is made up of a bunch of
single parameter traits, then `AbstractArray =>  BunchOfTraits{AbstractArray}`.
However, the converse is not necessarily true, namely when
`BunchOfTraits{X}` does not imply `X==AbstractArray`.

References:

- [Tim](https://github.com/JuliaLang/julia/pull/10458#issuecomment-77957672)
  about ambiguities and having general methods.
- [PR 10312](https://github.com/JuliaLang/julia/pull/10312) case about duck-typing
  maps and such as now many things are callable
- [Issue #5](https://github.com/JuliaLang/julia/issues/5#issuecomment-37901282)
  old discussion on multiple inheritance
- there is one discussion about the interface for AbstractArray
  [#10064](https://github.com/JuliaLang/julia/issues/10064) and related
  - [#987](https://github.com/JuliaLang/julia/issues/987)
  - [#9586](https://github.com/JuliaLang/julia/issues/9586)


- instead of creating a new trait, define one for an abstact type
- check for a concrete subtype will consist of checking that the trait
  is fulfilled.
- traitfns can use these type-traits `f{X; X<:AbstractArray}` instead
  of `f{X<:AbstractArray}` to check that all of the interfaces of its
  abstract super-types are fullfilled.

How is the istrait checking done?  Dispatch on type, if a DataType is
found look up the corresponding trait.

How are the traits stored?
- make a normal trait, say AbstractArrayTrait.  Insert them into
  `Traits` module, that way they will be available irrespective of
  where the original type was defined.


Todo:
- [ ] implement issue [#8](https://github.com/mauro3/Traits.jl/issues/8)
