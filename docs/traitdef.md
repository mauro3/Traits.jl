About trait definitions
=======================

It turns out that trait definitions based on methods signatures are a
rather tricky business because Julia allows for quite intricate method
definitions.

For a generic function `f` and a trait definition
```julia
@traitdef Tr{X} begin
    f{...}(...) -> ...
end
```
what does it mean that `istrait(Tr{T})==true` for some type `T`?
First a slight detour on the meaning of  `{...}(...)`, this is
essentially a type tuple with constraints on the actual types in
`(...)` in `{...}`.  Inside a `Method` `m` these two parts are stored
in `m.tvars` (the `{}`) and `m.sig` (the `()`), which I will use below.


What I implemented (discounting bugs) are the following rules:

## Method call signature

The method call signature, the `{...}(...)` <=> `tm.tvars`, `tm.sig`
part of above definition, is satisfied if at least one method `fm` of
generic function `f` satisfies:

A) `tm.sig<:fm.sig` i.e. just the type tuple of the trait-method is a
   subtype of the generic-fn-method.

B) The parametric constraints parameters on `fm.sig` and `tm.sig` need
   to feature in the same argument positions.  Except when the
   corresponding function parameter is constraint by a concrete type:
   then make sure that all the occurrences are the same concrete type.

So, as long as neither the trait-method nor the generic-fn-method has
any parametric constraints, it's easy. It's just the subtyping
relation between the two.  However, once parametric constraints are
use on either or both then it is complicated.

Examples

The same constraints on both methods:
```julia
@traitdef Pr0{X} begin
    fn75{Y <: Integer}(X, Y)
end
fn75{Y<:Integer}(x::UInt8, y::Y) = y+x
@test istrait(Pr0{UInt8})
````

Only the last constraint is general enough to assure `fn77` will be
callable for all `X` which are `Pr2{X}`:
```julia
@traitdef Pr2{X} begin
    fn77{Y<:Number}(X,Y,Y)
end
fn77(a::Array,b::Int, c::Float64) = a[1]
@test !istrait(Pr2{Array})
fn77{Y<:Real}(a::Array,b::Y, c::Y) = a[1]
@test !istrait(Pr2{Array})
fn77{Y<:Number}(a::Array,b::Y, c::Y) = a[1]
@test istrait(Pr2{Array})
```

A trait-method with constraints can be implemented with a method
without constraints for a concrete type:
```julia
@traitdef Pr3{X} begin
    fn78{T<:X}(T,T)
end
fn78(b::Int, c::Int) = b
# This works because fn78 can be called for all arguments (Int,):
@test istrait(Pr3{Int})

fn78(b::Real, c::Real) = b
# This fails because the call fn78(5, 6.) is possible but not allowed
# by Pr3:
@test !istrait(Pr3{Real})
# Now all good:
fn78{T}(b::T, c::T) = b
@test istrait(Pr3{Real})
@test istrait(Pr3{Any})
```

The other way around is similar
```julia
@traitdef Pr07{X} begin
    fnpr07(X, X, Integer) # no parametric-constraints
end
fnpr07{T<:Integer}(::T, ::T, ::Integer) = 1
# This is not true as for instance a call fnpr07(8, UInt(8)) would fail:
@test !istrait(Pr07{Integer})
# This is fine as any call (Int,Int) will succeed:
@test istrait(Pr07{Int})
```

There are a lot more examples in `test/traitdef.jl`.  Most of this
functionality is implemented in the `isfitting` function.

## Method return signature

The specifed return type in the `@traitdef` (`tret`) and return-type
interfered with `Base.treturn_types` of the generic function has to be
`fret<:tret`.  Note that this is backwards to argument types checking
above, which makes sense as the variance of functions arguments and
return types is different.

Note that currently there is no check that the method which satisfies
the 'method call signature' test is the same method which satisfies
the 'return signature' test.
