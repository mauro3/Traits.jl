# Traits.jl

[![Build Status](https://travis-ci.org/mauro3/Traits.jl.svg?branch=master)](https://travis-ci.org/mauro3/Traits.jl)

`Traits.jl` allows to:

- define traits/interfaces with `@traitdef`

- implement interfaces with `@traitimpl`

- make functions which dispatch on traits with `@traitfn`

It's based on what I think traits should be:

1.  contracts on a type or a tuple of types.  The contract can contain
    required methods but also other assertions.  (Assertions could be
    that certain fields are present or that it has some storage
    structure, etc.)

2.  they needn't be declared explicitly, but can be.

3.  they allow *dispatch* to work with them

Julia's generic functions are very good to set up contracts as
mentioned in (1).  But Julia does not support (2) or (3) yet.  (2) is
fairly easy to implement.  However, dispatch on a "contract" is not
easily possible, but Tim Holy recently came up with
[a trick](https://github.com/JuliaLang/julia/issues/2345#issuecomment-54537633).
The cool thing about that trick is that the generated machine-code for
a trait-dispatch function should be identical to a duck-typed
function, i.e. there is no loss in performance.

`Traits.jl` adds those kind of traits to Julia, using Tim's trick
combined with stagedfunctions and extensive facilities to define
traits.  See also the Julia-issue
[#6975](https://github.com/JuliaLang/julia/issues/6975) concerning
interfaces/traits.

My [*JuliaCon 2015*](https://youtu.be/j9w8oHfG1Ic) talk gives a 10
minute introduction to Traits.jl.  Also, Jeff mentioned Traits.jl
during Q&A in his
[JuliaCon talk](https://youtu.be/xUP3cSKb8sI?t=45m51s), suggesting
that trait functionality may well be added to Julia-Base.

Example `examples/ex1.jl`:
```julia
using Traits
# Check Cmp-trait (comparison) which is implemented in Traits.jl/src/commontraits.jl
@assert istrait(Cmp{Int,Float64})        # Int and Float64 can be compared
@assert istrait(Cmp{Int,String})==false  # Int and String cannot be compared

# make a new trait and add a type to it:
@traitdef MyTr{X,Y} begin
    foobar(X,Y) -> Bool # All type-tuples for which there is a method foo
                        # with that signature belong to MyTr
end
type A
    a::Int
end
@assert istrait(MyTr{A,A})==false  # foobar not implement yet
foobar(a::A, b::A) = a.a==b.a      # implement it
@assert istrait(MyTr{A,A})         # voila!
@assert istrait(MyTr{Int,Int})==false

# make a function which dispatches on traits:
@traitfn ft1{X,Y; Cmp{X,Y}}(x::X,y::Y)  = x>y ? 5 : 6
@traitfn ft1{X,Y; MyTr{X,Y}}(x::X,y::Y) = foobar(x,y) ? -99 : -999

ft1(4,5)        # ==6    i.e. dispatches to first definition
ft1(A(5), A(6)) # ==-999 i.e. dispatches to second definition

ft1("asdf", 6)
# -> ERROR: TraitException("No matching trait found for function ft1")
```

# Package status

New features are documented in [NEWS](NEWS.md) as they are added.  I
keep some notes, musings and plans in [dev_notes.md](docs/dev_notes.md).

This is a fairly experimental package and I will not try to keep
backwards compatibility as I move on.  Please try it out and give me
feedback, issues or pull requests!

# Syntax
The source of below examples is in `examples/ex2.jl`.  Most of the
important functions are documented and will respond to `?` in the REPL.

Trait definition (for details see [traitdef.md](docs/traitdef.md)):
```julia
using Traits
# simple
@traitdef Tr1{X} begin
    fun1(X) -> Number   # this means a method with signature fun1(::X)
                        # returning a Number
end
@traitdef Tr2{X,Y} begin
    fun2(X,Y) -> Number
end
# subtrait
@traitdef Tr3{X,Y} <: Tr1{X}, Tr2{X,Y} begin
    fun3(X,Y,Int)
end
# with additional constraint on the types
@traitdef Tr4{X,Y} begin
    fun4(X,Y)
    @constraints begin
        # both Types need to start with the same letter:
        string(X.name)[1]==string(Y.name)[1]
    end
end

# using associated types
@traitdef Tr5{X,Y} begin
    Z = promote_type(X,Y) # calculates Z from X and Y
    fun5(X,Y) -> Z
end
```
Note that return-type checking is quite experimental.  It can be
turned off with `check_return_types(false)`.


Trait implementation and checking with `istrait`:
```julia
# manual definiton, i.e. just define the functions
fun1(x::Int) = 5x
@assert istrait(Tr1{Int})

# using @traitimpl
@traitimpl Tr1{Float64} begin
    fun1(x::Float64) = 7x # the explicit "::Float64" is needed at the moment
end
@assert istrait(Tr1{Float64})

# wrong usage of @traitimpl
try
    @traitimpl Tr1{Float32} begin
        fun1(x::Float64) = 7x # if the explicit type is wrong, it may error
    end
catch e
    println(e)
end

# This gives an error because supertypes have not been defined yet:
try
    eval(:(
    @traitimpl Tr3{Int, Int} begin
        fun3(x::Int, y::Int, t::Int) = x+y+t
    end))
catch e
    println(e)
end

# this works:
@traitimpl Tr2{Int, Int} begin
    fun2(x::Int, y::Int) = x+y
end
@traitimpl Tr3{Int, Int} begin
    fun3(x::Int, y::Int, t::Int) = x+y+t
end
@traitimpl Tr4{Int, Int} begin
    fun4(x::Int, y::Int) = x+y
end

# This gives an error because constraints are not satisfied:
# Int starts with an "I" whereas Float64 with an "F":
try
    eval(:(
    @traitimpl Tr4{Int, Float64} begin
        fun4(x::Int, y::Float64) = x+y
    end))
catch e
    println(e)  # ErrorException("assertion failed: istrait(Tr4{Int,Float64})")
end
```

Trait functions & dispatch (for details see [traitfns.md](docs/traitfns.md)):
```julia
@traitfn tf1{X, Y; Tr1{X}, Tr1{Y}}(a::X, b::Y) = fun1(a) + fun1(b)             # I
@traitfn tf1{X, Y; Tr1{X}, Tr1{Y}}(a::X, b::Y, c::Int) = fun1(a) + fun1(b) + c # II
@traitfn tf1{X, Y; Tr2{X,Y}}(a::X, b::Y) = fun2(a,b)                           # III
# Note that all the type-parameters are in the {} and that all
# arguments need a type parameter (a limitation of the
# macro-parser). This doesn't work:
#
# julia> @traitfn ttt1{X, Y; Tr1{X}, Tr1{Y}}(a::X, b::Y, c) = fun1(a) + fun1(b) + c
# ERROR: type Symbol has no field args
#
# But this works:
#
# julia> @traitfn ttt1{X, Y, Z; Tr1{X}, Tr1{Y}}(a::X, b::Y, c::Z) = fun1(a) + fun1(b) + c
# ttt1 (generic function with 6 methods)


# tf1 now dispatches on traits
@assert tf1(5.,6.)==77. # -> 77 ; dispatches to I because istrait(Tr1{Float64})
                        #         but not istrait(Tr2{Float64,Float64})
@assert tf1(5.,6.,77)==154. # -> 154. ; dispatches to II because of the extra argument

# Errors because of dispatch ambiguity:
try
    tf1(5,6)  # istrait(Tr1{Int}) and istrait(Tr2{Int,Int}) are both true!
catch e
    println(e)
end

# Implementing Tr1 for a type will make it work with tf1:
type MyType
    a::Int
end
try
    tf1(MyType(8), 9) # not implemented yet
catch e
    println(e)
end
@traitimpl Tr1{MyType} begin
    fun1(x::MyType) = x.a+9
end

@assert tf1(MyType(8), 9)==62 # -> 62 ; dispatches to I
```

# Generated code

Continuing the example from last section, let's have a look at the
llvm code:
```julia
f(x,y) = 7x + 7y
@code_llvm f(5.,6.)
@code_llvm tf1(5.,6.)
```
both produces
```
define double @"julia_f;41342"(double, double) {
top:
  %2 = fmul double %0, 7.000000e+00, !dbg !1388
  %3 = fmul double %1, 7.000000e+00, !dbg !1388
  %4 = fadd double %2, %3, !dbg !1388
  ret double %4, !dbg !1388
}
```

However, for more complicated functions code is not quite the same,
see `test/perf/perf.jl`.

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
His trick uses a method to assign its arguments to a trait.  That trait-function is then used
for dispatch in another function.  Example of Tim's trick (`examples/ex_tims_traits.jl`):
```julia
type Trait1 end
type Trait2 end
type Trait3 end
# Now define function f which should dispatch on those traits:
f(x,y) = _f(x,y, traitfn(x,y))
# Logic which dispatches on trait:
_f(x,y,::Type{Trait1}) = x+y
_f(x,y,::Type{Trait2}) = x-y
_f(x,y,::Type{Trait3}) = x*y
# Association of types with traits through method definitions:
# Throw error as default
traitfn{T,S}(x::T,y::S) = error("Function f not implemented for type ($T,$S)")
# Add types-tuples to Trait1, Trait2 or Trait3
traitfn(::Int, ::Int) = Trait1
traitfn(::Int, ::FloatingPoint) = Trait2
traitfn(::FloatingPoint, ::FloatingPoint) = Trait3
# use
@assert f(3,4)==7      # Trait1
@assert f(3,4.)==-1.0  # Trait2
@assert f(3.,4.)==12.0 # Trait3
# add another type-tuple to Trait3
traitfn(::String, ::String) = Trait3
@assert f("Lorem ", "Ipsum")=="Lorem Ipsum"
```

What does this add compared to what we had before with usual dispatch?
When a new type, say `A`, is created it can made to work with the
function `f` without needing to re-define `f` for that particular
type.  Instead all that is needed is to add it to the `traitfn`, and
choosing the exact behavior of `f` by the type `traitfn` returns:
```julia
traitfn(::A, ::Int) = Trait1()
traitfn(::Int, ::A) = Trait1()
```

Therefore `traitfn` is in effect a function that groups type-tuples
into different *Traits* (via method definitions) and returns the
appropriate type when called (which is then used inside `f` for
dispatch).  However, the limitation of this approach is that `traitfn`
is married to `f` as can be seen from trying to reuse it for another
function `g` which wants to dispatch in different traits:
```julia
g(x,y) = _g(x,y, traitfn(x,y))
# Logic which dispatches on trait:
_g(x,y,::Type{Trait1}) = 2x+2y
_g(x,y,::Type{Trait4}) = 2x-2y  # g doesn't care about Trait2&3 but about 4

# However, say Trait4 should also be implemented by {Int, FloatingPoint} just
# like Trait2:
traitfn(::Int, ::FloatingPoint) = Trait4 # this will overwrite the
                                         # Trait2 definition above!
g(5, 6.) # doesn't work
```

This limitation can be overcome having a different `traitfn` for each
function which uses trait dispatch.  However, it becomes rather tricky
to remember to update all different `traitfn`s if a type-tuple is
added to a certain trait!  This problem is solved in Traits.jl by
de-coupling the *trait definition* from the *trait dispatch* helper
function, both of which was done above by the `traitfn`.

Whether a trait is defined is checked by the `istrait` function
(completely independent of any functions doing trait-dispatch).  For
instance `istrait(Tr1{Int,Float64})` will check whether `Tr1` is
implemented by `Tuple{Int,Float64}`.


For the trait dispatch of a function, say `f1`, a generated-method is
used (which also belongs to the generic function `f1`, so I needn't
worry about scopes).  The first time the generated method is called
with arguments of a specific type, it figures out which trait or
traits-tuple featuring in the method definitions of `f1` that type
satisfies, and constructs a constant method returning that trait.
This trait is then used for dispatch.  Time for an example!

For methods definition like so
```julia
@traitfn f1{X,Y<:Integer; D1{Y}, D4{X,Y}}(x::X,y::Y) = x + sin(y)
@traitfn f1{S,T<:Integer; D1{S}, D1{T}  }(s::S,t::T) = sin(s) - sin(t)
@traitfn f1{X,Y<:FloatingPoint; D1{X}, D1{Y}  }(x::X,y::Y) = cos(x) - cos(y)
```
the underlying definitions are:
```julia
f1{X,Y<:Integer}(x::X, y::Y)       = f1(f1(_TraitDispatch,x, y), x, y)
f1{S,T<:Integer}(s::S, t::T)       = f1(f1(_TraitDispatch,s, t), s, t)
f1{X,Y<:FloatingPoint}(x::X, y::Y) = f1(f1(_TraitDispatch,x, y), x, y)

# The logic for different traits is:
@inline f1{X,Y<:Integer}(::Type{(D1{Y}, D4{X,Y})}, x::X, y::Y) = x + sin(y)
@inline f1{S,T<:Integer}(::Type{(D1{S}, D1{T})}, s::S, t::T) = sin(s) - sin(t)
@inline f1{X,Y<:FloatingPoint}(::Type{(D1{X}, D1{Y})}, x::X, y::Y) = cos(x) - cos(y)

# Trait dispatch happens in these generated functions
@generated function f1{X1,X2<:Integer}(::Type{_TraitDispatch}, x1::X1, x2::X2)
    # Figure out which trait matches.  Note below list is updated as more
    # trait-dispatched methods are added to f1.
    traittypes = [(D1{X2}, D4{X1,X2}), (D1{X1}, D1{X2})]

    # errors if not a single match is found:
    traittyp = Traits.traitdispatch(traittypes, $(fn.name))

    out = :(())
    for s in traittyp
        push!(out.args, :($s))
    end
    return out
end
# For each type signature there is a trait-dispatch function
@generated function f1{X1,X2<:FloatingPoint}(::Type{_TraitDispatch}, x1::X1, x2::X2)
...
end

```

Dispatch, happening in the function `Traits.traitdispatch` is quite
simple taking trait-hierarchies into account.  Although, note that it
is easily possible to have unsolvable ambiguities with trait-dispatch
as traits do not have a strict hierarchy like types.

# Other trait implementations

See the Julia-issue
[#6975](https://github.com/JuliaLang/julia/issues/6975) for a
discussion about interfaces/traits.

@Rory-Finnegan's
[Interfaces.jl](https://github.com/Rory-Finnegan/Interfaces.jl)

- does dispatch on traits
- only single parameter traits
- uses a new type of mutable `Union` coded in C

Jason Morton's package
[Typeclass.jl](https://github.com/jasonmorton/Typeclass.jl)

- does multiple parameters
- no dispatch

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


Graphs.jl: http://graphsjl-docs.readthedocs.org/en/latest/interface.html

- interface verification


@timholy's trick
https://github.com/JuliaLang/julia/issues/2345#issuecomment-54537633

- does limited dispatch: a function returns a true/false type
  depending on the input types
- Jeff suggested some additions to it.
