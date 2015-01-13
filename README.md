# Traits.jl

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
The cool thing about that trick is that the code for a trait-dispatch
function should be identical to a duck-typed function, i.e. there is
no loss in performance.

`Traits.jl` adds those kind of traits to Julia, using Tim's trick
combined with stagedfunctions.  See also the Julia-issue
[#6975](https://github.com/JuliaLang/julia/issues/6975) concerning
interfaces/traits.

Example:
```julia
using Traits
# Check Cmp-trait (comparison) which is implemented in src/commontraits.jl
@assert istrait(Cmp{Int,Float64}) 
@assert istrait(Cmp{Int,String})==false

# make a new trait and add a type to it:
@traitdef MyTr{X,Y} begin
    foobar(X,Y) -> Bool
end
type A
    a::Int
end
foobar(a::A, b::A) = a.a==b.a
@assert istrait(MyTr{A,A})  # true
@assert istrait(MyTr{Int,Int})==false

# make a function which dispatches on traits:
@traitfn ft1{X,Y; Cmp{X,Y}}(x::X,y::Y)  = x>y ? 5 : 6
@traitfn ft1{X,Y; MyTr{X,Y}}(x::X,y::Y) = foobar(x,y) ? -99 : -999

ft1(4,5)  # 6
ft1(A(5), A(6)) # -999

ft1("asdf", 6)
# -> ERROR: TraitException("No matching trait found for function ft1")
```

This is an experimental package and I will not try to keep backwards
compatibility as I move on.  But please give it a try in your code and
give feedback.  I will try to document the new features in [NEWS](NEWS.md).

# Syntax
(source in `examples/ex2.jl`)

Trait definition:
```julia
using Traits
# simple
@traitdef Tr1{X} begin
    fun1(X) -> Number
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

# using parametric trait. Note the nested curly
@traitdef SemiFunctor{X{Y}} begin
    fmap( Function, X{Y} } -> Any
end

```
Note that return-type checking is quite experimental.  It can be
turned off by defining `Main.Traits_check_return_types=false` before
`using Traits`.


Trait implementation:
```julia
# manual, i.e. just define the functions
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

# for parametric trait,
@traitimpl SemiFunctor{Nullable{T}} begin
    fmap{T}( f::Function, x::Nullable{T}) = isnull(x) ? Nullable() : Nullable(f(x.value))
end

# for Array, it is a bit difficult because the eltype is the first argument.
# Also note that this sample implementation won’t cover higher dimensions
@traitimpl SemiFunctor{Array{T...}} begin
    fmap{T}( f::Function, x::Array{T,1}) = map(f, x)
end
```

Trait functions & dispatch:
```julia
@traitfn tf1{X, Y; Tr1{X}, Tr1{Y}}(a::X, b::Y) = fun1(a) + fun1(b)
@traitfn tf1{X, Y; Tr2{X,Y}}(a::X, b::Y) = fun2(a,b)
# Note that all the type-parameters are in the {} and that all
# arguments need a type parameter (a limitation of the
# macro-parser). Bad examples are:
#
# julia> @traitfn ttt1{X, Y; Tr1{X}, Tr1{Y}}(a::X, b::Y, c) = fun1(a) + fun1(b) + c
# ERROR: type Symbol has no field args
#
# julia> @traitfn ttt1{X, Y; Tr1{X}, Tr1{Y}}(a::X, b::Y, c::Int) = fun1(a) + fun1(b) + c
# ERROR: X3 not defined
#
# But this works:
#
# julia> @traitfn ttt1{X, Y, Z; Tr1{X}, Tr1{Y}}(a::X, b::Y, c::Z) = fun1(a) + fun1(b) + c
# ttt1 (generic function with 6 methods)

 
# tf1 now dispatches on traits
tf1(5.,6.) # -> 77  (Float64 is part of Tr1 but not Tr2)

# Errors because of dispatch ambiguity:
try
    tf1(5,6)  # Int is part of Tr1{Int} and Tr2{Int, Int}
catch e
    println(e)
end

# adding a type to Tr1 will make it work with tf1:
type MyType
    a::Int
end
@traitimpl Tr1{MyType} begin
    fun1(x::MyType) = x.a+9
end

tf1(MyType(8), 9) # -> 62
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
see `test/traitdispatch.jl`.

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
for dispatch in another function.  Example of Tim's trick:
```julia
type Trait1 end
type Trait2 end
type Trait3 end
# now define function
f(x,y) = _f(x,y, checkfn(x,y))
_f(x,y,::Type{Trait1}) = x+y
_f(x,y,::Type{Trait2}) = x-y
_f(x,y,::Type{Trait3}) = x*y
# default
checkfn{T,S}(x::T,y::S) = error("Function f not implemented for type ($T,$S)")
# add types-tuples to Trait1, Trait2 or Trait3:
checkfn(::Int, ::Int) = Trait1
checkfn(::Int, ::FloatingPoint) = Trait2
checkfn(::FloatingPoint, ::FloatingPoint) = Trait3
# use
f(3,4)  # 7
f(3,4.) # -1.0
f(3.,4.) # 12.0
# add another type-tuple to Trait3
checkfn(::String, ::String) = Trait3
f("Lorem ", "Ipsum") # "Lorem Ipsum"
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
f1{X,Y<:Integer}(x::X, y::Y)       = f1(f1(_TraitDispatch,x, y), x, y)
f1{S,T<:Integer}(s::S, t::T)       = f1(f1(_TraitDispatch,s, t), s, t)
f1{X,Y<:FloatingPoint}(x::X, y::Y) = f1(f1(_TraitDispatch,x, y), x, y)

# the logic is:
@inline f1{X,Y<:Integer}(::Type{(D1{Y}, D4{X,Y})}, x::X, y::Y) = x + sin(y)
@inline f1{S,T<:Integer}(::Type{(D1{S}, D1{T})}, s::S, t::T) = sin(s) - sin(t)
@inline f1{X,Y<:FloatingPoint}(::Type{(D1{X}, D1{Y})}, x::X, y::Y) = cos(x) - cos(y)

stagedfunction f1{X1,X2<:Integer}(::Type{_TraitDispatch}, x1::X1, x2::X2)
    # figure out which traits match:
    traittypes = [(D1{X2}, D4{X1,X2}), (D1{X1}, D1{X2})]

    traittyp = Traits.traitdispatch(traittypes, $(fn.name))

    out = :(())
    for s in poss[1]
        push!(out.args, :($s))
    end
    return out
end
stagedfunction f1{X1,X2<:FloatingPoint}(::Type{_TraitDispatch}, x1::X1, x2::X2)
...
end

```

Dispatch, happening in `Traits.traitdispatch` is quite simple taking
trait-hierarchies into account.  Although, note that it is easily
possible to have unsolvable ambiguities with trait-dispatch as traits
do not have a strict hierarchy like types.

# To ponder

-   For many "traits" in Julia, only a few functions need to be
    implemented to provide many more.  For example for comparison only
    `isless` and `==` need to be implemented to automatically get `>`,
    `<`, `>=`, `<=`.  It would be nice to somehow specify or query those
    automatic functions.

-   Are there better ways for trait-dispatch?

-   Issues related to parametric trait:
    * Triangular dispatch:
     https://github.com/JuliaLang/julia/issues/6984#issuecomment-49751358

# Issues


# Other trait implementations

See the Julia-issue
[#6975](https://github.com/JuliaLang/julia/issues/6975) for a
discussion about interfaces/traits.

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

