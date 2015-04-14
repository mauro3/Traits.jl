### Trait definitions
#####################
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

### Trait implementation
########################

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
    println(e) # TraitException("Not all supertraits of Tr3{Int64,Int64} are implemented.\nImplement them first.")
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

# This gives an error because constraints are not satisfied
try
    eval(:(
    @traitimpl Tr4{Int, Float64} begin
        fun4(x::Int, y::Int) = x+y
    end))
catch e
    println(e)  # ErrorException("assertion failed: istrait(Tr4{Int,Float64})")
end

### Trait functions
###################

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

### Generated machine code
##########################
f(x,y) = 7x + 7y
@code_llvm f(5.,6.)
@code_llvm tf1(5.,6.)
# It's the same.
