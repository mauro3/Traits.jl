# Example showing how dispatch ambiguities with traits compare to the normal ones.

println("""
## Normal dispatch
##################
""")

f(x,y::Int) = x-y
f(x::Int,y) = x+y # ->
# Warning: New definition 
#     f(Int64,Any) at none:1
# is ambiguous with: 
#     f(Any,Int64) at none:1.
# To fix, define 
#     f(Int64,Int64)
# before the new definition.
# f (generic function with 2 methods)

@show f(5,6) # picks one method definition at random

# fix ambiguity:
f(x::Int, y::Int) = x*y
@show f(5,6)


println("""

## Trait dispatch ambiguities
#############################
""")
using Traits
println(stringmime("text/plain", Docs.doc(Traits.traitdispatch)))

println("""

## case 1: similar to normal dispatch

""")

@traitdef TrTr1{X} begin
    len1(X) 
end

@traitfn tf{X<:Integer,Y; TrTr1{X}}(x::X,y::Y) = x-y
@traitfn tf{X<:Integer,Y; TrTr1{Y}}(x::X,y::Y) = x+y

@traitimpl TrTr1{Int} begin
    len1(x::Int) = 5
end

# now ambiguous:
try
    tf(5,6)  # actually errors, as opposed to randomly choosing one method
catch e
    println(e)
end

# resolve as above does not work:
@traitfn tf{X<:Integer,Y; TrTr1{X}, TrTr1{Y}}(x::X,y::Y) = x*y
tf(5,6)

# another way to resolve it would be:
@traitdef TrTr22{X,Y} <: TrTr1{X}, TrTr1{Y} begin
    # empty
end

@traitfn tf{X<:Integer,Y; TrTr22{X,Y}}(x::X,y::Y) = x*y*1000
try
    tf(5,6)  # but now errors again as it is ambiguous again
catch e
    println(e)
end

###########
println("""

## case 2: with just one argument

""")
# This case does not really exist for normal types, where there can
# never be ambiguities in methods with one argument.

@traitdef TrTr2{X} begin
    len2(X) 
end

@traitfn tttf{X; TrTr1{X}}(x::X) = len1(x)
@traitfn tttf{X; TrTr2{X}}(x::X) = len2(x)

@show tttf(5)  # works as TrTr2{Int} is not a trait

@traitimpl TrTr1{Float64} begin
    len1(x::Float64) = 5
end
@traitimpl TrTr2{Float64} begin
    len2(x::Float64) = 55
end

try
    tttf(5.)  # errors
catch e
    println(e)
end

# this can be resolved by making a method which dispatches on both types:
@traitfn tttf{X; TrTr1{X}, TrTr2{X}}(x::X) = len2(x) # pick len2 over len1

@show tttf(5.) # now works

# this could also be resolved by adding a sub-trait:
@traitdef STrTr{X} <: TrTr1{X}, TrTr2{X} begin
    # empty
end

# no need to implement it for Float64: it's already a member:
@assert istrait(STrTr{Float64})

@traitfn tttf{X; STrTr{X}}(x::X) = len2(x) # pick len2 over len1
try
    tttf(5.)  # but now errors again as is ambiguous with above new definition
catch e
    println(e)
end
