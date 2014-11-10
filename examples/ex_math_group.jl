# inspired by Jiahao Chen's post:
# https://groups.google.com/d/msg/julia-dev/iBBSQt1FGHE/KaXNq24Q7l8J

using Traits

@traitdef Group{X,Y} begin  # a trait defining a group
    @constraints begin
        X==Y
    end
    *(X,Y)   -> X
    *(X,Y,X) -> X
    one(X)   -> X
    inv(X)   -> X
end

@show istrait(Group{Int,Int}) # false
@show istrait(Group{Float64, Float64}) #true

#false, should be true:
@show istrait(Group{Array{Float64,2}, Array{Float64,2}}, verbose=true) 
# Because inv can error when matrix is not square.
# However, it does not complain about `one`?

@traitfn f{X; Group{X,X}}(x::X, y::X) = inv(x*y*y)

try
    f(5,6)
catch e
    println(e) # ERROR: TraitException("No matching trait found for function f")
end

@show f(5.,6.)

###
# Version 2: use only one parameter, maybe better?
###
@traitdef Group2{X} begin  # a trait defining a group
    *(X,X)   -> X
    *(X,X,X) -> X
    one(X)   -> X
    inv(X)   -> X
end

@show istrait(Group2{Int}) # false
@show istrait(Group2{Float64}) #true

#false, should be true:
@show istrait(Group2{Array{Float64,2}}, verbose=true) 
# Because inv can error when matrix is not square.
# However, it does not complain about `one`?

@traitfn f2{X; Group2{X}}(x::X, y::X) = inv(x*y*y)

try
    f2(5,6)
catch e
    println(e) # ERROR: TraitException("No matching trait found for function f")
end

@show f2(5.,6.)

