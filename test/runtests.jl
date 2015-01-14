# tests
using Base.Test
using Traits


type A1 end
type A2 end

@test !istraittype(A1)

# BUG flags: set to false once fixed to activate tests
method_exists_bug = true # see https://github.com/JuliaLang/julia/issues/8959
method_exists_bug2 = false # see https://github.com/JuliaLang/julia/issues/9043

# manual implementations
include("manual-traitdef.jl")
include("manual-traitimpl.jl")
include("manual-traitdispatch.jl")

# test Traits.jl
#include("helpers.jl")
include("traitdef.jl")
include("traitfns.jl")
include("traitdispatch.jl")
include("paramtraits.jl")
