# tests
using Base.Test
using Traits


type A1 end
type A2 end

@test !istraittype(A1)

## BUG flags: set to false once fixed to activate tests
# Julia issues:
method_exists_bug1 = true # see https://github.com/JuliaLang/julia/issues/8959
method_exists_bug2 = true # see https://github.com/JuliaLang/julia/issues/9043 and https://github.com/mauro3/Traits.jl/issues/2
# Traits.jl issues:
dispatch_bug1 = true # in traitdispatch.jl 

# manual implementations
include("manual-traitdef.jl")
include("manual-traitimpl.jl")
include("manual-traitdispatch.jl")

# test Traits.jl
#include("helpers.jl")
include("traitdef.jl")
include("traitfns.jl")
include("traitdispatch-manual-vs-auto.jl")
include("traitdispatch.jl")

# Run the performance tests as well.
include("perf/perf.jl")
