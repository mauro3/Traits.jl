# tests
using Base.Test
using Traits
## BUG flags: set to false once fixed to activate tests
# Julia issues:
method_exists_bug1 = false # see https://github.com/JuliaLang/julia/issues/8959
method_exists_bug2 = false # see https://github.com/JuliaLang/julia/issues/9043 and https://github.com/mauro3/Traits.jl/issues/2
function_types_bug1 = true # set to false if function types get implemented in Julia
# Traits.jl issues:
dispatch_bug1 = true # in traitdispatch.jl
concrete_type_bug = true

# src/Traits.jl tests
type A1 end
type A2 end
@test !istraittype(A1)

# istrait helper function:
@test Traits.subs_tvar(TypeVar(:I), Array{TypeVar(:I,Int64)}, Traits._TestType{1})==Array{TypeVar(:I,Int64)}
@test Traits.subs_tvar(TypeVar(:I,Int64), Array{TypeVar(:I,Int64)}, Traits._TestType{1})==Array{Traits._TestType{1}}
@test Traits.subs_tvar(TypeVar(:T), Array, Traits._TestType{1})==Array{Traits._TestType{1}}  # this is kinda bad
f8576{T}(a::Array, b::T) = T
other_T = f8576.env.defs.tvars
@test Traits.subs_tvar(other_T, Array, Traits._TestType{1})==Array # f8576.env.defs.tvars looks like TypeVar(:T) but is different!

@test Traits.find_tvar( (Array, ), TypeVar(:T))==[true]
@test Traits.find_tvar( (Array, ), other_T)==[false]
@test Traits.find_tvar( (Int, Array, ), TypeVar(:T))==[false,true]

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
