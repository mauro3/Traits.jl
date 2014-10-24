# tests
using Base.Test
using Traits


type A1 end
type A2 end
type T1
t1
end
type T2
t2
end

@test !istraittype(A1)

# manual implementations
include("manual-traitdef.jl")
include("manual-traitimpl.jl")
include("manual-traitdispatch.jl")

# test Traits.jl

include("traitdef.jl")
include("traitfns.jl")
include("traitdispatch.jl")
