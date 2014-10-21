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

@test !istrait(A1)
@test !istrait(Traits.NoTrait)
@test !traitcheck(Traits.NoTrait)

# manual implementations
include("manual-traitdef.jl")
include("manual-traitimpl.jl")
include("manual-traitdispatch.jl")

# test Traits.jl

include("traitdef.jl")
include("traitfns.jl")
include("traitdispatch.jl")
