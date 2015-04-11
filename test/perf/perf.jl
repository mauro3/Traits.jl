module TraitPerf
# some performance comparisons
using Traits
using Base.Test

# helpers:
println("""\n\nRunning performance tests.  There will be some warnings here, ignore them:\n""")

# from julia/test/perf/perfutil.jl
ntrials = 50
macro timeit(ex)
    quote
        out, time, mem, _ = 0.0, 0.0, 1, 0.0
        t = zeros(Float64, ntrials)
        m = zeros(Int, ntrials)
        for i=0:ntrials
            gc()
            out, time, mem, _ = @timed $(esc(ex))
            if i > 0
                # warm up on first iteration
                t[i] = time
                m[i] = mem
            end
        end
        mean(t), mean(m)
    end
end

function compare_code_native(f1, f2, types, fraction_range=[-Inf,Inf])
    # A very crude way to compare whether two functions produce the
    # same machine code: compare their lengths.  Returns the relative
    # difference of the length of code_native of two functions.
    #
    # If fraction_range is specified, print warning if outside that range.
    #
    # Note, when running with --code-coverage the traits-functions
    # will be much longer than the duck-typed ones.

    function prune_native(st)
        sts = split(st, "\n")
        out = ""
        for s in sts[3:end]     # the first two lines have some file names, etc
            if startswith(s, "Source line:")
                continue
            end
            out *= s
        end
        out
    end
    
    df1 = prune_native(Base._dump_function(f1, types, true, false, true))
    df2 = prune_native(Base._dump_function(f2, types, true, false, true))
    rel_diff = abs(length(df1)-length(df2))/length(df2)
    if !(fraction_range[1]<=rel_diff<=fraction_range[2])
        println("""Warning: length of code native of $(f1.env.name) and $(f2.env.name) differ by $rel_diff, 
                which is outside the specified range: $fraction_range.""")
    end
    return rel_diff
end

include("traits_vs_ducks.jl")

# load all examples from
dispatch_bug1 = true # needs to be synchronized with ../runtests.jl:dispatch_bug1
include("../traitdispatch.jl")

println("""\n\nAny warnings following this line can indicate code-generation problems:""")
# the bounds used below are from my Linux i7, Julia commit 01a3216*,
# Traits commit 6d9cc07e9

# checks the generated code is within some % of each other
# @code_native ft1(4,5)
# @code_native ff1(4,5)
compare_code_native(ff1, ft1, (Int,Int), [0.0,0.0])
compare_code_native(ff1, ft1, (BigFloat,BigInt), [0.0, 0.0])

# @code_llvm ft2(7.3,5.)
# @code_llvm ff2(7.3,5.)
compare_code_native(ff2, ft2, (Int,Int), [ 0.0, 0.0])
compare_code_native(ff2, ft2, (BigFloat,BigInt), [0.0, 0.0])

# @code_llvm ft3([1:10],5)
# @show "---------------------------------------------------"
# @code_llvm ff3([1:10],5)
compare_code_native(ff3, ft3, (Array{Int,1},Int), [0.025, 0.03])
compare_code_native(ff3, ft3, (Array{BigFloat,1},BigInt), [0.18, 0.19])

end
