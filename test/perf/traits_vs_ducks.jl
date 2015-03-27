# Compare performance of a duck-typed function with trait-dispatched function

## a simple function:
@traitfn function ft2{X,Y; Arith{X,Y}}(x::X,y::Y) 
    out = zero(promote(x,y)[1])
    for xe in 1:round(Int,x)
        out += xe + y
    end
    out
end
function ff2{X,Y}(x::X,y::Y) 
    out = zero(promote(x,y)[1])
    for xe in 1:round(Int,x)
        out += xe + y
    end
    out
end
        

n = 100000
x = rand(n)
y = rand(n)

function gt()
    out = 0.0
    for i=1:n
        ft2(x[i],y[i])
    end
    out
end
function gg()
    out = 0.0
    for i=1:n
        ff2(x[i],y[i])
    end
    out
end


gc()
t, m = @timeit gg()
println("Duck-typed function:  time $(round(t,5))s, mem $(round(Int,m/1e6))MB")

gc()

t, m = @timeit gt()
println("Trait-typed function: time $(round(t,5))s, mem $(round(Int,m/1e6))MB")
## 
