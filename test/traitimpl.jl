# Testing @traitimpl
fun1(x,y) = error("not implemented")
fun2(x) = error("not implemented")
fun3(y) = error("not implemented")
@traitdef Tr100{X,Y} begin
    fun1(X,Y)
    fun2(X)
    fun3(Y)
end
type A100
end
type B100
end
# manual implementation:
fun1(a::A100, b::B100) = 5
fun2(a::A100) = 6
fun3(b::B100) = 7

head = :(Tr100{A100,B100})

body = quote
    fun1(a::A100, b::B100) = 5
    fun2(a::A100) = 6
    fun3(b::B100) = 7
end
trait = Tr100{A100,B100}

sigs = Any[
        Any[:(a::A100), :(b::B100)],
        Any[:(a::A100)],
        Any[:(b::B100)]
        ]
i = 1
for ln in enumerate(body.args)
    fn = ln[2]
    if isa(fn, Expr) && !(fn.head==:line)
        @test Traits.get_fsig(fn)==sigs[i]
        i+=1
    end
end

implfs = Traits.parse_body(body)
@test Traits.check_macro_body(body.args, implfs, trait)

# check @traitimpl
type A110
end
type B110
end
#  implementation
@traitimpl Tr100{A110,B110} begin
    fun1(a::A110, b::B110) = 5
    fun2(a::A110) = 6
    fun3(b::B110) = 7
end
