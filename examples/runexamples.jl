# check examples are running:
dirr = dirname(Base.source_path())
ls = split(readall(`ls $dirr`))

for l in ls
    ll = joinpath(dirr,l)
    if l[end-2:end]==".jl" && !(l=="runexamples.jl")
        println("Running example: $l")
        println("----------------")
        include(ll)
        println(" ")
        println("Finished example: $l")
        println("----------------")
        println(" ")
    end
end
