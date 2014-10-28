function eval_curmod(expr::Union(Symbol,Expr,QuoteNode))
    # evaluates a symbol or expression in the current module.
    # I.e. the one where the macro definition is.
    return eval(current_module(),expr)
end

# to iterate over code blocks without the line-number bits:
immutable Lines
    block::Vector{Any}
end

Base.start(lns::Lines) = 1
function Base.next(lns::Lines, nr)
    for i=nr:length(lns.block)
        if isa(lns.block[i], Expr) && !(lns.block[i].head==:line)
            return lns.block[i], i+1
        end
    end
    return -1
end
function Base.done(lns::Lines, nr)
    if next(lns::Lines, nr)==-1
        true
    else
        false
    end
end
