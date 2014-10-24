function eval_curmod(expr::Union(Symbol,Expr,QuoteNode))
    # evaluates a symbol or expression in the current module.
    # I.e. the one where the macro definition is.
    return eval(current_module(),expr)
end
