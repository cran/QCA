`getSolution` <-
function(expressions, collapse, uplow, use.tilde, inputt, row.dom, initial, all.sol) {
        
    PI <- writePrimeimp(expressions, collapse=collapse, uplow=uplow, use.tilde=use.tilde)
    rownames(expressions) <- PI
    
    PI.sort <- sortVector(PI, collapse=collapse)
    
    expressions <- expressions[match(sortVector(PI.sort, collapse=collapse), PI), , drop=FALSE]
    rownames(expressions) <- PI.sort
    
     # create the prime implicants chart
    mtrx <- createChart(expressions, inputt)
    
    reduced <- list(expressions = expressions, mtrx = mtrx)
    
    if (row.dom) {
        reduced.rows <- rowDominance(mtrx)
        if (length(reduced.rows) > 0) {
            reduced$mtrx <- mtrx[reduced.rows, ]
            reduced$expressions <- expressions[reduced.rows, ]
        }
    }
    
    PI.red <- writePrimeimp(reduced$expressions, collapse=collapse, uplow=uplow, use.tilde=use.tilde)
    PI.red.sort <- sortVector(PI.red, collapse=collapse)
    
    mtrx <- reduced$mtrx[match(PI.red.sort, PI.red), , drop=FALSE]
    
    rownames(mtrx) <- PI.red.sort
    colnames(mtrx) <- initial
    
    # other.args <- list(...)
    # if ("bla" %in% names(other.args)) {
        # return(list(mtrx=mtrx, all.sol=all.sol))
        # return(list(RE=reduced$expressions, AP = all.PIs))
        # print(sortVector(rownames(reduced$expressions)[rownames(reduced$expressions) %in% all.PIs], collapse=collapse))
        
    # }
    
    sol.matrix <- solveChart(mtrx, all.sol = all.sol)
    
    sol.matrix <- matrix(rownames(mtrx)[sol.matrix], nrow=nrow(sol.matrix))
    
    all.PIs <- sortVector(unique(as.vector(sol.matrix[!is.na(sol.matrix)])), collapse=collapse)
    
    reduced$expressions <- reduced$expressions[sortVector(rownames(reduced$expressions)[rownames(reduced$expressions) %in% all.PIs], collapse=collapse), , drop=FALSE]
    
    all.PIs <- all.PIs[all.PIs %in% rownames(mtrx)]
    
    solution.list <- writeSolution(sol.matrix, mtrx)
    
    return(list(mtrx=mtrx, reduced=reduced, expressions=expressions, all.PIs=all.PIs, solution.list=solution.list))
}
