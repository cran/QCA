`solveChart` <-
function(chart) {
    if (!is.logical(chart)) {
        cat("\n")
        stop("Use a T/F matrix. See demoChart's output.\n\n", call. = FALSE)
    }
    
    if (all(dim(chart) > 1)) {
         ## solution provided by Gabor Grothendieck
         ## the function lp (from package lpSolve) finds a (guaranteed) minimum solution
         # k will be the minimum number of prime implicants necessary to cover all columns
        k <- ceiling(sum(lp("min", rep(1, nrow(chart)), t(chart), ">=", 1)$solution))
        
         # Stop if the matrix with all possible combinations of k PIs has over 1GB of memory
        if (nrow(chart)*choose(nrow(chart), k)*8/1024^3 > 1) {
            cat("\n")
            stop("Impossible to solve the PI chart (too many possible combinations).\n\n", call. = FALSE)
        }
        
         # create a matrix with all possible combinations of k prime implicants
        combos <- combn(nrow(chart), k)
        
         # sol.matrix will be a subset of the chart matrix with all minimum solutions
        sol.matrix <- combos[, apply(combos, 2, function(idx) all(colSums(chart[idx, , drop=FALSE]) > 0))]
    }
    else {
        sol.matrix <- 1:nrow(chart)
    }
    return(sol.matrix)
}

