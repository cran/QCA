`solveChart` <-
function(chart) {
    if (!is.logical(chart)) {
        cat("\n")
        stop("Use a T/F matrix. See demoChart's output.\n\n", call. = FALSE)
    }
    
    output <- list()
    
    if (all(dim(chart) > 1)) {
         ## solution provided by Gabor Grothendieck
         ## the function lp (from package lpSolve) finds a (guaranteed) minimum solution
         # k will be the minimum number of prime implicants necessary to cover all columns
        output$k <- ceiling(sum(lp("min", rep(1, nrow(chart)), t(chart), ">=", 1)$solution))
        
         # Stop if the matrix with all possible combinations of k PIs has over 2GB of memory
        if ((mem <- nrow(chart)*choose(nrow(chart), output$k)*8/1024^3) > 2) {
            cat("\n")
            stop(paste(paste("Error: Too much memory needed (", round(mem, 1), " GB) to solve the PI chart using combinations of", sep=""),
                             output$k, "PIs out of", nrow(chart), "minimised PIs.\n\n"), call. = FALSE)
        }
        
         # create a matrix with all possible combinations of k prime implicants
        combos <- combn(nrow(chart), output$k)
        
         # sol.matrix will be a subset of the chart matrix with all minimum solutions
        output$sol.matrix <- combos[, apply(combos, 2, function(idx) all(colSums(chart[idx, , drop=FALSE]) > 0)), drop=FALSE]
    }
    else {
        output$k <- 1
        output$sol.matrix <- matrix(seq(nrow(chart)))
    }
    return(output)
}

