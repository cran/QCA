`solveChart` <-
function(chart, row.dom = FALSE, all.sol = FALSE) {
    if (!is.logical(chart)) {
        cat("\n")
        stop("Use a T/F matrix. See demoChart's output.\n\n", call. = FALSE)
    }
    
    if (all.sol) {
        row.dom <- FALSE
    }
    
    row.numbers <- seq(nrow(chart))
    
    if (row.dom) {
        row.numbers <- rowDominance(chart)
        chart <- chart[row.numbers, ]
    }
    
    output <- list()
    
    if (all(dim(chart) > 1)) {
         ## initial solution provided by Gabor Grothendieck
         ## the function lp (from package lpSolve) finds a (guaranteed) minimum solution
         # k will be the minimum number of prime implicants necessary to cover all columns
        k <- ceiling(sum(lp("min", rep(1, nrow(chart)), t(chart), ">=", 1)$solution))
        # cat(paste("k: ", k, "\n"))
        
         # Stop if the matrix with all possible combinations of k PIs has over 2GB of memory
        if ((mem <- nrow(chart)*choose(nrow(chart), k)*8/1024^3) > 2) {
            cat("\n")
            stop(paste(paste("Error: Too much memory needed (", round(mem, 1), " GB) to solve the PI chart using combinations of", sep=""),
                             k, "PIs out of", nrow(chart), "minimised PIs.\n\n"), call. = FALSE)
        }
        
        if (all.sol & k < nrow(chart)) {
            
            output <- .Call("allSol", k, chart*1, PACKAGE="QCA")
            
            output[output == 0] <- NA
            
        }
        else {
            
             # create a matrix with all possible combinations of k prime implicants
            combos <- combn(nrow(chart), k)
            
             # sol.matrix will be a subset of the chart matrix with all minimum solutions
            output <- combos[, as.logical(.Call("solveChart", t(combos) - 1, chart*1, PACKAGE="QCA")[[1]]), drop=FALSE]
        }
    }
    else {
        output <- matrix(seq(nrow(chart)))
        
        if (ncol(chart) == 1) {
            output <- t(output)
        }
    } 
    
    # just in case row dominance was applied
    return(matrix(row.numbers[output], nrow=nrow(output)))
}

