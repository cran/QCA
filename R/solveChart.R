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
         ## solution provided by Gabor Grothendieck
         ## the function lp (from package lpSolve) finds a (guaranteed) minimum solution
         # k will be the minimum number of prime implicants necessary to cover all columns
        k <- ceiling(sum(lp("min", rep(1, nrow(chart)), t(chart), ">=", 1)$solution))
        
         # Stop if the matrix with all possible combinations of k PIs has over 2GB of memory
        if ((mem <- nrow(chart)*choose(nrow(chart), k)*8/1024^3) > 2) {
            cat("\n")
            stop(paste(paste("Error: Too much memory needed (", round(mem, 1), " GB) to solve the PI chart using combinations of", sep=""),
                             k, "PIs out of", nrow(chart), "minimised PIs.\n\n"), call. = FALSE)
        }
        
         # create a matrix with all possible combinations of k prime implicants
        combos <- combn(nrow(chart), k)
        
         # sol.matrix will be a subset of the chart matrix with all minimum solutions
        output <- combos[, apply(combos, 2, function(idx) all(colSums(chart[idx, , drop=FALSE]) > 0)), drop=FALSE]
        
        if (all.sol & k < nrow(chart)) {
            
            output.all <- list()
            output.all[[1]] <- output
            
            for (i in seq(k + 1, nrow(chart))) {
                combos <- combn(nrow(chart), i)
                
                combos <- combos[, apply(combos, 2, function(idx) all(colSums(chart[idx, , drop=FALSE]) > 0)), drop=FALSE]
                
                ## it is basically impossible to already be a solution, since "k + 1" is used
                
                for (j in seq(length(output.all))) {
                    already.sol <- apply(combos, 2, function(idx) {
                        any(apply(output.all[[j]], 2, function(outx) {
                            all(is.element(outx[!is.na(outx)], idx))
                        }))
                    })
                    
                    combos <- combos[, !already.sol, drop = FALSE]
                }
                  
                if (ncol(combos) > 0) {
                    output.all[[length(output.all) + 1]] <- combos
                }
            }
            
            
            if (length(output.all) > 1) {
                max.nrows <- max(unlist(lapply(output.all, nrow)))
                output.all <- lapply(output.all, function(x) {
                    apply(x, 2, function(y) {c(y, rep(NA, max.nrows - length(y)))})
                })
                
                output <- output.all[[1]]
                
                for (i in seq(2, length(output.all))) {
                    output <- cbind(output, output.all[[i]])
                }
            }
            
        }
    }
    else {
        output <- matrix(seq(nrow(chart)))
    }
    
    return(matrix(row.numbers[output], nrow=nrow(output)))
}

