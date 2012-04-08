 # function to create a list which will contain the solution(s) and the essential prime implicants

`writeSolution` <- 
function(sol.matrix, mtrx) {
    
    solution <- output <- list()
    row.matrix <- matrix(FALSE, nrow=nrow(mtrx), ncol=ncol(sol.matrix))
    rownames(row.matrix) <- rownames(mtrx)
    
    for (i in seq(ncol(sol.matrix))) {
        row.matrix[sol.matrix[, i], i] <- TRUE
    }
    
    ess.PIs <- rownames(mtrx)[rowSums(row.matrix) == ncol(row.matrix)]
    
    if (length(ess.PIs) > 0) {
        for (i in seq(ncol(sol.matrix))) {
            solution[[i]] <- c(ess.PIs, sol.matrix[, i][!(sol.matrix[, i] %in% ess.PIs)])
        }
    }
    else {
        for (i in seq(ncol(sol.matrix))) {
            solution[[i]] <- sol.matrix[, i]
        }
    }
    output[[1]] <- lapply(solution, as.vector)
    output[[2]] <- as.vector(ess.PIs)
    return(output)
}

