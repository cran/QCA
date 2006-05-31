 # function to create a list which will contain the solution(s) and the essential prime implicants

"writeSolution" <- 
function(sol.matrix, mtrx) {
    solution <- output <- NULL
    if (is.matrix(sol.matrix)) {
        row.matrix <- matrix(FALSE, nrow=nrow(mtrx), ncol=ncol(sol.matrix))
        for (i in 1:ncol(sol.matrix)) {
            row.matrix[sol.matrix[ , i], i] <- TRUE
            }
        ess.prime.imp <- logical(nrow(mtrx))
        ess.prime.imp[rowSums(row.matrix) == ncol(row.matrix)] <- TRUE
        if (sum(ess.prime.imp) > 0) {
            for (i in 1:ncol(sol.matrix)) {
                solution.ess <- sort(rownames(mtrx)[ess.prime.imp])
                not.essential <- sol.matrix[ , i][!sol.matrix[ , i] %in% which(ess.prime.imp)]
                solution[[i]] <- c(solution.ess, sort(rownames(mtrx)[not.essential]))
                }
            }
        else {
            for (i in 1:nrow(sol.matrix)) {
                solution[[i]] <- rownames(mtrx)[sol.matrix[ , i]]
                }
            }
        output[[1]] <- solution
        output[[2]] <- ess.prime.imp
        }
    else {
        solution <- rownames(mtrx)[sol.matrix]
        output[[1]] <- solution
        output[[2]] <- FALSE
        }
    output
    }

