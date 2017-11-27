`getSolution` <-
function(expressions, mv, use.tilde, collapse, inputt, row.dom, initial, all.sol, indata, ...) {
    mtrx <- NULL
    sol.matrix <- NULL
    if (is.list(expressions)) {
        mtrx <- expressions[[2]]
        sol.matrix <- expressions[[3]]
        if (is.null(sol.matrix)) {
            cat("\n")
            stop(simpleError("There are no solutions, given these constraints.\n\n"))
        }
        else {
            sol.matrix[sol.matrix == 0] <- NA
        }
        expressions <- expressions[[1]]
    }
    if (FALSE) {
    if (!missing(indata)) {
        hastime <- logical(ncol(expressions))
        for (i in seq(ncol(expressions))) {
            if (any(indata[, i] %in% c("-", "dc", "?"))) {
                hastime[i] <- TRUE
            }
        }
        indata <- indata[, !hastime, drop = FALSE]
        expressions <- expressions[, !hastime, drop = FALSE]
        inputt <- inputt[, !hastime, drop = FALSE]
        relevant <- apply(expressions, 1, sum) > 0
        if (any(!relevant)) {
            sol.matrix <- NULL
            mtrx <- mtrx[relevant, , drop = FALSE]
            expressions <- expressions[relevant, , drop = FALSE]
        }
    }
    }
    PI <- writePrimeimp(expressions, mv = mv, use.tilde = use.tilde, collapse = collapse)
    rownames(expressions) <- PI
    if (is.null(mtrx)) {
        mtrx <- makeChart(expressions, inputt, mv = mv, use.tilde = use.tilde, collapse = collapse)
    }
    else {
        rownames(mtrx) <- PI
    }
    setColnames(mtrx, rownames(inputt)) 
    reduced <- list(expressions = expressions, mtrx = mtrx)
    if (nrow(mtrx) > 0) {
        if (row.dom & is.null(sol.matrix)) {
            reduced.rows <- rowDominance(mtrx)
            if (length(reduced.rows) > 0) {
                reduced$mtrx <- mtrx[reduced.rows, , drop=FALSE]
                reduced$expressions <- expressions[reduced.rows, , drop=FALSE]
            }
            sol.matrix <- NULL 
        }
        mtrx <- reduced$mtrx
        setColnames(mtrx, initial)
        if (is.null(sol.matrix)) {
            if (nrow(mtrx) > 150 & nrow(mtrx) * ncol(mtrx) > 1500) {
                message(sprintf("Starting to search all possible solutions in a PI chart with %d rows and %d columns.\nThis will take some time...", nrow(mtrx), ncol(mtrx)))
            }
            sol.matrix <- solveChart(mtrx, all.sol = all.sol, ...=...)
        }
        tokeep <- sort(unique(as.vector(unique(sol.matrix))))
        all.PIs <- rownames(mtrx)[tokeep]
        sol.matrix <- matrix(rownames(mtrx)[sol.matrix], nrow = nrow(sol.matrix))
        reduced$expressions <- reduced$expressions[tokeep, , drop=FALSE]
        solution.list <- writeSolution(sol.matrix, mtrx)
    }
    else {
        all.PIs <- NA
        solution.list <- NA
    }
    return(list(mtrx=mtrx, reduced=reduced, expressions=expressions, all.PIs=all.PIs, solution.list=solution.list))
}
