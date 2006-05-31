"allExpr" <-
function(column.names, inside=FALSE) {
    ncolumns <- length(column.names)
    return.matrix <- matrix(NA, nrow = (3^ncolumns - 1), ncol = ncolumns)
    colnames(return.matrix) <- column.names
    rownames(return.matrix) <- 1:nrow(return.matrix)
    start.row <- 1
    all.combn <- sapply(1:ncolumns, function(idx) as.matrix(combn(ncolumns, idx)), simplify = FALSE)
    for (j in 1:length(all.combn)) {
        idk <- all.combn[[j]]
        tt <- as.matrix(expand.grid(rep(list(c(0, 1)), nrow(idk)))[, nrow(idk):1])
        for (k in 1:ncol(idk)) {
            end.row <- start.row + nrow(tt) - 1
            return.matrix[start.row:end.row, idk[, k]] <- tt
            start.row <- end.row + 1
        }
    }
    if (inside) {
        return.matrix
    } else {
        return.matrix[is.na(return.matrix)] <- ""
        print(prettyTable(return.matrix))
    }
}

