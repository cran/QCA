"createChart" <-
function(rows, columns) {
    mtrx <- matrix(logical(length(rows)*length(columns)), nrow = length(rows))
    colnames(mtrx) <- columns
    rownames(mtrx) <- rows
    
    for (i in 1:nrow(mtrx)) {
        for (j in 1:ncol(mtrx)) {
            rows.s <- unlist(strsplit(rows[i], ""))
            input <- unlist(strsplit(columns[j], ""))
            if (sum(rows.s %in% input) == length(rows.s)) {
                mtrx[i,j] <- TRUE
            }
        }
    }
    mtrx
}

