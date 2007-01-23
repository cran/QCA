`demoChart` <-
function(rows, columns) {
    mtrx <- t(sapply(rows, function(x) {regexpr(x, columns) > 0}))
    colnames(mtrx) <- columns
    rownames(mtrx) <- rows
    return(mtrx)
    }

