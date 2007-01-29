`demoChart` <-
function(rows, columns) {
    mtrx <- t(sapply(rows, function(x) {
        y <- unlist(strsplit(x, ""))
        sapply(columns, function(idcol) {
            all(sapply(y, function(z) regexpr(z, idcol) > 0))
            })
        }))
    colnames(mtrx) <- columns
    rownames(mtrx) <- rows
    return(mtrx)
    }

