`demoChart` <-
function(rows, columns, use.letters=TRUE) {
    splitmethod <- ifelse(use.letters, "", "\\*")
    mtrx <- t(sapply(rows, function(x) {
        y <- unlist(strsplit(x, splitmethod))
        sapply(columns, function(idcol) {
            all(sapply(y, function(z) regexpr(z, idcol) > 0))
            })
        }))
    colnames(mtrx) <- columns
    rownames(mtrx) <- rows
    return(mtrx)
    }

