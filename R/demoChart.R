`demoChart` <-
function(rows, columns, splitmethod="") {
    if (splitmethod != "") splitmethod <- paste("\\", splitmethod, sep="")
    mtrx <- t(sapply(rows, function(x) {
        y <- unlist(strsplit(x, splitmethod))
        sapply(columns, function(idcol) {
            all(sapply(y, function(z) z %in% unlist(strsplit(idcol, splitmethod))))
            })
        }))
    colnames(mtrx) <- columns
    rownames(mtrx) <- rows
    return(mtrx)
    }

