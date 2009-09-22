`findSubsets` <-
function(noflevels, row.no, maximum) {
    if (missing(maximum)) maximum <- prod(noflevels)
    mbaserow <- getRow(noflevels, row.no)
    mbase <- rev(c(1, cumprod(rev(noflevels))))[-1]
    
    increment <- function(x, y, cond.levels) {
        a <- x
        for (i in 1:cond.levels) {
            a <- as.vector(outer(y, a, "+"))
            if (max(a) >= maximum) {
                x <- c(x, a[a <= maximum])
                return(x)
            }
            else {
                x <- c(x, a)
            }
        }
        return(x)
    }
    
    indices <- which(!mbaserow)
    for (k in rev(indices)) {
        row.no <- increment(row.no, mbase[k], noflevels[k])
    }
    return(row.no[-1])
}

