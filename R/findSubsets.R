`findSubsets` <-
function(noflevels3k, row.no, maximum) {
    if (missing(maximum)) maximum <- prod(noflevels3k)
    mbaserow <- getRow(noflevels3k, row.no)
    mbase <- rev(c(1, cumprod(rev(noflevels3k))))[-1]
    
    increment <- function(x, y, cond.levels) {
        a <- x
        for (i in seq(cond.levels)) {
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
    
     # bring all levels to the "normal" values, in order to increment times
     # exactly those values
    noflevels <- noflevels3k - 1
    
    for (k in rev(indices)) {
        row.no <- increment(row.no, mbase[k], noflevels[k])
    }
    
    if (length(row.no) > 1) {
        return(row.no[-1])
    }
    else {
        return(NULL)
    }
}

