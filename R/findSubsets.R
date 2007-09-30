`findSubsets` <-
function(noflevels, row.no, maximum) {
    if (missing(maximum)) maximum <- prod(noflevels + 1)
    base3row <- getRow(noflevels + 1, row.no)
    mbase <- c(rev(cumprod(rev(noflevels + 1))), 1)[-1]
     # see function Reduce() in R-2.6.0, maybe it could directly perform below
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
    
    indices <- which(base3row == 0)
    for (k in rev(indices)) {
        row.no <- increment(row.no, mbase[k], noflevels[k])
        }
    
    return(row.no[-1])
    }






