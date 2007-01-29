`getRow` <- 
function(noflevels, row.no, zerobased=FALSE) {
    max.combs <- prod(noflevels)
    if (row.no > max.combs - zerobased) {
        cat("\n")
        stop("There cannot be more than ", max.combs, " rows.\n\n", call. = FALSE)
        }
    
    if (!zerobased) {row.no <- row.no - 1}
    
    mbase <- c(rev(cumprod(rev(noflevels))), 1)[-1]
    sapply(seq_len(noflevels), function(x) floor(row.no/mbase[x])%%noflevels[x])
    }

