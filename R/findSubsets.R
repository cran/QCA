`findSubsets` <-
function(noflevels3k, row.no, maximum) {
    if (length(row.no) > 1) {
        cat("\n")
        stop("Argument row.no should be a vector of length 1.\n\n", call. = FALSE)
    }
    if (missing(maximum)) maximum <- prod(noflevels3k)
    return(.Call("findSubsets", row.no, noflevels3k - 1, rev(c(1, cumprod(rev(noflevels3k))))[-1], maximum))
}

