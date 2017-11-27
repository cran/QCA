combinations <- function (n, k, aloe = 0, zero = FALSE, inC = FALSE) {
    if (!is.numeric(k)) {
        cat("\n")
        stop(simpleError("Argument k should be numeric.\n\n"))
    }
    if (length(k) != 1L) {
        cat("\n")
        stop(simpleError("Argument k should be a scalar of length 1.\n\n"))
    }
    if (k < 0) {
        cat("\n")
        stop(simpleError("Argument k should be positive.\n\n"))
    }
    if (n < k) {
        cat("\n")
        stop(simpleError("Argument n should be greater than or equal to k.\n\n"))
    }
    n <- as.integer(n)
    k <- as.integer(k)
    zero <- as.integer(zero)
    if (inC) {
        .Call("combinations", list(n = n, k = k, aloe = aloe, zero = zero), PACKAGE = "QCA")
    }
    else {
        aloe <- as.integer(aloe)
        e <- 0L
        ncols <- as.integer(choose(n, k))
        h <- k - ncols == 1
        out <- vector(mode = "list", length = ncols)
        comb <- seq.int(k) - zero 
        comb[k] <- comb[k] - 1L
        last <- n == k
        i <- 1
        while (comb[1] != n - k + 1 || last) {
            last <- FALSE
            if (e < n - h) {
                h <- 1L
                e <- comb[k] + zero 
                comb[k] <- comb[k] + 1L
                if (comb[k] < aloe) {
                    comb[k] <- aloe
                    e <- aloe - 1
                }
            }
            else {
                e <- comb[k - h] + zero 
                h <- h + 1L
                under <- logical(h)
                for (j in seq(h)) {
                    under[j] <- (e + j - zero < aloe) 
                    comb[k - h + j] <- e + j - zero  
                }
                if (all(under)) {
                    comb[k] <- aloe
                    e <- aloe - 1
                    h <- 1L
                }
            }
            out[[i]] <- comb
            i <- i + 1
        }
        return(do.call("cbind", out[!unlist(lapply(out, is.null))]))
    }
}
