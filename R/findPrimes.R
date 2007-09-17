`findPrimes` <-
function (explain, noflevels, mbase) {
    if (!is.matrix(explain)) {
        if (!is.vector(explain)) {
            cat("\n")
            stop("Explain must be either a base 3 matrix or a vector of line numbers.\n\n",
                 call. = FALSE)
            }
        else {
            if (any(explain > prod(noflevels + 1))) {
                cat("\n")
                stop(paste("Some line numbers do not belong in the 3^k space for",
                           length(noflevels), "causal conditions.\n\n"), call. = FALSE)
                }
            explain <- getRow(noflevels + 1, explain)
            }
        }
    
    allcombn <- t(createMatrix(noflevels)[-1, ])
    unique(as.vector(apply(explain, 1, function(x) (x*mbase) %*% allcombn + 1)))
    }

