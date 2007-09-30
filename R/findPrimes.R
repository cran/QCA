`findPrimes` <-
function (noflevels, input.combs) {
    if (!is.matrix(input.combs)) {
        if (!is.vector(input.combs)) {
            cat("\n")
            stop("Input.combs must be either a base 3 matrix or a vector of line numbers.\n\n",
                 call. = FALSE)
            }
        else {
            if (any(input.combs > prod(noflevels + 1))) {
                cat("\n")
                stop(paste("Some line numbers do not belong in the 3^k space for",
                           length(noflevels), "causal conditions.\n\n"), call. = FALSE)
                }
            input.combs <- getRow(noflevels + 1, input.combs)
            }
        }
    mbase <- c(rev(cumprod(rev(noflevels + 1))), 1)[-1]
    allcombn <- t(createMatrix(noflevels)[-1, ])
    unique(as.vector(apply(input.combs, 1, function(x) (x*mbase) %*% allcombn + 1)))
    }

