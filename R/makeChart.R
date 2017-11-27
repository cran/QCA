`makeChart` <-
function(primes = "", configs = "", snames = "", mv = FALSE,
         use.tilde = FALSE, collapse = "*", ...) {
    prmat <- is.matrix(primes)
    comat <- is.matrix(configs)
    other.args <- list(...)
    if (prmat & comat) {
        if (!(is.numeric(primes) & is.numeric(configs))) {
            cat("\n")
            stop(simpleError("Matrices have to be numeric.\n\n"))
        }
        if (any(primes < 0) | any(configs < 0)) {
            cat("\n")
            stop(simpleError("Matrix values have to be non-negative.\n\n"))
        }
        if (!is.element("getSolution", unlist(lapply(lapply(sys.calls(), as.character), "[[", 1)))) {
            if (any(apply(primes, 1, sum) == 0) | any(apply(configs, 1, sum) == 0)) {
                cat("\n")
                stop(simpleError("Matrices have to be specified at implicants level.\n\n"))
            }
        }
        if (nrow(primes == 1) & sum(primes) == 0) {
            mtrx = matrix(nrow = 0, ncol = nrow(configs))
        }
        else {
            primes2 <- matrix(logical(length(primes)), dim(primes))
            primes2[primes > 0] <- TRUE
            mtrx <- sapply(seq(nrow(primes)), function(x) {
                apply(configs, 1, function(y) {
                    all(primes[x, primes2[x, ]] == y[primes2[x, ]])
                })
            })
            if (nrow(configs) == 1) {
                mtrx <- matrix(mtrx)
            }
            else {
                mtrx <- t(mtrx)
            }
            rownames(mtrx) <- writePrimeimp(primes, mv = mv, use.tilde = use.tilde, collapse = collapse)
        }
        colnames(mtrx) <- writePrimeimp(configs, mv = mv, use.tilde = use.tilde, collapse = collapse)
        class(mtrx) <- c("matrix", "pic")
        return(mtrx)
    }
    else if (!prmat & !comat) {
        if (!identical(snames, "")) {
            if (length(snames) == 1 & is.character(snames)) {
                snames <- splitstr(snames)
            }
        }
        noflevels <- rep(2, length(snames))
        if (is.element("noflevels", names(other.args))) {
            noflevels <- other.args$noflevels
        }
        tconfigs <- attr(translate(configs, snames, noflevels), "retlist")
        if (identical(snames, "")) {
            snames <- names(tconfigs[[1]])
        }
        tprimes <- attr(translate(primes, snames, noflevels), "retlist")
        mtrx <- matrix(FALSE, nrow=length(tprimes), ncol=length(tconfigs))
        for (i in seq(nrow(mtrx))) {
            for (j in seq(ncol(mtrx))) {
                tp <- unlist(tprimes[[i]])
                tc <- unlist(tconfigs[[j]])
                mtrx[i, j] <- all(tp[tp >= 0] == tc[tp >= 0])
            }
        }
        colnames(mtrx) <- names(tconfigs)
        rownames(mtrx) <- names(tprimes)
        class(mtrx) <- c("matrix", "pic")
        return(mtrx)
    }
    else {
        cat("\n")
        stop(simpleError("Both arguments have to be matrices.\n\n"))
    }
}
