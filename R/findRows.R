`findRows` <-
function(expression = "", obj, remainders = TRUE, type = 1) {
    if (any(type == 0)) {
        type <- 0
    }
    if (any(is.element(type, 0:1)) & identical(expression, "")) {
        cat("\n")
        stop(simpleError("The expression is missing, for type 0 or 1.\n\n"))
    }
    if (missing(obj)) {
        cat("\n")
        stop(simpleError("The truth table object is missing.\n\n"))
    }
    if (methods::is(obj, "tt")) {
        noflevels <- obj$noflevels
        conditions <- obj$options$conditions
        if (any(is.element(type, c(0, 2, 3)))) {
            call <- obj$call
            call[[2]] <- obj$initial.data
            if (!obj$options$neg.out) {
                call[[3]] <- paste("~", call[[3]], sep = "")
            }
            nobj <- suppressWarnings(eval(call))
        }
    }
    else {
        if (is.matrix(obj) & is.numeric(obj)) {
            conditions <- colnames(obj)
            if (is.null(conditions)) {
                cat("\n")
                stop(simpleError("The input matrix does not have column names.\n\n"))
            }
        }
        else {
            cat("\n")
            stop(simpleError("Argument \"obj\" is not a truth table object or a numerical matrix.\n\n"))
        }
    }
    SBS <- NULL
    CSA <- NULL
    SSR <- NULL
    if (any(is.element(type, 0:1))) {
        trexp <- attr(translate(paste(expression, collapse = "+"), snames = conditions), "retlist")
        result <- matrix(ncol = length(trexp[[1]]), nrow = 0)
        if (is.matrix(obj)) {
            noflevels <- getNoflevels(obj)
        }
        for (i in seq(length(trexp))) {
            rowi <- trexp[[i]]
            detected <- !unlist(lapply(rowi, function(x) identical(x, -1)))
            rowi <- rowi[detected]
            rowi <- expand.grid(rowi)
            if (sum(!detected) > 0) {
                restm <- createMatrix(noflevels[!detected])
                colnames(restm) <- conditions[!detected]
                rowi <- apply(rowi, 1, function(x) rep(x, each = nrow(restm)))
                for (r in seq(ncol(rowi))) {
                    detm <- matrix(rowi[, r], nrow = nrow(restm))
                    colnames(detm) <- conditions[detected]
                    temp <- cbind(restm, detm)
                    result <- rbind(result, temp[, conditions])
                }
            }
            else {
                result <- rbind(result, rowi)
            }
        }
        if (methods::is(obj, "tt")) {
            mbase <- rev(c(1, cumprod(rev(noflevels))))[-1]
            diffwith <- NULL
            if (remainders) {
                diffwith <- obj$indexes
            }
            SBS <- setdiff(drop(result %*% mbase) + 1, diffwith)
        }
        else {
            SBS <- as.vector(which(apply(obj, 1, function(x) {
                x <- as.numeric(x)
                any(apply(result, 1, function(y) {
                    identical(x, as.numeric(y))
                }))
            })))
        }
    }
    if (any(is.element(type, c(0, 2)))) {
        pSA <- minimize(obj, include = "?")$SA
        pSAn <- minimize(nobj, include = "?")$SA
        SA1 <- pSA[[1]]
        if (length(pSA) > 1) {
            for (i in seq(2, length(pSA))) {
                SA1 <- rbind(SA1, pSA[[i]])
            }
        }
        SA2 <- pSAn[[1]]
        if (length(pSAn) > 1) {
            for (i in seq(2, length(pSAn))) {
                SA2 <- rbind(SA2, pSAn[[i]])
            }
        }
        CSA <- as.numeric(intersect(rownames(unique(SA1)), rownames(unique(SA2))))
    }
    if (any(is.element(type, c(0, 3)))) {
        SSR <- as.numeric(intersect(rownames(obj$tt)[obj$tt$OUT == 1], rownames(nobj$tt)[nobj$tt$OUT == 1]))
    }
    return(sort(unique(c(SBS, CSA, SSR))))
}
