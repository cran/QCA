`negate` <-
function(expression, snames = "", noflevels, use.tilde = FALSE) {
    if (!missing(noflevels)) {
        noflevels <- splitstr(noflevels)
    }
    isol <- NULL
    if (methods::is(expression, "qca")) {
        snames <- expression$tt$options$conditions
        if (expression$options$use.letters) {
            snames <- LETTERS[seq(length(snames))]
        }
        noflevels <- expression$tt$noflevels
        if ("i.sol" %in% names(expression)) {
            elengths <- unlist(lapply(expression$i.sol, function(x) length(x$solution)))
            isol <- paste(rep(names(expression$i.sol), each = elengths), unlist(lapply(elengths, seq)), sep = "-")
            expression <- unlist(lapply(expression$i.sol, function(x) {
                lapply(x$solution, paste, collapse = " + ")
            }))
        }
        else {
            expression <- unlist(lapply(expression$solution, paste, collapse = " + "))
        }
    }
    if (is.character(expression)) {
        star <- any(grepl("[*]", expression))
        if (any(hastilde(expression))) {
            use.tilde <- TRUE
        }
        mv <- any(grepl("[{|}]", expression))
        negateit <- function(x, snames, noflevels) {
            x <- sop(x, snames = snames, noflevels = noflevels)
            trexp <- translate(x, snames = snames, noflevels = noflevels)
            snames <- colnames(trexp)
            if (missing(noflevels)) {
                noflevels <- rep(2, ncol(trexp))
            }
            snoflevels <- lapply(noflevels, function(x) seq(x) - 1)
            negated <- paste(apply(trexp, 1, function(x) {
                wx <- which(x != -1) 
                x <- x[wx]
                nms <- names(x)
                x <- sapply(seq_along(x), function(i) {
                    paste(setdiff(snoflevels[wx][[i]], splitstr(x[i])), collapse = ",")
                })
                if (mv) {
                    return(paste("(", paste(nms, "{", x, "}", sep = "", collapse = " + "), ")", sep = ""))
                }
                else {
                    nms[x == 0] <- tolower(nms[x == 0])
                    return(paste("(", paste(nms, collapse = " + ", sep = ""), ")", sep = ""))
                }
            }), collapse = "")
            negated <- sop(negated, snames = snames, noflevels = noflevels)
            if (use.tilde & !mv) {
                trneg <- translate(negated, snames = snames, noflevels = noflevels)
                negated <- paste(apply(trneg, 1, function(x) {
                    wx <- which(x >= 0)
                    x <- x[wx]
                    nms <- names(x)
                    nms[x == 0] <- paste("~", nms[x == 0], sep = "")
                    return(paste(nms, collapse = "*"))
                }), collapse = " + ")
            }
            if (!star) {
                negated <- gsub("[*]", "", negated)
            }
            return(negated)
        }
        result <- unlist(lapply(expression, negateit, snames = snames, noflevels = noflevels))
        attr(result, "expressions") <- expression
        if (!identical(snames, "")) {
            attr(result, "snames") <- snames
        }
        if (!is.null(isol)) {
            attr(result, "isol") <- isol
        }
        class(result) <- c("character", "deMorgan")
        return(result)
    }
    else {
        cat("\n")
        stop(simpleError("The expression should be a character vector.\n\n"))
    }
}
`deMorgan` <- function(...) {
    .Deprecated(msg = "Function deMorgan() is deprecated. Use function negate() instead.\n")
    negate(...)
}
