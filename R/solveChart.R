`solveChart` <-
function(chart, row.dom = FALSE, all.sol = FALSE, ...) {
    if (!is.logical(chart)) {
        cat("\n")
        stop(simpleError("Use a T/F matrix. See makeChart's output.\n\n"))
    }
    other.args <- list(...)
    if ("min.dis" %in% names(other.args)) {
        if (is.logical(other.args$min.dis)) {
            all.sol <- !other.args$min.dis
        }
    }
    if (all.sol) {
        row.dom <- FALSE
    }
    row.numbers <- seq(nrow(chart))
    if (row.dom) {
        row.numbers <- rowDominance(chart)
        chart <- chart[row.numbers, ]
    }
    output <- list()
    if (all(dim(chart) > 1)) {
        k <- ceiling(sum(lp("min", rep(1, nrow(chart)), t(chart), ">=", 1)$solution))
        forceRAM <- 2
        if ("forceRAM" %in% names(other.args)) {
            if (length(other.args$forceRAM) == 1) {
                if (is.numeric(other.args$forceRAM) & other.args$forceRAM > 0) {
                    forceRAM <- other.args$forceRAM
                }
            }
        }
        if ((mem <- nrow(chart)*choose(nrow(chart), k)*8/1024^3) > forceRAM) {
            errmessage <- paste(paste("Too much memory needed (", round(mem, 1), " GB) to solve the PI chart using combinations of", sep=""),
                                   k, "out of", nrow(chart), "minimised PIs, with the PI chart having", ncol(chart), "columns.\n\n")
            cat("\n")
            stop(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep=""))
        }
        if (all.sol & k < nrow(chart)) {
            if (nrow(chart) > 29) { 
                cat("\n")
                stop(paste(strwrap("The PI chart is too large to identify all models.\n\n", exdent = 7), collapse = "\n", sep=""))
            }
            output <- .Call("allSol", k, chart*1, PACKAGE="QCA")
            output[output == 0] <- NA
        }
        else {
            combos <- combn(nrow(chart), k)
            output <- combos[, as.logical(.Call("solveChart", t(combos) - 1, chart*1, PACKAGE="QCA")[[1]]), drop=FALSE]
        }
    }
    else {
        output <- matrix(seq(nrow(chart)))
        if (ncol(chart) == 1) {
            output <- t(output)
        }
    } 
    return(matrix(row.numbers[output], nrow=nrow(output)))
}
