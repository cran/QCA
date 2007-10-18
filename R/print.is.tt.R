`is.tt` <-
function(x) {
    inherits(x, "tt")
}


`print.tt` <-
function(x, fooqmcc=FALSE, ...) {
    if (!fooqmcc & prod(x$noflevels) > 1024) {
        cat("\n")
        cat(paste("Warning: The truth table is too large (", prod(x$noflevels), " rows). ",
                   "Printing it on the screen is unmeaningful.\n         ",
                   "N.B.: You can still use its internal components (see ?str).", "\n\n", sep=""))
    }
    else {
        nofconditions <- length(x$noflevels)
        if (fooqmcc) x$tt <- x$tt[x$tt[, nofconditions + 1] != "?", ]
        names.mydata <- colnames(x$tt)[seq(nofconditions + 1)]
        colnames(x$tt)[seq(nofconditions)] <- LETTERS[seq(nofconditions)]
        colnames(x$tt)[nofconditions + 1] <- "OUT"
        cat("\n", sep="")
        for (i in seq(nofconditions)) {
            cat("    ", paste(LETTERS[i], ": ", sep=""), names.mydata[i], "\n", sep="")
        }
        cat("  ", "OUT: ", names.mydata[nofconditions + 1], "\n", sep="")
        cat("freq0: frequency of outcome equal to 0\nfreq1: frequency of outcome equal to 1\n")
        if ("cases" %in% colnames(x$tt)) cat("cases: case names\n", sep="")
        cat("\n", sep="")
        print(prettyTable(x$tt))
        cat("\n", sep="")
    }
}
