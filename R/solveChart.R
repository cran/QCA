`solveChart` <-
function(chart, row.dom = FALSE, all.sol = FALSE, depth = NULL, ...) {
    if (!is.logical(chart) && length(setdiff(chart, 0:1)) > 0) {
        cat("\n")
        stop(simpleError("Use a T/F matrix. See makeChart()'s output.\n\n"))
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
    if (findmin(chart) == 0) {
        cat("\n")
        stop(simpleError("The PI chart cannot be solved.\n\n"))
    }
    if (all(dim(chart) > 1)) {
        if (is.null(depth)) depth <- 0L
        output <- .Call("solveChart", t(matrix(as.logical(chart), nrow = nrow(chart))), all.sol, as.integer(depth), PACKAGE = "QCA")
        output[output == 0] <- NA
    }
    else {
        output <- matrix(seq(nrow(chart)))
        if (ncol(chart) == 1) {
            output <- t(output)
        }
    }
    return(matrix(row.numbers[output], nrow=nrow(output)))
}
