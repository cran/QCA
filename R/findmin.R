`findmin` <-
function(chart) {
    if (!methods::is(chart, "pic")) {
        if (!is.matrix(chart) | (!is.logical(chart) & length(setdiff(chart, 0:1)) > 0)) {
            cat("\n")
            stop(simpleError("The input should be a logical matrix. See function makeChart()\n\n"))
        }
    }
    return(.Call("findmin", t(matrix(as.logical(chart), nrow = nrow(chart))), PACKAGE = "QCA"))
}
