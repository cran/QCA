`allExpressions` <-
function(noflevels, arrange = FALSE, depth = NULL, raw = FALSE, ...) {
    result <- createMatrix(noflevels + 1, arrange = arrange, depth = depth, ... = ...) - 1
    attr(result, "raw") <- raw
    class(result) <- c("matrix", "aE")
    return(result)
}
