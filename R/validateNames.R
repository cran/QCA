`validateNames` <-
function(expression = "", snames = "") {
    ppm <- translate(expression = expression, snames = snames)
    return(colnames(ppm)[apply(ppm, 2, function(x) any(x >= 0))])
}
