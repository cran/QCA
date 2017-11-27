`possibleNumeric` <-
function(x) {
    return(!any(is.na(suppressWarnings(as.numeric(na.omit(as.character(x)))))) & !all(is.na(x)))
}
`asNumeric` <-
function(x) {
    return(suppressWarnings(as.numeric(as.character(x))))
}
`agteb` <- function(a, b) {
    all(a > b | abs(a - b) <= .Machine$double.eps^0.5)
}
`alteb` <- function(a, b) {
    all(a < b | abs(a - b) <= .Machine$double.eps^0.5)
}
