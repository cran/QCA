`setColnames` <- function(matrix, colnames) {
    invisible(.Call("setColnames", matrix, colnames))
}
`setRownames` <- function(matrix, colnames) {
    invisible(.Call("setRownames", matrix, colnames))
}
`setDimnames` <- function(matrix, list) {
    invisible(.Call("setDimnames", matrix, colnames))
}
