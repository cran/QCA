`getNoflevels` <- 
function(data) {
    data <- as.data.frame(data)
    colnames <- paste("V", ncol(data), sep = ".")
    noflevels <- apply(data, 2, max) + 1
    noflevels[noflevels == 1] <- 2
    noflevels[apply(data, 2, function(x) any(x %% 1 > 0))] <- 2
    return(as.vector(noflevels))
}
