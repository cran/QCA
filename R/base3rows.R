base3rows <- function(noflevels) {
    multiplier <- 0
    gap <- NULL
    for (i in 2:length(noflevels)) {
        multiplier <- 3*multiplier + 1
        gap <- c(gap, multiplier, gap)
        }
    
    linejump <- (3^length(noflevels) + 1)/2
    rownums <- c(linejump, sapply(gap, function(jump) {
        linejump <<- linejump + jump + 2
        }))
    return(sort(c(rownums, rownums + 1)))
    }

