 ## function to sort a vector of strings according to their length

"sortVector" <- 
function(x) {
    strings <- NULL
    lengths <- sort(unique(nchar(x)))
    for (i in 1:length(lengths)) {
        strings <- c(strings, sort(x[which(nchar(x) == lengths[i])]))
        }
    strings
    }

