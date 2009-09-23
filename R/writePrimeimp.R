 ## when the matrix cannot be further minimized, this function writes the prime implicants
 ## as the name of the conditions (columns), collapsed together in a single string

`writePrimeimp` <- 
function(idx, collapse="*", uplow=FALSE) {
    conditions <- names(idx)
    condition <- idx
    if (uplow) {
        condition[idx == 1] <- tolower(conditions[idx == 1])
        condition[idx == 2] <- toupper(conditions[idx == 2])
    } else {
        for (i in sort(unique(as.vector(idx)))) {
            condition[idx == i] <- paste(conditions[idx == i], "{", i - 1, "}", sep="")
        }
    }
    primeimp <- paste(condition[idx != 0], collapse=collapse)
    return(primeimp)
}

