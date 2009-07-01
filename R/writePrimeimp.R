 ## when the matrix cannot be further minimized, this function writes the prime implicants
 ## as the name of the conditions (columns), collapsed together in a single string
 
 ## Attention: the argument is "collapse", not "collapse" ("11" instead of "ll")
 
`writePrimeimp` <- 
function(idx, collapse) {
    conditions <- names(idx)
    condition <- idx
    condition[idx == 1] <- tolower(conditions[idx == 1])
    condition[idx == 2] <- toupper(conditions[idx == 2])
    primeimp <- paste(condition[idx != 0], collapse=collapse)
    return(primeimp)
    }

