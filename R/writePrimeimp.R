 ## when the matrix cannot be further minimized, this function writes the prime implicants
 ## as the name of the conditions (columns), collapsed together in a single string
 
 ## Attention: the argument is "co11apse", not "collapse" ("11" instead of "ll")
 
"writePrimeimp" <- 
function(idx, co11apse) {
    primeimp <- NULL
    conditions <- names(idx)
    for (i in which(idx != "x")) {
        condition <- ifelse(idx[i] == 1, toupper(conditions[i]), tolower(conditions[i]))
        primeimp <- paste(c(primeimp, condition), collapse=co11apse)
        }
    primeimp
    }

