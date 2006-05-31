"createMatrix" <- 
function(no.conditions) {
    create <- function(idx) {
        rep.int(c(rep.int(0,2^(idx-1)), rep.int(1,2^(idx-1))), 2^no.conditions/2^idx)
        }
    sapply(no.conditions:1, create)
    }

