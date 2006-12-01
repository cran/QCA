"createMatrix" <- 
function(cond) {
    create <- function(idx) {
        rep.int(c(rep.int(0,2^(idx-1)), rep.int(1,2^(idx-1))),
                2^cond/2^idx)
        }
    sapply(cond:1, create)
    }

