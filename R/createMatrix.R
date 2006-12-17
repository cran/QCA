`createMatrix` <- 
function(levels.no) {
    if (all(levels.no == 2)) {
        cond <- length(levels.no)
        create <- function(idx) {
        rep.int(c(rep.int(0, 2^(idx-1)), rep.int(1, 2^(idx-1))),
                2^cond/2^idx)
        }
        sapply(cond:1, create)
        }
    else {
        nrcol <- length(levels.no)
        rep.fac <- prod(levels.no[-1])
        bb <- matrix(NA, nrow=prod(levels.no), ncol=nrcol)
         # lines stolen and adapted from expand.grid
        orep <- 1
        for (i in 1:nrcol) {
            x <- seq_len(levels.no[i]) - 1
            bb[,i] <- rep.int(rep.int(x, rep.int(rep.fac, levels.no[i])), orep)
            orep <- orep * levels.no[i]
            if (i < nrcol) {rep.fac <- rep.fac/levels.no[i + 1]}
            }
        return(bb)
        }
    }

