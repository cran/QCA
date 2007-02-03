`createMatrix` <- 
function(noflevels) {
    conds <- length(noflevels)
    if (all(noflevels == 2)) {
        create <- function(idx) {
            rep.int(c(rep.int(0, 2^(idx-1)), rep.int(1, 2^(idx-1))),
                    2^conds/2^idx)
            }
        sapply(conds:1, create)
        }
    else {
        mbase <- c(rev(cumprod(rev(noflevels))), 1)[-1]
        orep <- rev(mbase)
        sapply(seq_len(conds), function(x) {
            rep.int(rep.int(seq_len(noflevels[x]) - 1, rep.int(mbase[x], noflevels[x])), orep[x])
            })
        }
    }




        

