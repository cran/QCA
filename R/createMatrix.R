`createMatrix` <- 
function(noflevels) {
    conds <- length(noflevels)
    pow <- unique(noflevels)
    if (length(pow) == 1) {
        create <- function(idx) {
            rep.int(c(sapply(seq_len(pow) - 1, function(x) rep.int(x, pow^(idx - 1)))),
                    pow^conds/pow^idx)
        }
        sapply(rev(seq_len(conds)), create)
    }
    else {
        mbase <- c(rev(cumprod(rev(noflevels))), 1)[-1]
        orep <- rev(mbase)
        sapply(seq_len(conds), function(x) {
            rep.int(rep.int(seq_len(noflevels[x]) - 1, rep.int(mbase[x], noflevels[x])), orep[x])
        })
    }
}

