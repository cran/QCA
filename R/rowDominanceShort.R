`rowDominanceShort` <-
function(mtrx, primes) {
    sums <- rowSums(mtrx)
    tabsums <- sort(unique(sums))
    reduced <- logical(nrow(mtrx))
    if (length(tabsums) >= 2) {
        for (i in length(tabsums):2) {
            x <- which(sums == tabsums[i])
            y <- which(sums < tabsums[i])
            for (k in x) {
                to.be.reduced <- sapply(y, function(h) {
                    all(mtrx[k, mtrx[h,]])
                    })
                reduced[y[to.be.reduced]] <- TRUE
                to.be.reduced <- sapply(x, function(h) {
                    if (k != h) {
                        all(mtrx[k, mtrx[h,]]) & all(primes[k, primes[k,] != 0] == primes[h, primes[k,] != 0])
                        }
                    else {
                        return(FALSE)
                        }
                    })
                reduced[x[to.be.reduced]] <- TRUE
                }
            }
        }
    return(reduced)
    }





