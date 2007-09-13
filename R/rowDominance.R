`rowDominance` <-
function(mtrx) {
    rows <- rownames(mtrx)
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
                }
            }
        }
    return(reduced)
    }



