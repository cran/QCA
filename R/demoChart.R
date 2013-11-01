`demoChart` <-
function(primes = c(""), configs = c(""), prod.split="") {
    if (prod.split != "") prod.split <- paste("\\", prod.split, sep="")
    mtrx <- t(sapply(primes, function(x) {
        y <- unlist(strsplit(x, prod.split))
        sapply(configs, function(idcol) {
            all(sapply(y, function(z) z %in% unlist(strsplit(idcol, prod.split))))
            })
        }))
        
    if (nrow(mtrx) == length(configs) & ncol(mtrx) == length(primes)) {
        mtrx <- t(mtrx)
    }
    colnames(mtrx) <- configs
    rownames(mtrx) <- primes
    return(mtrx)
}

