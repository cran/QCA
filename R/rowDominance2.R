`rowDominance2` <-
function(mtrx, primes) {
    sums <- rowSums(mtrx)
    mtrx <- mtrx[order(sums, decreasing=TRUE), , drop=FALSE]
    primes <- primes[order(sums, decreasing=TRUE), , drop=FALSE]
    sums <- sort(sums, decreasing=T)
    line.no <- 1
    while(line.no < nrow(mtrx)) {
        less <- sums < sums[line.no]
        if (any(less)) {
            aa <- apply(mtrx[less, , drop=FALSE], 1, function(x) {all(mtrx[line.no, x])})
            mtrx <- rbind(mtrx[!less, , drop=FALSE], mtrx[less, , drop=FALSE][!aa, , drop=FALSE])
            primes <- rbind(primes[!less, , drop=FALSE], primes[less, , drop=FALSE][!aa, , drop=FALSE])
            sums <- rowSums(mtrx)
            line.no <- line.no + 1
            }
        else {
            break
            }
        }
    return(list(mtrx=mtrx, primes=primes))
    }

