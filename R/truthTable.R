`truthTable` <-
function(mydata, outcome = "", conditions = c(""), complete = FALSE,
         show.cases = FALSE, quiet = FALSE) {
    
    verify.tt(mydata, outcome, conditions, complete, show.cases)
    
    if (all(conditions == c(""))) {
        conditions <- names(mydata)[-which(names(mydata) == outcome)]
    }
    mydata <- mydata[, c(conditions, outcome)]
    
    nofconditions <- length(conditions)
    
    # the data MUST begin with 0 and MUST be incremented by 1 for each level
    noflevels <- apply(mydata[, conditions], 2, max) + 1
    
    mbase <- c(rev(cumprod(rev(noflevels))), 1)[-1]
    line.mydata <- as.vector(mbase %*% t(mydata[, conditions]) + 1)
    tt <- unique(mydata[, conditions])
    line.tt <- unique(line.mydata)
    tt <- tt[order(line.tt), ]               
    rownames(tt) <- line.tt <- sort(line.tt)
    
    if (complete) {
         # the "old" method, which at the time it was 10 times faster than the former code
         # tt <- as.matrix(expand.grid(rep(list(c(0,1)), nofconditions))[, nofconditions:1])
        
         # the new method, which is 5.5 times faster than expand.grid for binary data :))
        tt <- as.data.frame(createMatrix(noflevels))
        line.tt <- seq_len(dim(tt)[1])
    }
    
    tt <- cbind(tt, "?")
    colnames(tt) <- colnames(mydata)
    tt[, outcome] <- as.character(tt[, outcome])
    
    all.lines <- vector(mode="list")
    
    for (i in sort(unique(mydata[, outcome]))) {
        tt <- cbind(tt, "-")
        colnames(tt)[nofconditions + 2 + i] <- paste("freq", i, sep="")
        tt[, ncol(tt)] <- as.character(tt[, ncol(tt)])
        linesubset <- table(line.mydata[mydata[, outcome] == i])
        tt[match(names(linesubset), line.tt), nofconditions + i + 2] <- linesubset
        tt[match(names(linesubset), line.tt), outcome] <- i
        all.lines[[i + 1]] <- names(linesubset)
    }
    res <- table(unlist(all.lines))
    if (any(match(names(res[res > 1]), line.tt))) {
         tt[match(names(res[res > 1]), line.tt), outcome] <- "C"
    }
    
    if (show.cases) {
        tt <- cbind(tt, NA)
        tt[, ncol(tt)] <- sapply(line.tt, function(x) {
            paste(rownames(mydata)[which(line.mydata == x)], collapse=",")
        })
        casenames <- tt[, ncol(tt)]
        tt[, ncol(tt)] <- format(tt[, ncol(tt)], justify="left")
        colnames(tt)[ncol(tt)] <- format("cases", width=max(nchar(tt[, ncol(tt)])), justify="left")
        tt[, ncol(tt)][is.na(tt[, ncol(tt)])] <- ""
    }
    
    rownames(tt) <- paste(format(1:nrow(tt)), " ")
    
    x <- list(tt=tt, indexes=sort(unique(line.mydata)),
              noflevels=as.vector(noflevels))
    
    if (show.cases) x$casenames <- casenames
    
    if (!quiet) {
        print.tt(x)
    }
    
    invisible(structure(x, class="tt"))
}

