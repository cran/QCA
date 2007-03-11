`truthTable` <-
function(mydata, outcome = "", conditions = c(""), inside = FALSE,
         complete = FALSE, show.cases = FALSE) {
    
    verify.truthTable(mydata, outcome, conditions, inside, complete, show.cases)
    
    if (all(conditions == c(""))) {
        conditions <- names(mydata)[-which(names(mydata)==outcome)]
        }
    mydata <- mydata[, c(conditions, outcome)]
    
    nofconditions <- length(conditions)
    
     # the data MUST begin with 0 and MUST be incremented by 1 for each level
    noflevels <- apply(mydata[, conditions], 2, max) + 1
    
    if (inside) complete <- FALSE
    
    if (complete) {
        if (prod(noflevels) > 1000 & !inside) {
            cat("\n")
            stop(paste("The truth table is too large (over 1000 rows).",
                       "Printing it on the screen is unmeaningful.", "\n\n", sep=""), call. = FALSE)
            }
         # the "old" way, which at the time it was 10 times faster than the former code
         # tt <- as.matrix(expand.grid(rep(list(c(0,1)), nofconditions))[, nofconditions:1])
        
         # the new way, which is 5.5 times faster than expand.grid for binary data :))
        tt <- createMatrix(noflevels)
        mbase <- c(rev(cumprod(rev(noflevels))), 1)[-1]
        line.tt <- seq_len(prod(noflevels))
        }
    else {
        tt <- unique(mydata[, conditions])
        mbase <- c(rev(cumprod(rev(noflevels))), 1)[-1]
        line.tt <- colSums(apply(tt, 1, function(x) x*mbase)) + 1
        tt <- tt[order(line.tt), ]               
        rownames(tt) <- line.tt <- line.tt[order(line.tt)]
        }
    
    tt <- cbind(tt, NA)
    colnames(tt) <- colnames(mydata)
    
    for (i in 1:2) {tt <- cbind(tt, 0)}
    colnames(tt)[(ncol(tt) - 1):ncol(tt)] <- c("freq0", "freq1")
    tt <- as.data.frame(tt)
    
    line.mydata <- as.character(colSums(apply(mydata[, conditions], 1, function(x) x*mbase)) + 1)
    column.tt <- ncol(mydata) + mydata[, outcome] + 1
    
    for (i in 1:nrow(mydata)) {
        tt[line.mydata[i], column.tt[i]] <- tt[line.mydata[i], column.tt[i]] + 1
        }
    
    tt[, outcome][as.numeric(tt[, "freq1"]) > 0 & as.numeric(tt[, "freq0"]) == 0] <- 1
    tt[, outcome][as.numeric(tt[, "freq0"]) > 0 & as.numeric(tt[, "freq1"]) == 0] <- 0
    tt[, outcome][as.numeric(tt[, "freq0"]) > 0 & as.numeric(tt[, "freq1"]) >  0] <- "C"
    tt[, outcome][(as.numeric(tt[, "freq0"]) + as.numeric(tt[, "freq1"]))   == 0] <- "?"
    tt[, "freq0"][tt[, "freq0"] == 0] <- tt[, "freq1"][tt[, "freq1"] == 0] <- "-"
    
    
    if (show.cases) {
        tt <- cbind(tt, NA)
        tt[, ncol(tt)] <- sapply(line.tt, function(x) {
            paste(rownames(mydata)[which(line.mydata == x)], collapse=",")
            })
        tt[, ncol(tt)] <- format(tt[, ncol(tt)], justify="left")
        colnames(tt)[ncol(tt)] <- format("cases", width=max(nchar(tt[, ncol(tt)])), justify="left")
        tt[, ncol(tt)][is.na(tt[, ncol(tt)])] <- ""
        }
    
    rownames(tt) <- paste(format(1:nrow(tt)), " ")
    
    if (inside) {
        list(tt=as.data.frame(tt), indexes=as.vector(line.tt), noflevels=as.vector(noflevels))
        }
    else {
        if (!complete) {
            if (any(tt[, outcome] == "?")) {tt <- tt[-which(tt[, outcome] == "?"), ]}
            }
        var.names <- colnames(tt)[1:ncol(mydata)]
        colnames(tt)[1:nofconditions] <- LETTERS[1:nofconditions]
        colnames(tt)[ncol(mydata)] <- "OUT"
        cat("\n", sep="")
        for (i in 1:nofconditions) {cat("    ", paste(LETTERS[i], ": ", sep=""), var.names[i], "\n", sep="")}
        cat("  ", "OUT: ", var.names[nofconditions + 1], "\n", sep="")
        cat("freq0: frequency of outcome equal to 0\nfreq1: frequency of outcome equal to 1\n", sep="")
        if (show.cases) cat("cases: case names\n", sep="")
        cat("\n", sep="")
        print(prettyTable(tt))
        cat("\n", sep="")
        }
    }

