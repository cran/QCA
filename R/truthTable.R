`truthTable` <-
function(mydata, outcome = "", conditions = c(""), inside = FALSE,
         complete = FALSE, show.cases = FALSE) {
    
    verify.truthTable(mydata, outcome, conditions, inside, complete, show.cases)
    
    no.of.conditions <- ncol(mydata) - 1
    
     # if the outcome variable is not the last one, it will be placed the last
    if (which(colnames(mydata) %in% outcome) != ncol(mydata)) {
        outcm.name <- colnames(mydata)[which(colnames(mydata) == outcome)]
        outcm   <- mydata[,  which(colnames(mydata) == outcome)]
        mydata  <- mydata[, -which(colnames(mydata) == outcome)]
        mydata <- cbind(mydata, outcm)
        colnames(mydata)[ncol(mydata)] <- outcm.name
        }
    
    noflevels <- apply(mydata[, 1:no.of.conditions], 2, max) + 1
    
     # the "old" way, which at the time it was 10 times faster than the former code
     # tt <- as.matrix(expand.grid(rep(list(c(0,1)), no.of.conditions))[, no.of.conditions:1])
    
     # the new way, which is 5.5 times faster than expand.grid :))
    tt <- createMatrix(noflevels)
    
    tt <- cbind(tt, NA)
    colnames(tt) <- colnames(mydata)
    
    
     # the code below transform the lines from mydata
     # into corresponding row numbers from the truth table
    #mbase <- 2^((no.of.conditions - 1):0)
    mbase <- c(rev(cumprod(rev(noflevels))), 1)[-1]
    line.tt <- colSums(apply(mydata[, 1:no.of.conditions], 1, function(x) x*mbase)) + 1
    
    
    for (i in 1:2) {tt <- cbind(tt, 0)}
    colnames(tt)[(ncol(tt) - 1):ncol(tt)] <- c("freq0", "freq1")
    
    column.tt <- sapply(seq(length(line.tt)), function(x) ncol(mydata) + mydata[x, outcome] + 1)
    
    tt <- as.data.frame(tt)
    
    for (i in seq(length(line.tt))) {
        tt[line.tt[i], column.tt[i]] <- tt[line.tt[i], column.tt[i]] + 1
        }
    tt[, outcome][as.numeric(tt[, "freq1"]) > 0 & as.numeric(tt[, "freq0"]) == 0] <- 1
    tt[, outcome][as.numeric(tt[, "freq0"]) > 0 & as.numeric(tt[, "freq1"]) == 0] <- 0
    tt[, outcome][as.numeric(tt[, "freq0"]) > 0 & as.numeric(tt[, "freq1"]) >  0] <- "C"
    tt[, outcome][(as.numeric(tt[, "freq0"]) + as.numeric(tt[, "freq1"]))   == 0] <- "?"
    tt[, "freq0"][tt[, "freq0"] == 0] <- tt[, "freq1"][tt[, "freq1"] == 0] <- "-"
    
    if (show.cases) {
        tt.lines <- sort(unique(line.tt))
        tt <- cbind(tt, NA)
        tt[tt.lines, ncol(tt)] <- sapply(tt.lines, function(x) paste(rownames(mydata)[which(line.tt == x)], collapse=","))
        
        tt[tt.lines, ncol(tt)] <- format(tt[tt.lines, ncol(tt)], justify="left")
        colnames(tt)[ncol(tt)] <- format("cases", width=max(nchar(tt[, ncol(tt)])), justify="left")
        tt[, ncol(tt)][is.na(tt[, ncol(tt)])] <- ""
        }
    
    rownames(tt) <- paste(format(1:nrow(tt)), " ")
    
    if (inside) {
        tt
        }
    else {
        if (!complete) {
            if (any(tt[, outcome] == "?")) {tt <- tt[-which(tt[, outcome] == "?"), ]}
            }
        var.names <- colnames(tt)[1:(no.of.conditions + 1)]
        colnames(tt)[1:no.of.conditions] <- LETTERS[1:no.of.conditions]
        colnames(tt)[no.of.conditions + 1] <- "OUT"
        cat("\n", sep="")
        for (i in 1:no.of.conditions) {cat("    ", paste(LETTERS[i], ": ", sep=""), var.names[i], "\n", sep="")}
        cat("  ", "OUT: ", var.names[no.of.conditions + 1], "\n", sep="")
        cat("freq0: frequency of outcome equal to 0\nfreq1: frequency of outcome equal to 1\n", sep="")
        if (show.cases) cat("cases: case names\n", sep="")
        cat("\n", sep="")
        print(prettyTable(tt))
        cat("\n", sep="")
        }
    }

