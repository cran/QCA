"truth.table" <-
function(mydata, outcome="", inside=FALSE, complete=FALSE, show.lines=FALSE) {
    
    if (nchar(outcome) == 0) {
        cat("\n")
        stop("You haven't specified the outcome variable.\n\n", call. = FALSE)
    } else if (! outcome %in% colnames(mydata)) {
        cat("\n")
        stop("The outcome's name is not correct.\n\n", call. = FALSE)
    }
    
    conditions <- ncol(mydata) - 1
    
     # if the outcome variable is not the last one, it will be placed the last
    if (which(colnames(mydata) %in% outcome) != ncol(mydata)) {
        outcm.name <- colnames(mydata)[which(colnames(mydata) == outcome)]
        outcm   <- mydata[,  which(colnames(mydata) == outcome)]
        mydata  <- mydata[, -which(colnames(mydata) == outcome)]
        mydata <- cbind(mydata, outcm)
        colnames(mydata)[ncol(mydata)] <- outcm.name
    }
    
    tt <- as.matrix(expand.grid(rep(list(c(0,1)), conditions))[, conditions:1])
    tt <- cbind(tt, NA)
    colnames(tt) <- colnames(mydata)
    
     # the three lines below transform the binary lines from mydata
     # into corresponding row numbers from the truth table
    line.tt <- mydata[, 1]
    for (i in 2:conditions) line.tt <- 2*line.tt + mydata[, i]
    line.tt <- line.tt + 1
    
    for (i in 1:2) tt <- cbind(tt, 0)
    colnames(tt)[(ncol(tt) - 1):ncol(tt)] <- c("freq0", "freq1")
    
    column.tt <- sapply(1:length(line.tt), function(x) ncol(mydata) + mydata[x, outcome] + 1)
    
    for (i in 1:length(line.tt)) {
        tt[line.tt[i], column.tt[i]] <- tt[line.tt[i], column.tt[i]] + 1
    }
    tt[, outcome][as.numeric(tt[,"freq1"]) > 0 & as.numeric(tt[,"freq0"]) == 0] <- 1
    tt[, outcome][as.numeric(tt[,"freq0"]) > 0 & as.numeric(tt[,"freq1"]) == 0] <- 0
    tt[, outcome][as.numeric(tt[,"freq0"]) > 0 & as.numeric(tt[,"freq1"]) > 0] <- "C"
    tt[, outcome][(as.numeric(tt[, "freq0"]) + as.numeric(tt[, "freq1"])) == 0] <- "?"
    tt[, "freq0"][tt[, "freq0"] == 0] <- tt[, "freq1"][tt[, "freq1"] == 0] <- "-"
    
    if (show.lines) {
        tt <- cbind(tt, NA)
        colnames(tt)[ncol(tt)] <- "lines"
        tt.lines <- sort(unique(line.tt))
        for (i in tt.lines) {
            tt[i, "lines"] <- paste(rownames(mydata)[which(line.tt == i)], collapse=",")
            if (nchar(tt[i, "lines"]) < 5) {
                tt[i, "lines"] <- formatC(tt[i, "lines"], wid= -5)
            }
        }
        tt[, "lines"][is.na(tt[, "lines"])] <- ""
    }
    
    rownames(tt) <- paste(1:nrow(tt), " ")
    
    if (inside) {
        tt
    } else {
        if (!complete) {
            if (any(tt[, outcome] == "?")) tt <- tt[-which(tt[, outcome] == "?"), ]
        }
        var.names <- colnames(tt)[1:(conditions + 1)]
        colnames(tt)[1:conditions] <- LETTERS[1:conditions]
        colnames(tt)[conditions + 1] <- "OUT"
        cat("\n", sep="")
        print(pretty.table(tt))
        cat("\n", sep="")
        for (i in 1:conditions) cat("    ", paste(LETTERS[i], ": ", sep=""), var.names[i], "\n", sep="")
        cat("  ", "OUT: ", var.names[conditions + 1], "\n", sep="")
        cat("freq0: frequency of outcome equal to 0\nfreq1: frequency of outcome equal to 1\n", sep="")
        if (show.lines) cat("lines: case names\n", sep="")
        cat("\n", sep="")
    }
}

