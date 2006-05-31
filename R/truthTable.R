"truthTable" <-
function(mydata, outcome = "", conditions = c(""), inside = FALSE,
         complete = FALSE, show.cases = FALSE) {
    
    if (nchar(outcome) == 0) {
        cat("\n")
        stop("You haven't specified the outcome variable.\n\n", call. = FALSE)
        }
    else if (! outcome %in% colnames(mydata)) {
        cat("\n")
        stop("The outcome's name is not correct.\n\n", call. = FALSE)
        }
    
     # subset the data with the conditions specified
     if (length(conditions) > 1) {
        if (outcome %in% conditions) {
            cat("\n")
            stop('Variable "', outcome, '" cannot be both outcome _and_ condition!\n\n', call. = FALSE)
             }
        if (!all(conditions %in% names(mydata))) {
            cat("\n")
            stop("The conditions' names are not correct.\n\n", call. = FALSE)
            }
        else {
            mydata <- mydata[, c(conditions, outcome)]
            }
        }
     else if (nchar(conditions) > 0) {
                cat("\n")
                stop("Cannot create a truth table with only one condition.\n\n", call. = FALSE)
        }
    
    no.conditions <- ncol(mydata) - 1
    
     # if the outcome variable is not the last one, it will be placed the last
    if (which(colnames(mydata) %in% outcome) != ncol(mydata)) {
        outcm.name <- colnames(mydata)[which(colnames(mydata) == outcome)]
        outcm   <- mydata[,  which(colnames(mydata) == outcome)]
        mydata  <- mydata[, -which(colnames(mydata) == outcome)]
        mydata <- cbind(mydata, outcm)
        colnames(mydata)[ncol(mydata)] <- outcm.name
        }
    
    
    
    # the "old" way, which at the time it was 10 times faster than the initial code
    # tt <- as.matrix(expand.grid(rep(list(c(0,1)), no.conditions))[, no.conditions:1])
    
    # the new way, which is 5.5 times faster than expand.grid :))
    tt <- createMatrix(no.conditions)
    
    
    tt <- cbind(tt, NA)
    colnames(tt) <- colnames(mydata)
    
        
    
    
    
     # the three lines below transform the binary lines from mydata
     # into corresponding row numbers from the truth table
    line.tt <- mydata[, 1]
    for (i in 2:no.conditions) {line.tt <- 2*line.tt + mydata[, i]}
    line.tt <- line.tt + 1
    
    for (i in 1:2) {tt <- cbind(tt, 0)}
    colnames(tt)[(ncol(tt) - 1):ncol(tt)] <- c("freq0", "freq1")
    
    column.tt <- sapply(1:length(line.tt), function(x) ncol(mydata) + mydata[x, outcome] + 1)
    
    for (i in 1:length(line.tt)) {
        tt[line.tt[i], column.tt[i]] <- tt[line.tt[i], column.tt[i]] + 1
        }
    tt[, outcome][as.numeric(tt[, "freq1"]) > 0 & as.numeric(tt[, "freq0"]) == 0] <- 1
    tt[, outcome][as.numeric(tt[, "freq0"]) > 0 & as.numeric(tt[, "freq1"]) == 0] <- 0
    tt[, outcome][as.numeric(tt[, "freq0"]) > 0 & as.numeric(tt[, "freq1"]) >  0] <- "C"
    tt[, outcome][(as.numeric(tt[, "freq0"]) + as.numeric(tt[, "freq1"]))   == 0] <- "?"
    tt[, "freq0"][tt[, "freq0"] == 0] <- tt[, "freq1"][tt[, "freq1"] == 0] <- "-"
    
    if (show.cases) {
        tt <- cbind(tt, NA)
        colnames(tt)[ncol(tt)] <- "cases"
        tt.lines <- sort(unique(line.tt))
        for (i in tt.lines) {
            tt[i, "cases"] <- paste(rownames(mydata)[which(line.tt == i)], collapse=",")
            if (nchar(tt[i, "cases"]) < 5) {
                tt[i, "cases"] <- formatC(tt[i, "cases"], wid= -5)
                }
            }
        tt[, "cases"][is.na(tt[, "cases"])] <- ""
        }
    
    rownames(tt) <- paste(1:nrow(tt), " ")
    
    if (inside) {
        tt
        }
    else {
        if (!complete) {
            if (any(tt[, outcome] == "?")) {tt <- tt[-which(tt[, outcome] == "?"), ]}
            }
        var.names <- colnames(tt)[1:(no.conditions + 1)]
        colnames(tt)[1:no.conditions] <- LETTERS[1:no.conditions]
        colnames(tt)[no.conditions + 1] <- "OUT"
        cat("\n", sep="")
        for (i in 1:no.conditions) {cat("    ", paste(LETTERS[i], ": ", sep=""), var.names[i], "\n", sep="")}
        cat("  ", "OUT: ", var.names[no.conditions + 1], "\n", sep="")
        cat("freq0: frequency of outcome equal to 0\nfreq1: frequency of outcome equal to 1\n", sep="")
        if (show.cases) cat("cases: case names\n", sep="")
        cat("\n", sep="")
        print(prettyTable(tt))
        cat("\n", sep="")
        }
    }

