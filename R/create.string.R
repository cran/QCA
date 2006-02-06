"create.string" <-
function(mydata, use.letters=TRUE) {
    if (is.null(colnames(mydata))) colnames(mydata) <- LETTERS[1:ncol(mydata)]
    alreadyletters <- sum(nchar(colnames(mydata))) == ncol(mydata)
    changed <- FALSE
    if (use.letters & !alreadyletters & ncol(mydata) < 27) {
        varnames <- colnames(mydata)
        colnames(mydata) <- LETTERS[1:ncol(mydata)]
        changed <- TRUE
    }
    
    for (i in 1:ncol(mydata)) { 
        mydata[, i][mydata[, i] == 1] <- toupper(colnames(mydata)[i])
        mydata[, i][mydata[, i] == 0] <- tolower(colnames(mydata)[i])
    }
    
    input <- rep(NA, nrow(mydata))
    
    ifelse(alreadyletters | changed, collapsemethod <- "", collapsemethod <- "*")
    
    for (i in 1:nrow(mydata)) {
        input[i] <- paste(mydata[i, ], collapse = collapsemethod)
    }
    input
}

