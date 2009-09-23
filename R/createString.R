`createString` <-
function(mydata, use.letters=TRUE, uplow=TRUE) {
    mydata <- changemydata <- as.matrix(mydata)
    conditions <- colnames(mydata)
    if (is.null(conditions)) {conditions <- LETTERS[1:ncol(mydata)]}
    alreadyletters <- sum(nchar(conditions)) == ncol(mydata)
    changed <- FALSE
    if (use.letters & !alreadyletters & ncol(mydata) < 27) {
        conditions <- LETTERS[1:ncol(mydata)]
        changed <- TRUE
        }
    
    if (uplow) {
        changemydata[mydata == 0] <- tolower(rep(conditions, each=nrow(mydata))[mydata == 0])
        changemydata[mydata == 1] <- toupper(rep(conditions, each=nrow(mydata))[mydata == 1])
    } else {
        for (i in sort(unique(as.vector(mydata)))) {
            changemydata[mydata == i] <- paste(rep(conditions, each=nrow(mydata))[mydata == i], "{", i, "}", sep="")
        }
    }
    
    input <- rep(NA, nrow(mydata))
    
    if(!all(mydata %in% c(0, 1))) uplow <- FALSE
    collapse <- ifelse((alreadyletters | changed) & uplow, "", "*")
    
    for (i in 1:nrow(mydata)) {
        input[i] <- paste(changemydata[i, ], collapse = collapse)
    }
    return(input)
}

