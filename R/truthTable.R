`truthTable` <-
function(mydata, outcome = "", neg.out = FALSE, conditions = c(""), n.cut = 1,
         incl.cut1 = 1, incl.cut0 = 1, complete = FALSE, show.cases = FALSE,
         sort.by = c(""), decreasing = TRUE, use.letters = FALSE, ...) {
    
    if (all(conditions == c(""))) {
        conditions <- names(mydata)[-which(names(mydata) == outcome)]
    }
    
    verify.tt(mydata, outcome, conditions, complete, show.cases, incl.cut1, incl.cut0)
    
    if (incl.cut0 > incl.cut1) {
        incl.cut0 <- incl.cut1
    }
    
    colnames(mydata) <- toupper(colnames(mydata))
    conditions <- toupper(conditions)
    outcome <- toupper(outcome)
    
    mydata <- initial.data <- mydata[, c(conditions, outcome)]
    
    if (neg.out) {
        mydata[, outcome] <- 1 - mydata[, outcome]
    }
    
    dc.code <- unique(unlist(lapply(mydata, function(x) {
        if (is.numeric(x)) {
            return(x[x < 0])
        }
        else {
            return(as.character(x[x %in% c("-", "dc")]))
        }
    })))
    
    if (length(dc.code) == 0) {
        dc.code <- -1
    }
    else if (length(dc.code) > 1) {
        cat("\n")
        stop("Multiple \"Don't care\" codes found.\n\n", call. = FALSE)
    }
    
    mydata <- as.data.frame(lapply(mydata, function(x) {
        x <- as.character(x)
        x[x == dc.code] <- -1
        return(as.numeric(x))
    }))
    mydata[mydata < 0] <- -1
    
    nofconditions <- length(conditions)
    fuzzy.cc <- apply(mydata[, conditions], 2, function(x) any(x %% 1 > 0))
    
    for (i in seq(length(conditions))) {
        if (!fuzzy.cc[i]) {
            copy.cc <- mydata[, i]
            if (any(copy.cc < 0)) {
                copy.cc[copy.cc < 0] <- max(copy.cc) + 1
                mydata[, i] <- copy.cc
            }
        }
    }
    
    # the data MUST begin with 0 and MUST be incremented by 1 for each level...!
    # perhaps trying something like
    # apply(mydata[, conditions], 2, function(x) length(unique(x))) + 1
    noflevels <- apply(mydata[, conditions], 2, max) + 1
    noflevels[fuzzy.cc] <- 2
    
    other.args <- list(...)
    if ("via.pof" %in% names(other.args)) {
        return(as.vector(noflevels))
    }
    
    excluded <- rep(FALSE, nrow(mydata))
    
     ## function needed to check equality of sub-unit numbers (a floating point issue...)
    check.equal <- function(x, y) {
        check.vector <- as.logical(unlist(lapply(x, all.equal, y)))
        check.vector[is.na(check.vector)] <- FALSE
        return(check.vector)
    }
    
    tt <- createMatrix(noflevels)
    minmat <- matrix(NA, nrow=nrow(mydata), ncol=nrow(tt))
    colnames(minmat) <- seq_len(nrow(tt))
    rownames(minmat) <- rownames(mydata)
    
    for (i in seq(nrow(mydata))) {
        row.i <- as.numeric(mydata[i, conditions])
        minmat[i, ] <- apply(tt, 1, function(x, values=row.i) {
            if (any(zerox <- x[fuzzy.cc] == 0)) {
                values[fuzzy.cc][zerox] <- 1 - values[fuzzy.cc][zerox]
            }
            copy.values <- values[!fuzzy.cc]
            if (length(copy.values) > 0) {
                values[!fuzzy.cc][x[!fuzzy.cc] != copy.values] <- 0
                values[!fuzzy.cc][x[!fuzzy.cc] == copy.values] <- 1
            }
            return(min(values))
        })
    }
    
    preserve  <- apply(minmat, 2, function(x)  sum(x > 0.5)) >= n.cut
    val.outcome <- mydata[, outcome]
    inclusion <- apply(minmat, 2, function(x) sum(pmin(x, val.outcome))/sum(x))
    pri <- apply(minmat, 2, function(x) {
        prisum <- sum(pmin(x, 1 - val.outcome, val.outcome))
        (sum(pmin(x, val.outcome)) - prisum)/(sum(x) - prisum)
    })
    inclusion[is.na(inclusion)] <- NA
    pri[is.na(pri)] <- NA
    
    minmat <- minmat[, preserve]
    outvalues <- as.numeric(inclusion[preserve] > incl.cut1 | check.equal(inclusion[preserve], incl.cut1))
    outvalues[inclusion[preserve] < incl.cut1 & (inclusion[preserve] > incl.cut0 | check.equal(inclusion[preserve], incl.cut0))] <- "C"
    
    for (i in seq_len(nrow(mydata))) {
        if (any(minmat[i, ] > 0.5)) {
            minmatcol <- which(minmat[i, ] > 0.5)
            mydata[i, conditions] <- tt[as.numeric(colnames(minmat)[minmatcol]), ]
            mydata[i, outcome] <- outvalues[minmatcol]
        }
        else {
            excluded[i] <- TRUE
        }
    }
    
    if (any(excluded)) {
        excluded.cases <- mydata[excluded, ]
        mydata <- mydata[!excluded, ]
    }
    
    mbase <- c(rev(cumprod(rev(noflevels))), 1)[-1]
    line.mydata <- drop(mbase %*% t(mydata[, conditions])) + 1
    
    if (complete) {
        line.tt <- seq_len(dim(tt)[1])
    }
    else {
        line.tt <- sort(unique(line.mydata))
        tt <- tt[line.tt, ]
    }
    
    tt <- as.data.frame(tt)
    rownames(tt) <- line.tt
    
    tt <- cbind(tt, "?") # code for missing outcomes
    colnames(tt) <- c(conditions, "OUT") #  Alrik Thiem's express request
    tt$OUT <- as.character(tt$OUT)
    
    tt[, "n"] <- 0
    freq.lines <- table(line.mydata)
    tt[names(freq.lines), "n"] <- freq.lines
    
    outcome.values <- sort(unique(mydata[, outcome]))
    
    for (i in seq(length(outcome.values))) {
        linesubset <- table(line.mydata[mydata[, outcome] == outcome.values[i]])
        tt[match(names(linesubset), line.tt), "OUT"] <- outcome.values[i]
    }
    
    tt <- cbind(tt, incl="-", PRI="-")
    tt$incl <- as.character(tt$incl)
    tt$PRI <- as.character(tt$PRI)
    inclusion <- inclusion[which(names(inclusion) %in% rownames(tt))]
    pri <- pri[which(names(pri) %in% rownames(tt))]
    tt[names(inclusion), "incl"] <- inclusion
    tt[names(inclusion), "PRI"] <- pri
    
    if (any(sort.by != "")) {
        sort.args <- c("incl", "n")
        if (!all(is.na(args.match <- match(sort.by, sort.args)))) {
            sort.args <- sort.args[args.match[!is.na(args.match)]]
            consorder <- order(tt[, sort.args[1]], decreasing=decreasing)
            if (length(sort.args) == 2) {
                consorder <- order(tt[, sort.args[1]], tt[, sort.args[2]], decreasing=decreasing)
            }
            tt <- tt[consorder, ]
            line.tt <- line.tt[consorder]
        }
    }
    
    cases <- sapply(line.tt, function(x) {
        paste(rownames(initial.data)[which(line.mydata == x)], collapse=",")
    })
    
    if (show.cases) {
        tt <- cbind(tt, cases)
    }
    
    for (i in seq(length(conditions))) {
        if (!fuzzy.cc[i]) {
            if (any(initial.data[, i] == dc.code)) {
                tt[, i][tt[, i] == max(tt[, i])] <- dc.code
                mydata[, i][mydata[, i] == max(mydata[, i])] <- dc.code
                noflevels[i] <- noflevels[i] - 1
            }
        }
    }
    
    x <- list(tt=tt, indexes=sort(unique(line.mydata)), noflevels=as.vector(noflevels), initial.data=initial.data, recoded.data=mydata, cases=cases, neg.out=neg.out)
    
    if (any(excluded)) {
       x$excluded <- excluded.cases
    }
    
    x$tt$incl[is.na(x$tt$incl)] <- "-"
    x$tt$PRI[is.na(x$tt$PRI)] <- "-"
    
    if (use.letters & sum(nchar(colnames(mydata)[-ncol(mydata)])) != (ncol(mydata) - 1)) { # also verify if not already letters
        colnames(x$tt)[seq(nofconditions)] <- LETTERS[seq(nofconditions)]
    }
    return(structure(x, class="tt"))
}

