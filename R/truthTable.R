`truthTable` <-
function(mydata, outcome = "", neg.out = FALSE, conditions = c(""), n.cut = 1,
         incl.cut1 = 1, incl.cut0 = 1, complete = FALSE, show.cases = FALSE,
         sort.by = c(""), decreasing = TRUE, use.letters = FALSE,...) {
    
    memcare <- FALSE # to be updated with a future version
    
    if (all(conditions == c(""))) {
        conditions <- names(mydata)[-which(names(mydata) == outcome)]
    }
    
    if (memcare) {
        complete <- FALSE
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
    rownames(mydata) <- rownames(initial.data)
    
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
    
    if (memcare) {
        mbase <- c(rev(cumprod(rev(noflevels))), 1)[-1]
        inclpri <- .Call("truthTableMem", as.matrix(mydata[, conditions]), noflevels, mbase, fuzzy.cc, mydata[, outcome], package="QCA")
    }
    else {
        tt <- createMatrix(noflevels)
        inclpri <- .Call("truthTable", as.matrix(mydata[, conditions]), tt, fuzzy.cc, mydata[, outcome], package="QCA")
    }
    
    colnames(inclpri[[1]]) <- seq_len(ncol(inclpri[[1]]))
    line.mydata <- inclpri[[2]]
    
    preserve <- inclpri[[1]][3, ] >= n.cut
    inclpri  <- inclpri[[1]][1:2, ]
    # inclpri  <- inclpri[[1]][1:2, ncut.fre >= n.cut]
    
    inclpri[is.na(inclpri)] <- NA
    
    outvalues <- as.numeric(inclpri[1, preserve] >= (incl.cut1 - .Machine$double.eps ^ 0.5))
    outvalues[inclpri[1, preserve] < incl.cut1 & inclpri[1, preserve] >= (incl.cut0 - .Machine$double.eps ^ 0.5)] <- "C"
    names(outvalues) <- colnames(inclpri)[preserve]
    
    freq.lines <- table(line.mydata)
    
    line.mydata[!line.mydata %in% colnames(inclpri)[preserve]] <- 0
    
    excluded <- line.mydata == 0
    line.mydata <- line.mydata[!excluded]
    
    
    if (memcare) {
        mydata[!excluded, conditions] <- getRow(noflevels, line.mydata)
    }
    else {
        mydata[!excluded, conditions] <- tt[line.mydata, ]
    }
    
    
    
    if (any(excluded)) {
        excluded.cases <- mydata[excluded, ]
        mydata <- mydata[!excluded, ]
    }
    
    mydata[, outcome] <- outvalues[match(line.mydata, names(outvalues))]
    
    
    if (complete) { # implicitly memcare is FALSE
        line.tt <- seq_len(dim(tt)[1])
    }
    else { # this doesn't implicitly imply memcare = TRUE, although it could be
        line.tt <- sort(unique(line.mydata))
        tt <- getRow(noflevels, line.tt)
    }
    
    tt <- as.data.frame(tt)
    rownames(tt) <- line.tt
    
    tt <- cbind(tt, "?") # code for missing outcomes
    colnames(tt) <- c(conditions, "OUT") #  Alrik Thiem's express request
    tt$OUT <- as.character(tt$OUT)
    
    tt[, "n"] <- 0
    lines.in.tt <- which(names(freq.lines) %in% rownames(tt))
    tt[names(freq.lines)[lines.in.tt], "n"] <- freq.lines[lines.in.tt]
    
    
    outcome.values <- sort(unique(mydata[, outcome]))
    
    for (i in seq(length(outcome.values))) {
        linesubset <- table(line.mydata[mydata[, outcome] == outcome.values[i]])
        tt[match(names(linesubset), line.tt), "OUT"] <- outcome.values[i]
    }
    
    
    tt <- cbind(tt, incl="-", PRI="-")
    tt$incl <- as.character(tt$incl)
    tt$PRI <- as.character(tt$PRI)
    inclpri <- inclpri[, which(colnames(inclpri) %in% rownames(tt))]
    tt[colnames(inclpri), "incl"] <- inclpri[1, ]
    tt[colnames(inclpri), "PRI"] <- inclpri[2, ]
    
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
    
    #return(list(line.tt, initial.data, line.mydata))
    
    cases <- sapply(line.tt, function(x) {
        paste(rownames(mydata)[which(line.mydata == x)], collapse=",")
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

