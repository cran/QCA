`eqmcc` <-
function(mydata, outcome = "", conditions = c(""), incl.rem = FALSE,
         expl.1 = FALSE, expl.0 = FALSE, expl.ctr = FALSE, expl.mo = FALSE,
         incl.1 = FALSE, incl.0 = FALSE, incl.ctr = FALSE, incl.mo = FALSE,
         quiet = FALSE, chart = FALSE, use.letters = TRUE, show.cases = FALSE,
         uplow=TRUE) {
    
    if (!is.tt(mydata)) {
        verify.data(mydata, outcome, conditions, incl.rem, expl.1, expl.0,
                    expl.ctr, expl.mo, incl.1, incl.0, incl.ctr, incl.mo,
                    quiet, chart, use.letters, show.cases)
        
        tt <- truthTable(mydata, outcome, conditions, show.cases=TRUE, quiet=TRUE)
        if (all(conditions == c(""))) {
            conditions <- names(mydata)[-which(names(mydata)==outcome)]
        }
        mydata <- mydata[, c(conditions, outcome)]
    }
    else {
        mydata$tt <- mydata$tt[mydata$tt[, outcome] != "?", ]
        tt <- mydata
        mydata <- mydata$tt[, seq(length(mydata$noflevels) + 1)]
        if("cases" %in% gsub("^ *", "", gsub(" *$", "", names(tt$tt)))) {
            rownames(mydata) <- tt$casenames[tt$casenames != ""]
        }
    }
    
    if (quiet) {show.cases <- FALSE} # chart <- 
    
    noflevels <- tt$noflevels
    
    # if not quiet, print the truth table on the screen
    if (!quiet) {
        print.tt(tt, funqmcc=TRUE)
    }
    
    expl.incl <- c(1, 0, "C")[c(expl.1, expl.0, expl.ctr) | c(incl.1, incl.0, incl.ctr)]
    subset.tt <- tt$tt[, outcome] %in% expl.incl
    explain <- as.matrix(tt$tt[subset.tt, seq(length(noflevels))])
    explain <- matrix(as.numeric(explain), ncol=length(noflevels)) + 1
    
    subset.tt <- !tt$tt[, outcome] %in% expl.incl
    exclude <- as.matrix(tt$tt[subset.tt, seq(length(noflevels))])
    exclude <- matrix(as.numeric(exclude), ncol=length(noflevels)) + 1
    
    expl.args <- c(1, 0, "C")[c(expl.1, expl.0, expl.ctr)]
    subset.tt <- tt$tt[, outcome] %in% expl.args
    inputt <- as.matrix(tt$tt[subset.tt, seq(length(noflevels))])
    inputt <- matrix(as.numeric(inputt), ncol=length(noflevels)) + 1
    
    if (nrow(explain) == 0) {
        cat("\n")
        stop("Nothing to explain. Please check the truth table.\n\n", call. = FALSE)
    }
    
    if (nrow(exclude) == 0 & incl.rem) {
        cat("\n")
        stop(paste("All combinations have been included into analysis. The solution is 1.\n",
                   "Please check the truth table.", "\n\n", sep=""), call. = FALSE)
    }
    
    if (incl.rem) {
        primes <- sort(setdiff(findPrimes(noflevels + 1, explain), findPrimes(noflevels + 1, exclude)))
        index <- 0
        while ((index <- index + 1) < length(primes)) {
            primes <- setdiff(primes, findSubsets(noflevels + 1, primes[index], max(primes)))
        }
        primes <- getRow(noflevels + 1, primes)
    }
    else {
        minimized <- TRUE
        while (any(minimized)) {
            minimized <- logical(nrow(explain))
            distance <- dist(explain, method="manhattan")
            distance <- as.matrix(distance)
            distance[!upper.tri(distance)] <- NA
             # tbc means "to be compared"
            tbc <- as.matrix(which(distance == 1, arr.ind=TRUE))
            
            if (nrow(tbc) > 0) {
                differences <- t(apply(tbc, 1, function(idx) explain[idx[1], ] != explain[idx[2], ]))
                result <- matrix(0, nrow=0, ncol=ncol(differences))
                for (i in 1:nrow(differences)) {
                    stable.values <- explain[tbc[i, 1], !differences[i, ]]
                    subset.explain <- apply(explain[, !differences[i, ]], 1, function(x) all(x == stable.values))
                    if (sum(subset.explain) == noflevels[differences[i, ]]) {
                        minimized[subset.explain] <- TRUE
                        minimization.result <- explain[tbc[i, 1], ]
                        minimization.result[differences[i, ]] <- 0
                        result <- rbind(result, as.vector(minimization.result))
                    }
                }
            }
            if (sum(minimized) > 0) {
                explain <- rbind(explain[!minimized, ], unique(result))
            }
        }
        primes <- explain
    }
    
     # check if the condition names are not already letters
    alreadyletters <- sum(nchar(colnames(mydata)[-ncol(mydata)])) == ncol(mydata) - 1
    if (!all(as.matrix(mydata) %in% c(0, 1))) uplow <- FALSE
    collapse <- ifelse(alreadyletters & uplow, "", "*")
    changed <- FALSE
    
     # if not already letters and user specifies using letters for conditions, change it
    if (use.letters & !alreadyletters) {
        colnames(primes) <- colnames(inputt) <- LETTERS[1:ncol(primes)]
        changed <- TRUE
        collapse <- ifelse(uplow, "", "*")
        }
    else {
        colnames(primes) <- colnames(inputt) <- colnames(mydata[, seq(ncol(primes))])
        }
    
    initial <- apply(inputt, 1, writePrimeimp, collapse=collapse, uplow=uplow)
    
    output <- list()
    
     # create the prime implicants chart
    mtrx <- createChart(primes, inputt)
    reduced <- rowDominance2(mtrx, primes)
    
    primeimp <- apply(reduced$primes, 1, writePrimeimp, collapse=collapse, uplow=uplow)
    primeimpsort <- sortVector(primeimp)
    output$PIs <- primeimpsort
    mtrx <- reduced$mtrx[match(primeimpsort, primeimp), , drop=FALSE]
    rownames(mtrx) <- primeimpsort
    colnames(mtrx) <- initial
    
    sol.matrix <- solveChart(mtrx)
    output$k <- sol.matrix[[1]]
    if (length(sol.matrix)==1) {
        return(invisible(output))
    }
    
    sol.matrix <- sol.matrix[[2]]
    
    solution.list <- writeSolution(sol.matrix, mtrx)
    output$solution <- solution <- solution.list[[1]]
    ess.prime.imp <- rownames(mtrx)[solution.list[[2]]]
    
    if (chart) {
        cat("\n")
        mtrx2 <- mtrx
        rownames(mtrx2) <- paste(rownames(mtrx2), "")
        mtrx2[mtrx]  <- "x"
        mtrx2[!mtrx] <- "-"
        print(prettyTable(mtrx2))
    }
    
    cat("\n\n")
    
    if (!is.list(solution)) {
        cat(prettyString("Solution:", paste(sortVector(solution), collapse=" + ")))
        cat("\n")
    }
    else {
        cat("There are multiple solutions:\n\n")
        prettyNums <- formatC(seq(length(solution)), dig = nchar(length(solution))-1, flag = 0)
        for (i in seq(length(solution))) {
            preamble <- paste("Solution ", prettyNums[i], ":", sep="")
            str.solution <- paste(sortVector(solution[[i]]), collapse=" + ")
            cat(prettyString(preamble, str.solution))
        }
        cat("\n")
        if (length(ess.prime.imp) > 0) {
            preamble <- "Essential prime implicants:"
            str.implicants <- paste(sortVector(ess.prime.imp), collapse=" + ")
            cat(prettyString(preamble, str.implicants))
        }
        cat("\n")
    }
    
     # create a string vector of all prime implicants, sorted according to size
    all.primeimps <- NULL
    for (i in seq(length(solution))) {
        all.primeimps <- c(all.primeimps, solution[[i]])
        }
    all.primeimps <- sortVector(unique(all.primeimps))
    
     # print the lines from the initial data, which correspond to the minimized prime implicants
    if (show.cases) {
         # for start, mydata.rows will be a string with all _existing_ combinations (e.g. "AbcDe")
        mydata.rows <- createString(mydata[, -which(colnames(mydata) == outcome)], use.letters, uplow)
        
        mtrx <- demoChart(all.primeimps, mydata.rows, ifelse(use.letters & uplow, "", "*"))
        
         # replace mydata.rows with a vector of all rownames (case IDs) from the initial data
        mydata.rows <- rownames(mydata)
        
        cat("Correspondence to cases:\n")
        
        for (i in seq(length(all.primeimps))) {
            blanks <- paste(rep(" ", max(nchar(all.primeimps)) - nchar(all.primeimps[i])), collapse="")
            cat(blanks)
            preamble <- paste(all.primeimps[i], ":", sep="")
            lines.explained <- paste(mydata.rows[mtrx[i, ]], collapse="; ")
            cat(prettyString(preamble, lines.explained, blanks))
        }
        cat("\n")
    }
    
    if (nrow(explain) == 1) {
        preamble <- "NB:"
        warning.message <- paste("There is only one combination to be explained.",
                                 "The solution is simply that combination,",
                                 "this algorithm did not perform any minimization.")
        cat("\n")
        cat(prettyString(preamble, warning.message))
        cat("\n")
    }
    return(invisible(output))
}
