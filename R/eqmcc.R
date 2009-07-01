`eqmcc` <-
function(mydata, outcome = "", conditions = c(""), incl.rem = FALSE,
         expl.1 = FALSE, expl.0 = FALSE, expl.ctr = FALSE, expl.mo = FALSE,
         incl.1 = FALSE, incl.0 = FALSE, incl.ctr = FALSE, incl.mo = FALSE,
         quiet = FALSE, chart = FALSE, use.letters = TRUE, show.cases = FALSE) {
    
    
    if (!is.tt(mydata)) {
        verify.qmcc(mydata, outcome, conditions, incl.rem, expl.1, expl.0,
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
    
    if (quiet) {show.cases <- chart <- FALSE}
    
    # check if the user checked both something like "expl.1" AND "incl.1"
    if (expl.1 & incl.1) {
        cat("\nWarning: the presence of the outcome cannot be both explained and included\n\n")
        incl.1 <- FALSE
    }
    
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
    #else if (nrow(explain) == 1) {
    #    cat("\n")
    #    stop("Nothing to reduce. There is only one combination to be explained.\n\n",
    #         call. = FALSE)
    #}
    
    if (nrow(exclude) == 0 & incl.rem) {
        cat("\n")
        stop(paste("All combinations have been included into analysis. The solution is 1.\n",
                   "Please check the truth table.", "\n\n", sep=""), call. = FALSE)
    }
    
    
    if (incl.rem) {
        primes <- sort(setdiff(findPrimes(noflevels, explain), findPrimes(noflevels, exclude)))
        index <- 0
        while ((index <- index + 1) < length(primes)) {
            primes <- setdiff(primes, findSubsets(noflevels, primes[index], max(primes)))
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
            to.be.compared <- as.matrix(which(distance == 1, arr.ind=TRUE))
            
            if (nrow(to.be.compared) > 0) {
                logical.result <- apply(to.be.compared, 1, function(idx) explain[idx[1], ] == explain[idx[2], ])
                compare.minimized <- unique(as.vector(to.be.compared))
                result <- sapply(1:nrow(to.be.compared), function(idx) explain[to.be.compared[idx, 1], ])
                result[!logical.result] <- 0
                minimized[compare.minimized] <- TRUE
            }
            if (sum(minimized) > 0) {
                explain <- rbind(explain[!minimized, ], unique(t(result)))
            }
        }
        primes <- explain
    }
    
     # check if the condition names are not already letters
    alreadyletters <- sum(nchar(colnames(mydata)[-ncol(mydata)])) == ncol(mydata) - 1
    collapse <- ifelse(alreadyletters, "", "*")
    changed <- FALSE
    
     # if not already letters and user specifies using letters for conditions, change it
    if (use.letters & !alreadyletters) {
        colnames(primes) <- colnames(inputt) <- LETTERS[1:ncol(primes)]
        changed <- TRUE
        collapse = ""
        }
    else {
        colnames(primes) <- colnames(inputt) <- colnames(mydata[, seq(ncol(mydata) - 1)])
        }
    
    initial <- apply(inputt, 1, writePrimeimp, collapse=collapse)
    
     # create the prime implicants chart
    mtrx <- createChart(primes, inputt)
    reduced <- rowDominance2(mtrx, primes)
    
    primeimp <- apply(reduced$primes, 1, writePrimeimp, collapse=collapse)
    primeimpsort <- sortVector(primeimp)
    mtrx <- reduced$mtrx[match(primeimpsort, primeimp), , drop=FALSE]
    rownames(mtrx) <- primeimpsort
    colnames(mtrx) <- initial
    
    sol.matrix <- solveChart(mtrx)
    
    solution.list <- writeSolution(sol.matrix, mtrx)
    solution <- solution.list[[1]]
    ess.prime.imp <- rownames(mtrx)[solution.list[[2]]]
    
    if (chart) {
        cat("\n")
        mtrx2 <- mtrx
         # if not quiet, print the prime implicants chart
        if (!quiet) {
            rownames(mtrx2) <- paste(rownames(mtrx2), "")
            mtrx2[mtrx]  <- "x"
            mtrx2[!mtrx] <- "-"
            print(prettyTable(mtrx2))
            }
        }
    
    cat("\n\n")
    
    if (!is.list(solution)) {
        cat("Solution: ", prettyString(solution, 70, 10, " + "), "\n\n", sep="")
        }
    else {
        cat("There are multiple solutions:\n")
        prettyNums <- formatC(seq(length(solution)), dig = nchar(length(solution))-1, flag = 0)
        for (i in seq(length(solution))) {
            cat("Solution ", paste(prettyNums[i], ": ", sep=""),
                prettyString(sortVector(solution[[i]]), 70, 11, " + "), "\n", sep="")
            }
        if (length(ess.prime.imp) > 0) {
            cat("Essential prime implicants: ", 
                prettyString(sortVector(ess.prime.imp), 53, 28, " + "), "\n", sep="")
            }
        cat("\n")
        }
    
    
     # create a string vector of all prime implicants, sorted according to size
    all.primeimps <- NULL
    for (i in 1:length(solution)) {
        all.primeimps <- c(all.primeimps, solution[[i]])
        }
    all.primeimps <- sortVector(unique(all.primeimps))
    
     # print the lines from the initial data, which correspond to the minimized prime implicants
    if (show.cases) {
         # for start, mydata.rows will be a string with all _existing_ combinations (e.g. "AbcDe")
        mydata.rows <- createString(mydata[, -which(colnames(mydata) == outcome)], use.letters)
        mtrx <- demoChart(all.primeimps, mydata.rows, use.letters)
        
         # replace mydata.rows with a vector of all rownames (case IDs) from the initial data
        mydata.rows <- rownames(mydata)
        
         # check which is the largest number of characters in the prime implicant names
        max.length <- max(nchar(all.primeimps))
        for (i in 1:length(all.primeimps)) {
            rows.explained <- mydata.rows[mtrx[i, ]]
            lines.explained <- paste(rows.explained, collapse="; ")
            if (nchar(lines.explained) > 50) {
                lines.explained <- prettyString(rows.explained, 50, max.length + 22, "; ")
                }
            cat(all.primeimps[i], 
                paste(rep(" ", max.length - nchar(all.primeimps[i]) + 1), collapse=""),
                "correspond to lines: ", lines.explained, "\n", sep="")
            }
        cat("\n")
        }
    
     # print which letter correspond to which condition
    if (changed) {
        conditions <- sort(unique(unlist(strsplit(all.primeimps, NULL))))
        varnames <- colnames(mydata)
        var.cond <- match(toupper(conditions), LETTERS)
        for (i in unique(var.cond)) {
            if (length(which(var.cond == i)) > 1) {
                cat(paste(letters[i], "/", LETTERS[i], sep=""), "is the absence/presence of", varnames[i], "\n")
                }
            else {
                if (conditions[which(var.cond == i)] %in% LETTERS) {
                    cat(" ", LETTERS[i], "is the presence of", varnames[i], "\n")
                    }
                else {
                    cat(" ", letters[i], "is the absence  of", varnames[i], "\n")
                    }
                }
            }
        cat("\n")
        }
    }

