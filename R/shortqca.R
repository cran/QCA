`shortqca` <-
function(mydata, outcome = "", conditions = c(""), incl.rem = FALSE,
         expl.1 = FALSE, expl.0 = FALSE, expl.ctr = FALSE, expl.mo = FALSE,
         incl.1 = FALSE, incl.0 = FALSE, incl.ctr = FALSE, incl.mo = FALSE,
         quiet = FALSE, chart = FALSE, use.letters = TRUE, show.cases = FALSE) {
    
    verify.qmcc(mydata, outcome, conditions, incl.rem, expl.1, expl.0, expl.ctr,
                expl.mo, incl.1, incl.0, incl.ctr, incl.mo, quiet,
                chart, use.letters, show.cases, diffmatrix)
    
    if (all(conditions == c(""))) {
        conditions <- names(mydata)[-which(names(mydata)==outcome)]
        }
    mydata <- mydata[, c(conditions, outcome)]
    
    if (quiet) {show.cases <- chart <- FALSE}
    
     # coerce the primes data to a matrix
    mydata <- as.matrix(mydata)
    
     # checking for complete data (without missings)
    rows.with.missings <- which(is.na(rowSums(mydata)))
    if (length(rows.with.missings) > 0) {
        cat("\n")
        stop("The following rows have missing data:\n",
             paste(rows.with.missings, collapse=", "), "\n\n", sep="")
        }
    
     # check if the data present values other than 0 and 1
    if (!all(mydata %in% c(0,1))) {
        not.valid <- which(!(mydata == 0 | mydata == 1), arr.ind = TRUE)
        cat("\n")
        stop("The data present values other than 0 or 1.\nSee for example line ",
             not.valid[1, 1], ' from variable "', colnames(mydata)[not.valid[1, 2]], '"\n\n', call. = FALSE, sep="")
        }
    
     # check if the user checked both something like "expl.1" AND "incl.1"
    if (expl.1 & incl.1) {
        cat("\nWarning: the presence of the outcome cannot be both explained\n\n")
        incl.1 <- FALSE
        }
    
     # if the user included some other values for minimization, there will be two
     # minimizations and the one with the smallest number of literals will be reported
    any.inclusions <- any(c(incl.0, incl.1, incl.ctr, incl.rem))
    repetitions <- ifelse(any.inclusions, 2, 1)
    
    tt <- truthTable(mydata, outcome, conditions, show.cases=TRUE, inside=TRUE)
    
     # print the truthtable on the screen, if not quiet
    if (!quiet) {
        cat("\n")
        rownames(tt$tt) <- paste(format(1:nrow(tt$tt)), " ")
        print(prettyTable(tt$tt))
        }
    
    line.tt <- tt$indexes
    
     # Compute the multiple bases.
    mbase <- c(rev(cumprod(rev(tt$noflevels + 1))), 1)[-1]
    
     # Compute all possible line numbers - equivalent of the 2^k combinations
    totlines <- base3rows(length(tt$noflevels))
    
    explain <- c(1, 0, "C")[c(expl.1, expl.0, expl.ctr) | c(incl.1, incl.0, incl.ctr)]
    explain.indexes <- tt$indexes[tt$tt[[outcome]] %in% explain]
    explain <- totlines[explain.indexes]
    remainders <- totlines[-tt$indexes]
    exclude <- setdiff(totlines, c(explain, remainders))
    
    inputt <- c(1, 0, "C")[c(expl.1, expl.0, expl.ctr)]
    explain.indexes <- tt$indexes[tt$tt[[outcome]] %in% inputt]
    inputt <- totlines[explain.indexes]
    
    if (all(is.na(explain))) {
        cat("\n")
        stop("Nothing to explain. Please check the truth table.\n\n", call. = FALSE)
        }
    else if (length(explain) == 1) {
        cat("\n")
        stop("Nothing to reduce. There is only one combination to be explained.\n\n",
             call. = FALSE)
        }
    else if (length(explain) == prod(tt$noflevels)) {
        cat("\n")
        stop(paste("All combinations have been included into analysis. The solution is 1.\n",
                   "Please check the truth table.", "\n\n", sep=""), call. = FALSE)
        }
    
    
    ###########################
    ###########################
    ###########################
    
    if (incl.rem) {
        `cucu` <- function (noflevels, row.no) {
            mbase <- c(rev(cumprod(rev(noflevels + 1))), 1)[-1]
            mbasep3 <- getRow(noflevels + 1, row.no)*mbase
            unique(colSums(t(createMatrix(noflevels)[-1, ])*rev(mbasep3))) + 1
            }
        
        primes <- unique(as.vector(sapply(explain, function(x) cucu(tt$noflevels, x))))
        notprimes <- unique(as.vector(sapply(exclude, function(x) cucu(tt$noflevels, x))))
        primes <- sort(primes[!primes %in% notprimes])
        mbase <- cumprod(tt$noflevels + 1)
        start.pos <- 1
        while (start.pos < length(primes)) {
            toadd <- mbase[which(primes[start.pos] <= mbase)[1]]
            if (primes[start.pos] + toadd > max(primes)) break
            primes <- primes[!primes %in% seq(primes[start.pos] + toadd, max(primes), toadd)]
            start.pos <- start.pos + 1
            }
        }
    else {
        minimized <- 1
        primes <- inputt <- getRow(tt$noflevels + 1, explain)
        while (any(minimized)) {
            minimized <- logical(nrow(primes))
            distance <- dist(primes, method="manhattan")
            distance <- as.matrix(distance)
            distance[!upper.tri(distance)] <- NA
            to.be.compared <- as.matrix(which(distance == 1, arr.ind=TRUE))
            
            if (nrow(to.be.compared) > 0) {
                logical.result <- apply(to.be.compared, 1, function(idx) primes[idx[1], ] == primes[idx[2], ])
                compare.minimized <- unique(as.vector(to.be.compared))
                result <- sapply(1:nrow(to.be.compared), function(idx) primes[to.be.compared[idx, 1], ])
                result[!logical.result] <- 0
                minimized[compare.minimized] <- TRUE
                }
            if (sum(minimized) > 0) {
                primes <- rbind(primes[!minimized, ], unique(t(result)))
                }
            }
        }
    
    
    if (incl.rem) {
        primes <- getRow(tt$noflevels + 1, primes)
        inputt <- getRow(tt$noflevels + 1, inputt)
        primes <- primes[!rowSums(primes > 0) > 4, ]
        }
    
     # check if the condition names are not already letters
    alreadyletters <- sum(nchar(colnames(mydata)[-ncol(mydata)])) == ncol(mydata) - 1
    co11apse <- ifelse(alreadyletters, "", "*")
    changed <- FALSE
    
     # if not already letters and user specifies using letters for conditions, change it
    if (use.letters & !alreadyletters) {
        colnames(primes) <- colnames(inputt) <- LETTERS[1:ncol(primes)]
        changed <- TRUE
        co11apse = ""
        }
    else {
        colnames(primes) <- colnames(inputt) <- colnames(mydata[, seq(ncol(mydata) - 1)])
        }
    
    initial <- apply(inputt, 1, writePrimeimp, co11apse=co11apse)
    
     # create the prime implicants chart
    mtrx <- createChart(primes, inputt)
    
    reduced <- rowDominanceShort(mtrx, primes)
    if (length(reduced) > 0) {
        mtrx <- mtrx[!reduced, , drop=FALSE]
        primes <- primes[!reduced, , drop=FALSE]
        }
    
    primeimp <- apply(primes, 1, writePrimeimp, co11apse=co11apse)
    primeimpsort <- sortVector(primeimp)
    mtrx <- mtrx[match(primeimpsort, primeimp), , drop=FALSE]
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
                    cat(" ", letters[i], "is the absence of", varnames[i], "\n")
                    }
                }
            }
        cat("\n")
        }
    }

