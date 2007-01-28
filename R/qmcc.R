`qmcc` <-
function(mydata, outcome = "", conditions = c(""), incl.rem = FALSE,
         expl.1 = FALSE, expl.0 = FALSE, expl.ctr = FALSE, expl.mo = FALSE,
         incl.1 = FALSE, incl.0 = FALSE, incl.ctr = FALSE, incl.mo = FALSE,
         quiet = FALSE, details = FALSE, chart = FALSE, use.letters = TRUE,
         show.cases = FALSE) {
    
    verify.qmcc(mydata, outcome, conditions, incl.rem, expl.1, expl.0, expl.ctr,
                expl.mo, incl.1, incl.0, incl.ctr, incl.mo, quiet, details,
                chart, use.letters, show.cases, tt)
    
    if (quiet) {details <- show.cases <- chart <- FALSE}
    
     # if the outcome variable is not the last one, it will be placed the last
    if (which(colnames(mydata) %in% outcome) != ncol(mydata)) {
        outcm.name <- colnames(mydata)[which(colnames(mydata) == outcome)]
        outcm   <- mydata[,  which(colnames(mydata) == outcome)]
        mydata  <- mydata[, -which(colnames(mydata) == outcome)]
        mydata <- cbind(mydata, outcm)
        colnames(mydata)[ncol(mydata)] <- outcm.name
        }

     # coerce the input data to a matrix
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
    
     # if the user included some other values for minimization, there will be two
     # minimizations and the one with the smallest number of literals will be reported
    any.inclusions <- sum(incl.0, incl.1, incl.ctr, incl.rem)
    repetitions <- ifelse(any.inclusions, 2, 1)
    
    if (!quiet) {cat("\n")}
    
    tt <- truthTable(mydata, outcome=outcome, show.cases=TRUE, inside=TRUE)
    
    line.tt <- tt$indexes
    
     # Compute the multiple bases.
    mbase <- c(rev(cumprod(rev(tt$noflevels + 1))), 1)[-1]
    
    binarymatrix <- createMatrix(tt$noflevels) + 1
    totlines <- rowSums(sapply(1:length(tt$noflevels), function(x) {
        binarymatrix[, x]*mbase[x]
        })) + 1
    
    rm(binarymatrix)
    
     # this line is about 10 times slower than the above code, because it works on rows
    #totlines <- colSums(apply(createMatrix(tt$noflevels) + 1, 1, function(x) x*mbase)) + 1
    
    for (repetition in 1:repetitions) {
        
         # create the vector of outcome values to subset for minimization
        if (repetitions == 2 & repetition == 1) {
            explain <- c(1, 0, "C")[c(expl.1, expl.0, expl.ctr) | c(incl.1, incl.0, incl.ctr)]
            }
        else {
            explain <- c(1, 0, "C")[c(expl.1, expl.0, expl.ctr)]
            }
        
        explain <- tt$tt[[outcome]] %in% explain
        linenums <- totlines[line.tt[explain]]
        if (incl.rem & repetition == 1) linenums <- sort(c(linenums, totlines[-line.tt]))
        
        
        if (all(is.na(linenums))) {
            cat("\n")
            stop("Nothing to explain. Please check the truth table.\n\n", call. = FALSE)
            }
        else if (length(linenums) == 1) {
            cat("\n")
            stop("Nothing to reduce. There is only one combination to be explained.\n\n",
                 call. = FALSE)
            }
        else if (length(linenums) == prod(tt$noflevels)) {
            cat("\n")
            stop(paste("All combinations have been included into analysis. The solution is 1.\n",
                       "Please check the truth table.", "\n\n", sep=""), call. = FALSE)
            }
        
         # print the truthtable on the screen, if not quiet
        if (!quiet & repetition == 1) {
            rownames(tt$tt) <- paste(format(1:nrow(tt$tt)), " ")
            print(prettyTable(tt$tt))
            }
        
        
         # increment the input matrix by 1 (so that 0 will represent a minimized literal)
        biginput <- createMatrix(tt$noflevels + 1)
        colnames(biginput) <- colnames(tt$tt[1:(ncol(mydata) - 1)])
        
        
         # check if the condition names are not already letters
        alreadyletters <- sum(nchar(colnames(mydata))) == ncol(mydata)
        ifelse(alreadyletters, co11apse <- "", co11apse <- "*")
        changed <- FALSE
        
         # if not already letters and user specifies using letters for conditions, change it
        if (use.letters & !alreadyletters) {
            colnames(biginput) <- LETTERS[1:ncol(biginput)]
            changed <- TRUE
            co11apse = ""
            }
        
        input <- copyinput <- biginput[linenums, ]
        
        if (details & repetitions == 2 & repetition == 1) {
            cat("\nStep 1. Finding prime implicants for explained and included values:", "\n\n")
            }
        else if (details & repetitions == 2 & repetition == 2) {
            cat("  Now finding prime implicants only for the explained values:", "\n\n")
            }
        else if (details & repetitions == 1) {
            cat("\nStep 1. Finding prime implicants for the explained values:", "\n\n")
            }
        
        
        iteration <- minimized <- 1
        
        while (any(minimized)) {
            
            if (details) {cat ("  Iteration ", iteration,
                "\n       Number of (unique) prime implicants:",
                 formatC(nrow(input), big.mark=",", big.interval=3, format="fg"),
                 "\n")} #, " (",
                              #formatC(choose(nrow(input), 2), big.mark=",", big.interval=3, format="fg"), 
                              #" possible paired comparisons)\n", sep="")}
            
            minimized <- logical(length(linenums))
            reduced <- c()
            
            
             # working on columns is faster than working on rows
             # to preserve memory usage, I avoid creating another two big matrices; instead,
             # I work on columns using logical indexes from the input matrix
            for (i in 1:ncol(input)) {
                trueline <- linenums[!(input[, i] - 1)]
                addon <- trueline + mbase[i]
                if (any(addon %in% linenums)) {
                    minimized[!(input[, i] - 1)] <- minimized[!(input[, i] - 1)] | addon %in% linenums
                    minimized[linenums %in% addon] <- TRUE
                    reduced <- c(reduced, 2*linenums[!(input[, i] - 1)][addon %in% linenums] - 
                                          addon[addon %in% linenums])
                    }
                }
            
            if (details) {cat("       Number of comparable pairs:", 
                              formatC(length(reduced), big.mark=",", big.interval=3, format="fg"),
                              "\n")}
            
            if (any(minimized)) {
                 # create the next input matrix, which will contain all rows from the initial
                 # input matrix which have not been minimized, plus the rows from the reduced vector
                linenums <- sort(unique(c(linenums[!minimized], reduced)))
                input <- biginput[linenums, ]
                iteration <- iteration + 1
                }
            else {
                colnames(input) <- colnames(copyinput)
                 # if no further minimization is possible, prepare the prime implicants vector
                primeimp <- apply(unique(input), 1, writePrimeimp, co11apse=co11apse)
                initial <- as.vector(apply(copyinput, 1, writePrimeimp, co11apse=co11apse))
                if (repetitions == 2 & repetition == 1) {
                    primeimp.incl <- sortVector(primeimp)
                    input.incl <- input[match(primeimp.incl, primeimp), ]
                    if (details) {cat ("\n")}
                    }
                }
            }
        }
    
     # create the prime implicants chart
    primeimpsort <- sortVector(primeimp)
    mtrx <- createChart(input[match(primeimpsort, primeimp), ], copyinput, primeimpsort, initial)
    mtrxDom <- rowDominance(mtrx)
    sol.matrix <- solveChart(mtrxDom)
    
    if (repetitions == 2 & repetition == 2) {
         # do the same thing if the user included other values for minimization
        mtrx.incl <- createChart(input.incl, copyinput, primeimp.incl, initial)
        mtrxDom.incl <- rowDominance(mtrx.incl)
        sol.matrix.incl <- solveChart(mtrx.incl)
        }
    
    solution.list <- writeSolution(sol.matrix, mtrxDom)
    solution <- solution.list[[1]]
    ess.prime.imp <- rownames(mtrxDom)[solution.list[[2]]]
    
    if (repetitions == 2) {
         # compare the two solutions and retain the one with the smallest number of conditions (literals)
        solution.list.incl <- writeSolution(sol.matrix.incl, mtrxDom.incl)
        solution.incl <- solution.list.incl[[1]]
        ess.prime.imp.incl <- rownames(mtrxDom.incl)[solution.list.incl[[2]]]
        ncond <- unique(toupper(unlist(strsplit(solution[[1]], ""))))
        ncond.incl <- unique(toupper(unlist(strsplit(solution.incl[[1]], ""))))
        if (length(ncond.incl) < length(ncond)) {
            solution <- solution.incl
            primeimp <- primeimp.incl
            ess.prime.imp <- ess.prime.imp.incl
            mtrx <- mtrx.incl
            if (details) {cat("\n  Solution with minimum number of literals found for", 
                              "explained _and_ included values\n")}
            }
        else {
            if (details) {cat("\n  Solution with minimum number of literals found for", 
                              "the explained values only\n")}
            }
        }
    
    if (details) {
        cat("\nStep 2. Removing redundant prime implicants:\n\n")
        }
    
    
    if (chart) {
        mtrx2 <- mtrx <- mtrx[rowSums(mtrx) > 0, ]
        cat("\n")
         # if not quiet, print the prime implicants chart
        if (!quiet) {
            rownames(mtrx2) <- paste(rownames(mtrx2), "")
            mtrx2[mtrx]  <- "x"
            mtrx2[!mtrx] <- "-"
            print(prettyTable(mtrx2))
            }
        }
    
    cat("\n")
    
    if (!is.list(solution)) {
        cat("Solution: ", prettyString(solution, 70, 10, " + "), "\n\n", sep="")
        }
    else {
        cat("There are multiple solutions:\n")
        for (i in 1:length(solution)) {
            cat("Solution", paste(i, ": ", sep=""),
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
        mtrx <- demoChart(all.primeimps, mydata.rows)
        
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

