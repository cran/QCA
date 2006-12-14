`qmcc` <-
function(mydata, outcome = "", conditions = c(""), incl.rem = FALSE,
         expl.1 = FALSE, expl.0 = FALSE, expl.ctr = FALSE, expl.mo = FALSE,
         incl.1 = FALSE, incl.0 = FALSE, incl.ctr = FALSE, incl.mo = FALSE,
         quiet = FALSE, details = FALSE, chart = FALSE, use.letters = TRUE,
         show.cases = FALSE, tt = FALSE) {
    
    verify.qmcc(mydata, outcome, conditions, incl.rem, expl.1, expl.0, expl.ctr,
                expl.mo, incl.1, incl.0, incl.ctr, incl.mo, quiet, details,
                chart, use.letters, show.cases, tt)
    
    if (quiet) {complete <- details <- show.cases <- chart <- FALSE}
    if (details) {chart <- TRUE}
    
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
    ifelse(sum(incl.0, incl.1, incl.ctr, incl.rem) > 0, repetitions <- 2, repetitions <- 1)
    
    if (!quiet) {cat("\n")}
    
    for (repetition in 1:repetitions) {
        
         # create the vector of outcome values to subset for minimization
        if (repetitions == 2 & repetition == 1) {
            explain.values <- c(expl.1, expl.0, expl.ctr, incl.1, incl.0, incl.ctr, incl.rem)
            explain <- unique(c(1, 0, "C", 1, 0, "C", "?")[explain.values])
            }
        else {
            explain <- c(1, 0, "C")[c(expl.1, expl.0, expl.ctr)]
            }
        
         # if not already thuthtable, create it based on input data
        ifelse(!tt, ttmydata <- print.tt <- truthTable(mydata, outcome=outcome, inside=TRUE, show.cases=TRUE), 
                    ttmydata <- print.tt <- as.data.frame(matrix(as.character(mydata), ncol=ncol(mydata))))
        rownames(ttmydata) <- 1:nrow(ttmydata)
        
         # select the rows from print.tt table which have a 0, 1 or a contradiction in the outcome
        print.tt <- print.tt[!print.tt[, outcome] %in% "?", ]
        
         # select only the conditions and the rows with the values to be explained
        ttmydata <- ttmydata[ttmydata[, outcome] %in% explain, 1:(ncol(mydata) - 1)]
        
        if (is.null(nrow(ttmydata)) | nrow(ttmydata) == 0) {
            cat("\n")
            stop("Nothing to explain. Please check the truth table.\n\n", call. = FALSE)
            }
        else if (nrow(ttmydata) == 1) {
            cat("\n")
            stop("Nothing to reduce. There is only one combination to be explained.\n\n",
                 call. = FALSE)
            }
        else if (nrow(ttmydata) == 2^ncol(ttmydata)) {
            cat("\n")
            stop(paste("All combinations have been included into analysis. The solution is 1.\n",
                       "Please check the truth table.", "\n\n", sep=""), call. = FALSE)
            }
        
         # print the truthtable on the screen, if not quiet
        if (!quiet & repetition == 1) print(prettyTable(print.tt))
        
        
         # check if the condition names are not already letters
        alreadyletters <- sum(nchar(colnames(ttmydata))) == ncol(ttmydata)
        ifelse(alreadyletters, co11apse <- "", co11apse <- "*")
        changed <- FALSE
        
         # if not already letters and user specifies using letters for conditions, change it
        if (use.letters & !alreadyletters) {
            colnames(ttmydata) <- LETTERS[1:ncol(ttmydata)]
            changed <- TRUE
            co11apse = ""
            }
        
        input <- copyinput <- apply(ttmydata, 2, as.character)
        
        minimized <- iteration <- 1
        
        if (details & repetitions == 2 & repetition == 1) {
            cat("\nStep 1. Finding prime implicants for explained and included values:", "\n\n")
            }
        else if (details & repetitions == 2 & repetition == 2) {
            cat("  Now finding prime implicants only for the explained values:", "\n\n")
            }
        else if (details & repetitions == 1) {
            cat("\nStep 1. Finding prime implicants for the explained values:", "\n\n")
            }
        
        while (sum(minimized) > 0) {
           
            if (details) {cat ("  Iteration ", iteration,
                              "\n       Number of prime implicants: ", nrow(input), " (",
                              formatC(choose(nrow(input), 2), big.mark=",", big.interval=3, format="fg"), 
                              " paired comparisons)\n", sep="")}
            
             # to check which prime implicant was minimized (or not, because is initialized with FALSE)
            minimized <- logical(nrow(input))
             # the function "daisy" from package "cluster" calculates the distance between
             # all pairs of rows from the input dataset; if two vectors differ by only one value,
             # the distance between them is set to 1.
             # Solution suggested by Martin Maechler
            distance <- dist(input, method="manhattan")
            distance <- as.matrix(distance)
            distance[!upper.tri(distance)] <- NA
            to.be.compared <- as.matrix(which(distance == 1, arr.ind=TRUE))
            
            if (details) {cat("       Number of paired comparisons which differ by only one literal: ", 
                              nrow(to.be.compared), "\n", sep="")}
            
            if (nrow(to.be.compared) > 0) {
                 # create a matrix with the results from comparing all pairs of rows
                logical.result <- apply(to.be.compared, 1, function(idx) input[idx[1], ] == input[idx[2], ])
                compare.minimized <- unique(as.vector(to.be.compared))
                 # the result matrix will contain all rows from the input matrix that have been minimized...
                result <- sapply(1:nrow(to.be.compared), function(idx) input[to.be.compared[idx, 1], ])
                 # thus each row in the result matrix has only one difference, which is replaced by "x"
                result[!logical.result] <- -1
                 # mark which prime implicant was minimized
                minimized[compare.minimized] <- TRUE
                }
            
            if (sum(minimized) > 0) {
                 # create the next input matrix, which will contain all rows from the initial
                 # input matrix which have not been minimized, plus the rows from the result matrix
                input <- rbind(input[!minimized, ], unique(t(result)))
                iteration <- iteration + 1
                }
            else {
                colnames(input) <- colnames(copyinput)
                 # if no further minimization is possible, prepare the prime implicants vector
                primeimp <- sortVector(apply(unique(input), 1, writePrimeimp, co11apse=co11apse))
                copyinput <- as.vector(apply(copyinput, 1, writePrimeimp, co11apse=co11apse))
                if (repetitions == 2 & repetition == 1) {
                    primeimp.incl <- primeimp
                    if (details) {cat ("\n")}
                    }
                }
            }
        }
    
     # create the prime implicants chart
    mtrx <- mtrx2 <- createChart(primeimp, copyinput)
    sol.matrix <- solveChart(mtrx)
    
    if (repetitions == 2 & repetition == 2) {
         # do the same thing if the user included other values for minimization
        mtrx.incl <- createChart(primeimp.incl, copyinput)
        sol.matrix.incl <- solveChart(mtrx.incl)
        }
    
    
    solution <- writeSolution(sol.matrix, mtrx)[[1]]
    ess.prime.imp <- primeimp[writeSolution(sol.matrix, mtrx)[[2]]]
    
    if (repetitions == 2) {
         # compare the two solutions and retain the one with the smallest number of conditions (literals)
        solution.incl <- writeSolution(sol.matrix.incl, mtrx.incl)[[1]]
        ess.prime.imp.incl <- primeimp.incl[writeSolution(sol.matrix.incl, mtrx.incl)[[2]]]
        ncond <- unique(toupper(unlist(strsplit(solution[[1]], ""))))
        ncond.incl <- unique(toupper(unlist(strsplit(solution.incl[[1]], ""))))
        if (length(ncond.incl) < length(ncond)) {
            solution <- solution.incl
            primeimp <- primeimp.incl
            ess.prime.imp <- ess.prime.imp.incl
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
        mtrx <- createChart(all.primeimps, mydata.rows)
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

