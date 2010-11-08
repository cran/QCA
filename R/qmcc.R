`qmcc` <-
function(mydata, outcome = "", conditions = c(""), incl.rem = FALSE,
         expl.1 = FALSE, expl.0 = FALSE, expl.ctr = FALSE, expl.mo = FALSE,
         incl.1 = FALSE, incl.0 = FALSE, incl.ctr = FALSE, incl.mo = FALSE,
         quiet = FALSE, details = TRUE, chart = FALSE, use.letters = TRUE,
         show.cases = FALSE, diffmat=TRUE) {
    
    if (!is.tt(mydata)) {
        verify.data(mydata, outcome, conditions, incl.rem, expl.1, expl.0, expl.ctr,
                    expl.mo, incl.1, incl.0, incl.ctr, incl.mo, quiet,
                    chart, use.letters, show.cases, diffmatrix)
        
        # check if the data present values other than 0 and 1
        if (!all(as.matrix(mydata) %in% c(0, 1))) {
            not.valid <- which(!(mydata == 0 | mydata == 1), arr.ind = TRUE)
            cat("\n")
            stop("The data present values other than 0 or 1.\nSee for example line ",
                 not.valid[1, 1], ' from variable "', colnames(mydata)[not.valid[1, 2]], '"\n\n', call. = FALSE, sep="")
        }
        
        tt <- truthTable(mydata, outcome, conditions, show.cases=TRUE, quiet=TRUE)
        if (all(conditions == c(""))) {
            conditions <- names(mydata)[-which(names(mydata) == outcome)]
        }
        mydata <- mydata[, c(conditions, outcome)]
    }
    else {
        mydata$tt <- mydata$tt[mydata$tt[, outcome] != "?", ]
        tt <- mydata
        mydata <- mydata$tt[, seq(length(mydata$noflevels) + 1)]
        if("cases" %in% gsub("^ *", "", gsub(" *$", "", names(tt$tt)))) {
            rownames(mydata) <- tt$casenames
        }
    }
    
    if (quiet) {details <- show.cases <- chart <- FALSE}
    
    # check if the user checked both something like "expl.1" AND "incl.1"
    if (expl.1 & incl.1) {
        cat("\nWarning: the presence of the outcome cannot be both explained and included\n\n")
        incl.1 <- FALSE
    }
    
    # if the user included some other values for minimization, there will be two
    # minimizations and the one with the smallest number of literals will be reported
    any.inclusions <- any(c(incl.0, incl.1, incl.ctr, incl.rem))
    repetitions <- ifelse(any.inclusions, 2, 1)
    
    # if not quiet, print the truth table on the screen
    if (!quiet) {
        print.tt(tt, funqmcc=TRUE)
    }
    
    line.tt <- tt$indexes
    
    # Compute the multiple bases.
    mbase <- rev(c(1, cumprod(rev(tt$noflevels + 1))))[-1]
    
    # Compute all possible line numbers - equivalent of the 2^k combinations
    totlines <- base3rows(length(tt$noflevels))
    
    if (details & diffmat) {
        cat("\n\nGenerating the differences matrix...", "\n")
    }
    
    if (diffmat) {
         # Allocate the matrix with all possible differences
        diffmatrix <- sapply(seq(length(tt$noflevels)), function(x) {
            as.vector(outer(seq_len(mbase[x]), seq(mbase[x], 3*mbase[1] - mbase[x] - 1, 3*mbase[x]), "+"))
        })
    }
    
    if (details & repetitions == 2 & any.inclusions) {
        cat("\nStep 1. Finding prime implicants...\n\n  For the explained and included values:", "\n\n")
    }
    else if (details & repetitions == 1) {
        cat("\nStep 1. Finding prime implicants...\n\n  For the explained values:", "\n\n")
    }
    
    
    for (repetition in 1:repetitions) {
        
         # create the vector of outcome values to subset for minimization
        if (repetitions == 2 & repetition == 1) {
            explain <- c(1, 0, "C")[c(expl.1, expl.0, expl.ctr) | c(incl.1, incl.0, incl.ctr)]
        }
        else {
            explain <- c(1, 0, "C")[c(expl.1, expl.0, expl.ctr)]
        }
        
        explain <- tt$tt[, outcome] %in% explain
        linenums <- copylinenums <- totlines[line.tt[explain]]
        if (incl.rem & repetition == 1) linenums <- sort(c(linenums, totlines[-line.tt]))
        
        if (all(is.na(linenums))) {
            cat("\n")
            stop("Nothing to explain. Please check the truth table.\n\n", call. = FALSE)
        }
        
        if (length(linenums) == prod(tt$noflevels)) {
            cat("\n")
            stop(paste("All combinations have been included into analysis. The solution is 1.\n",
                       "Please check the truth table.", "\n\n", sep=""), call. = FALSE)
        }
        
        
        if (details & repetition == 2) {
            cat("  Now finding prime implicants only for the explained values:", "\n\n")
        }
        
        
        iteration <- 1
        minimized <- TRUE
        
        while (any(minimized)) {
            if (details) {
                cat ("  Iteration ", iteration, "\n       Number of (unique) prime implicants:",
                formatC(length(linenums), big.mark=",", big.interval=3, format="fg"), "\n")
            } #, " (",
              #formatC(choose(nrow(input), 2), big.mark=",", big.interval=3, format="fg"), 
              #" possible paired comparisons)\n", sep="")}
            
            max.diffs <- ceiling(length(linenums)*length(tt$noflevels)/2)
            result <- matrix(nrow=max.diffs, ncol=2)
            startrow <- 1
            for (i in seq(length(tt$noflevels))) {
                if (diffmat) {
                    match.lines <- linenums[linenums %in% diffmatrix[, i]]
                }
                else {
                    match.lines <- linenums[linenums %in% outer(seq_len(mbase[i]), seq(mbase[i], 3*mbase[1] - mbase[i] - 1, 3*mbase[i]), "+")]
                }
                match.lines <- match.lines[(match.lines + mbase[i]) %in% linenums]
                length.match <- length(match.lines)
                if (length.match > 0) {
                    endrow <- startrow + length.match - 1
                    result[startrow:endrow, ] <- c(match.lines, match.lines + mbase[i])
                }
                startrow <- startrow + length.match
            }
            
            result <- result[-(startrow:max.diffs), , drop=FALSE]
            minimized <- linenums %in% result
            
            
            if (details) {
                cat("       Number of comparable pairs:", 
                formatC(nrow(result), big.mark=",", big.interval=3, format="fg"), "\n")
            }
            
            if (any(minimized)) {
                 # create the next input matrix, which will contain all rows from the initial
                 # input matrix which have not been minimized, plus the rows from the reduced vector
                 # using the formula 2x-y
                linenums <- unique(c(linenums[!minimized], 2*result[,1] - result[,2]))
                iteration <- iteration + 1
            }
            else {
                linenums <- sort(unique(linenums))
                if (repetitions == 2 & repetition == 1) {
                    linenums.incl <- linenums
                    if (details) {cat ("\n")}
                }
            }
        }
    }
    
    input <- getRow(tt$noflevels + 1, linenums)
    copyinput <- getRow(tt$noflevels + 1, copylinenums)
    if (repetitions == 2) {
        input.incl <- getRow(tt$noflevels + 1, linenums.incl)
    }
    
    # check if the condition names are not already letters
    alreadyletters <- sum(nchar(colnames(mydata)[-ncol(mydata)])) == ncol(mydata) - 1
    collapse <- ifelse(alreadyletters, "", "*")
    changed <- FALSE
    
    # if not already letters and user specifies using letters for conditions, change it
    if (use.letters & !alreadyletters) {
        colnames(input) <- colnames(copyinput) <- LETTERS[1:ncol(input)]
        if (repetitions == 2) {
            colnames(input.incl) <- LETTERS[1:ncol(input)]
        }
        changed <- TRUE
        collapse = ""
    }
    else {
        colnames(input) <- colnames(copyinput) <- colnames(mydata[, seq(ncol(mydata) - 1)])
        if (repetitions == 2) {
            colnames(input.incl) <- colnames(mydata[, seq(ncol(mydata) - 1)])
        }
    }
    
    initial <- apply(copyinput, 1, writePrimeimp, collapse=collapse, uplow=TRUE)
    
    output <- list()
    
    # create the prime implicants chart
    mtrx <- createChart(input, copyinput)
    reduced <- rowSums(mtrx) == 0
    if (any(reduced)) {
        mtrx <- mtrx[!reduced, , drop=FALSE]
        input <- input[!reduced, , drop=FALSE]
    }
    
    primeimp <- apply(input, 1, writePrimeimp, collapse=collapse, uplow=TRUE)
    primeimpsort <- sortVector(primeimp)
    mtrx <- mtrx[match(primeimpsort, primeimp), , drop=FALSE]
    rownames(mtrx) <- output$PIs <- primeimpsort
    colnames(mtrx) <- initial
    
    reduced <- rowDominance(mtrx)
    
    if (any(reduced)) {
        mtrxDom <- mtrx[!reduced, , drop=FALSE]
        rownames(mtrxDom) <- output$PIs <- primeimpsort[!reduced]
    }
    else {
        mtrxDom <- mtrx
    }
    
    sol.matrix <- solveChart(mtrxDom)
    
    output$k <- sol.matrix[[1]]
    if (length(sol.matrix)==1) {
        return(invisible(output))
    }
    
    sol.matrix <- sol.matrix[[2]]
    
    if (repetitions == 2) {
         # do the same thing if the user included other values for minimization
        mtrx.incl <- createChart(input.incl, copyinput)
        reduced <- rowSums(mtrx.incl) == 0
        if (length(reduced) > 0) {
            mtrx.incl <- mtrx.incl[!reduced, , drop=FALSE]
            input.incl <- input.incl[!reduced, , drop=FALSE]
        }
        
        primeimp.incl <- apply(input.incl, 1, writePrimeimp, collapse=collapse, uplow=TRUE)
        primeimpsort <- sortVector(primeimp.incl)
        mtrx.incl <- mtrx.incl[match(primeimpsort, primeimp.incl), , drop=FALSE]
        rownames(mtrx.incl) <- output$PIs <- primeimpsort
        colnames(mtrx.incl) <- initial
        reduced <- rowDominance(mtrx.incl)
        
        if (any(reduced)) {
            mtrxDom.incl <- mtrx.incl[!reduced, , drop=FALSE]
            rownames(mtrxDom.incl) <- output$PIs <- primeimpsort[!reduced]
        }
        else {
            mtrxDom.incl <- mtrx.incl
        }
        
        sol.matrix.incl <- solveChart(mtrxDom.incl)
        output$k <- sol.matrix.incl[[1]]
        if (length(sol.matrix.incl)==1) {
            return(invisible(output))
        }
        sol.matrix.incl <- sol.matrix.incl[[2]]
    }
    
    solution.list <- writeSolution(sol.matrix, mtrxDom)
    output$solution <- solution <- solution.list[[1]]
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
        cat("\nStep 2. Removing redundant prime implicants...\n")
    }
    
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
        cat(prettyString("Solution:", paste(sortVector(solution), collapse=" + ")))
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
        mydata.rows <- createString(mydata[, -which(colnames(mydata) == outcome)], use.letters)
        mtrx <- demoChart(all.primeimps, mydata.rows, ifelse(use.letters, "", "*"))
        
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
    
    if (length(linenums) == 1) {
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
