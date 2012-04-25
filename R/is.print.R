`is.tt` <-
function(x) {
    inherits(x, "tt")
}



`is.qca` <-
function(x) {
    inherits(x, "qca")
}



`print.tt` <-
function(x, ...) {
    if (prod(x$noflevels) > 1024) {
        cat("\n")
        cat(paste("Warning: The truth table is too large (", prod(x$noflevels), " rows). ",
                  "Printing it on the screen is impractical.\n         ",
                  "N.B.: You can still use its internal components (see ?str).", "\n\n", sep=""))
    }
    else {
        rownames(x$tt) <- paste(format(as.numeric(rownames(x$tt))), "")
        nofconditions <- length(x$noflevels)
        names.mydata <- colnames(x$recoded.data)[seq(nofconditions + 1)]
        cat("\n", sep="")
        
        if (!all(names(x$tt)[seq(nofconditions)] %in% names(x$recoded.data)[seq(nofconditions)])) {
            for (i in seq(nofconditions)) {
                cat("    ", paste(names(x$tt)[i], ": ", sep=""), names.mydata[i], "\n", sep="")
            }
        }
        
        inclusion <- x$tt[, "incl"]
        pri <- x$tt[, "PRI"]
        missincl <- x$tt[, "incl"] == "-"
        misspri <- x$tt[, "PRI"] == "-"
        x$tt[!missincl, "incl"] <- formatC(as.numeric(inclusion[!missincl]), digits=3, format="f")
        x$tt[!misspri, "PRI"] <- formatC(as.numeric(pri[!misspri]), digits=3, format="f")
        
        if (any(missincl)) {
            x$tt[missincl, "incl"] <- "  -"
        }
        
        if (any(misspri)) {
            x$tt[misspri, "PRI"] <- "  -"
        }
        
        cat(" OUT: outcome value\n")
        cat("   n: number of cases in configuration\n")
        cat("incl: sufficiency inclusion score\n")
        cat(" PRI: proportional reduction in inconsistency\n")
        
        print(prettyTable(x$tt))
        cat("\n")
    }
}



`print.pic` <-
function(x, ...) {
    mtrx2 <- x[[1]]
    rownames(mtrx2) <- paste(rownames(mtrx2), "")
    colnames(mtrx2) <- format(colnames(mtrx2), width=2)
    mtrx2[x[[1]]]  <- "x"
    mtrx2[!x[[1]]] <- "-"
    print(prettyTable(mtrx2))
    cat("\n")
}



`print.qca` <-                                                      
function(x, ...) {
    if (x$opts$print.truth.table) {
        print.tt(x$tt)
    }
    else {
        nofconditions <- length(x$tt$noflevels)
        if (!all(names(x$tt$tt)[seq(nofconditions)] %in% names(x$tt$recoded.data)[seq(nofconditions)]) & x$opts$use.letters) {
            cat("\n")
            names.mydata <- colnames(x$tt$recoded.data)[seq(nofconditions + 1)]
            for (i in seq(nofconditions)) {
                cat("    ", paste(names(x$tt$tt)[i], ": ", sep=""), names.mydata[i], "\n", sep="")
            }
        }
    }
    
    if (x$opts$details) {
        if (!x$opts$print.truth.table) cat("\n")
        cat("n OUT = 1/0/C:", paste(x$numbers[1:3], collapse="/"), "\n")
        cat("  Total      :", x$numbers[4], "\n")
    }
    cat("\n")
    if ("i.sol" %in% names(x)) {
        for (i in seq(length(x$i.sol))) {
            cat(paste(ifelse(i > 1, "\n", ""), "p.sol: ", sep=""))
            cat(prettyString(x$i.sol[[i]]$p.sol, getOption("width") - 7, 7, " + "))
            cat("\n")
            prettyNums <- formatC(seq(length(x$i.sol[[i]]$solution)), digits = nchar(length(x$solution)) - 1, flag = 0)
                
            if (x$opts$show.cases) {
                PIchart <- x$PIchart$i.sol[[i]][[1]]
                PIchart <- PIchart[rownames(PIchart) %in% unique(unlist(x$i.sol[[i]]$solution[[1]])), , drop=FALSE]
                mult.cov <- ifelse(any(colSums(PIchart) > 1), length(unlist(lapply(x$inputcases[colSums(PIchart) > 1], strsplit, split=","))), 0)
                cat("\nNumber of multiple-covered cases:", mult.cov, "\n")
            }
            
            cat("\n")
            
            for (sol in seq(length(x$i.sol[[i]]$solution))) {
                preamble <- paste("S", prettyNums[sol], ": ", sep="")
                preamble <- paste(preamble, paste(rep(" ", 7 - nchar(preamble)), collapse=""), sep="")
                cat(preamble)
                
                xsol <- x$i.sol[[i]]$solution[[sol]]
                if (length(x$i.sol[[i]]$essential) > 0) {
                    xsol <- xsol[!xsol %in% x$i.sol[[i]]$essential]
                    xsol <- paste(paste(sortVector(x$i.sol[[i]]$essential), collapse="#"), ifelse(length(xsol) > 0, paste("#(", paste(xsol, collapse="#"), ")", sep=""), ""), sep="")
                    cat(prettyString(unlist(strsplit(xsol, split="#")), getOption("width") - 7, 7, " + "), "\n")
                }
                else {
                    cat(prettyString(x$i.sol[[i]]$solution[[sol]], getOption("width") - 7, 7, " + "), "\n")
                }
            }
            
            if (x$opts$details) {
                print.pof(x$i.sol[[i]]$IC)
            }
            else {
                cat("\n")
            }
        }
    }
    else {
        
        if (x$opts$show.cases) {
            PIchart <- x$PIchart$c.sol[[1]]
            if (is.null(PIchart)) {
                PIchart <- x$PIchart$p.sol[[1]]
            }
            
            PIchart <- PIchart[rownames(PIchart) %in% unique(unlist(x$solution[[1]])), , drop=FALSE]
            mult.cov <- ifelse(any(colSums(PIchart) > 1), length(unlist(lapply(x$inputcases[colSums(PIchart) > 1], strsplit, split=","))), 0)
            cat("Number of multiple-covered cases:", mult.cov, "\n\n")
        }
        
        if (length(x$solution) == 1) {
            cat(paste("S1: ", prettyString(x$solution[[1]], getOption("width"), 4, " + "), "\n", sep=""))
            if (!x$opts$details) cat("\n")
        }
        else {
            prettyNums <- formatC(seq(length(x$solution)), digits = nchar(length(x$solution)) - 1, flag = 0)
            for (i in seq(length(x$solution))) {
                cat(paste("S", prettyNums[i], ": ", sep=""))
                xsol <- x$solution[[i]]
                if (length(x$essential) > 0) {
                    xsol <- xsol[!xsol %in% x$essential]
                    xsol <- paste(paste(sortVector(x$essential), collapse="#"), ifelse(length(xsol) > 0, paste("#(", paste(xsol, collapse="#"), ")", sep=""), ""), sep="")
                    cat(prettyString(unlist(strsplit(xsol, split="#")), getOption("width") - nchar(prettyNums[i]) - 3, nchar(prettyNums[i]) + 3, " + "), "\n")
                }
                else {
                    cat(prettyString(x$solution[[i]], getOption("width") - nchar(prettyNums[i]) - 3, nchar(prettyNums[i]) + 3, " + "), "\n")
                }
            }
            cat("\n")
        }
        if (x$opts$details) {
            print.pof(x$IC)
        }
    }
    
    if (x$opts$warn1conf) {
        warning.message <- paste("There is only one configuration to be explained.",
                                 "No minimization was performed.")
        cat("\nNB: ")
        cat(prettyString(unlist(strsplit(warning.message, split=" ")), getOption("width") - 4, 4, " "))
        cat("\n\n")
    }
}




`print.pof` <-
function(x, ...) {
    
    essential.PIs <- NULL
    if ("essential" %in% names(x)) {
        essential.PIs <- x$essential
    }
    essentials <- length(essential.PIs) > 0
    
    overall <- FALSE
    if ("overall" %in% names(x)) {
        overall <- TRUE
    }
    cases.column <- sol.exists <- FALSE
    max.nchar.cases <- 0
    line.length <- floor(getOption("width")*0.95)
    valid.cov.u <- TRUE
    
    if (overall) {
        incl.cov <- x$overall$incl.cov
        
        nrow.incl.cov <- nrow(incl.cov)
        nchar.nrow <- nchar(nrow.incl.cov)
        ind.len <- length(x$individual)
        
        if (essentials) {
            essential.PIs.rows <- rownames(incl.cov) %in% essential.PIs
        }
        
        if ("cases" %in% names(incl.cov)) {
            max.nchar.cases <- max(nchar(incl.cov$cases))
            cases.column <- TRUE
            incl.cov.cases <- incl.cov$cases
            incl.cov$cases <- NULL
            if (essentials) {
                incl.cov.e.cases <- incl.cov.cases[essential.PIs.rows]
                incl.cov.cases <- incl.cov.cases[!essential.PIs.rows]
            }
        }
        prettyNums <- format(seq(nrow.incl.cov))
        for (i in seq(ncol(incl.cov))) {
            NAs <- is.na(incl.cov[, i])
            incl.cov[!NAs, i] <- formatC(incl.cov[!NAs, i], digits=3, format="f")
            incl.cov[NAs, i] <- "  -  "
        }
        
        colnames(incl.cov) <- format(colnames(incl.cov))
        if (essentials) {
            which.essential <- seq(length(which(essential.PIs.rows)))
            prettyNums.e <- prettyNums[which.essential]
            prettyNums <- prettyNums[-which.essential]
            incl.cov.e <- incl.cov[essential.PIs.rows, , drop=FALSE]
            incl.cov <- incl.cov[!essential.PIs.rows, , drop=FALSE]
            for (i in seq(ind.len)) {
                unique.coverages <- formatC(x$individual[[i]]$incl.cov$cov.u[rownames(x$individual[[i]]$incl.cov) %in% essential.PIs], digits=3, format="f")
                incl.cov.e <- cbind(incl.cov.e, S=unique.coverages, stringsAsFactors=FALSE)
                x$individual[[i]]$incl.cov <- x$individual[[i]]$incl.cov[!rownames(x$individual[[i]]$incl.cov) %in% essential.PIs, ]
            }
        }
        
        for (i in seq(ind.len)) {
            incl.cov <- cbind(incl.cov, "     ", stringsAsFactors=FALSE)
            colnames(incl.cov)[ncol(incl.cov)] <- format(ifelse(ind.len < line.length, paste("(S", i, ")", sep=""), paste("S", i, sep="")), width=5)
            incl.cov[rownames(x$individual[[i]]$incl.cov), ncol(incl.cov)] <- formatC(x$individual[[i]]$incl.cov$cov.u, digits=3, format="f")
        }
        
        sol.incl.cov <- matrix(unlist(lapply(x$individual, "[", "sol.incl.cov")),
                               nrow=length(x$individual), ncol=3, byrow=TRUE)
        rownames(sol.incl.cov) <- paste("S", seq(length(x$individual)), sep="")
        sol.exists <- TRUE
        
    }
    else {
        incl.cov <- x$incl.cov
        #if (x$relation == "sufficiency") {
        #    if (sum(as.numeric(incl.cov[, "cov.u"])) == 0) {
        #        valid.cov.u <- FALSE
        #        incl.cov <- incl.cov[, -which(colnames(incl.cov) == "cov.u")]
        #    }
        #}
        
        nrow.incl.cov <- nrow(incl.cov)
        nchar.nrow <- nchar(nrow.incl.cov)
        prettyNums <- format(seq(nrow.incl.cov))
        incl.cov[incl.cov == "  NA"] <- "     "
        colnames(incl.cov) <- format(colnames(incl.cov))
        if ("cases" %in% names(incl.cov)) {
            max.nchar.cases <- max(nchar(incl.cov$cases))
            cases.column <- TRUE
            incl.cov.cases <- incl.cov$cases
            incl.cov$cases <- NULL
        }
        
        for (i in seq(ncol(incl.cov))) {
            NAs <- is.na(incl.cov[, i])
            incl.cov[!NAs, i] <- formatC(incl.cov[!NAs, i], digits=3, format="f")
            incl.cov[NAs, i] <- "  -  "
        }
        
        if ("sol.incl.cov" %in% names(x)) {
            sol.incl.cov <- t(as.matrix(x$sol.incl.cov))
            rownames(sol.incl.cov) <- "S1"
            sol.exists <- TRUE
        }
    }
    
    if (is.null(rownames(incl.cov))) {
        rownames(incl.cov) <- rep("  ", nrow(incl.cov))
    }
    
    nchar.rownames <- max(nchar(rownames(incl.cov)))
    if (nchar.rownames == 1) {
        nchar.rownames <- 2
    }
    
    if (essentials) {
        nchar.rownames <- max(nchar.rownames, max(nchar(rownames(incl.cov.e))))
        rownames(incl.cov.e) <- format(rownames(incl.cov.e), width=max(2, nchar.rownames))
    }
    
    if (x$relation == "necessity") {
        incl.cov <- incl.cov[, 1:3]
    }
    
    rownames(incl.cov) <- format(rownames(incl.cov), width=max(2, nchar.rownames))
    if (sol.exists) {
        rownames(sol.incl.cov) <- format(rownames(sol.incl.cov), width=nchar.rownames)
        sol.incl.cov <- formatC(sol.incl.cov, digits=3, format="f")
    }
    
    if (x$relation == "necessity") {
        max.chars <- line.length - 1
    }
    else {
        first.printed.row <- paste(c(rep(" ", nchar.rownames + nchar.nrow + 25), rep("-", 7*(ncol(incl.cov) - (2 + valid.cov.u)) - 2)), collapse="")
        max.chars <- nchar(first.printed.row)
    }
    other.args <- list(...)
    
    if (max.chars < line.length) {
        if (cases.column) {
             # calculate the number of characters including the cases column
            max.chars <- max.chars + max.nchar.cases
        }
        
         # then compare again the max.chars with a "normal" length of a line
        if (max.chars < line.length) {
            if (ncol(incl.cov) > 4) {
                cat(first.printed.row, "\n")
            }
            else {
                cat("\n")
            }
            
            colnames.row <- cat(paste(c(paste(rep(" ", nchar.rownames + nchar.nrow + 2), collapse=""), format(colnames(incl.cov))), collapse="  "))
            cat(paste(colnames.row, ifelse(cases.column, "  cases", ""), sep=""), "\n")
            
            sep.row <- paste(rep("-", nchar.rownames + 7*ncol(incl.cov) + ifelse(cases.column, max.nchar.cases + 2, 0) + nchar.nrow + 2), collapse="")
            cat(sep.row, "\n")
            if (essentials) {
                for (i in seq(nrow(incl.cov.e))) {
                    i.row <- paste(prettyNums.e[i], paste(c(rownames(incl.cov.e)[i], incl.cov.e[i, ]), collapse="  "), sep="  ")
                    if (cases.column) {
                        i.row <- paste(i.row, incl.cov.e.cases[i], sep="  ")
                    }
                    cat(i.row, "\n")
                }
                cat(sep.row, "\n")
            }
            
            for (i in seq(nrow(incl.cov))) {
                i.row <- paste(prettyNums[i], paste(c(rownames(incl.cov)[i], incl.cov[i, ]), collapse="  "), sep="  ")
                if (cases.column) {
                    i.row <- paste(i.row, incl.cov.cases[i], sep="  ")
                }
                cat(i.row, "\n")
            }
            cat(sep.row, "\n")
            
            if (sol.exists) {
                for (i in seq(nrow(sol.incl.cov))) {
                    cat(paste(paste(rep(" ", nchar.nrow), collapse=""), paste(c(rownames(sol.incl.cov)[i], sol.incl.cov[i, ]), collapse="  "), sep="  "), "\n")
                }
            }
            cat("\n")
        }
        else {
             # the number of characters including the cases exceeds a normal length line
             # therefore the cases will be printed separately
            if (ncol(incl.cov) > 4) {
                cat(first.printed.row, "\n")
            }
            else {
                cat("\n")
            }
            
            cat(paste(c(paste(rep(" ", nchar.rownames + nchar.nrow + 2), collapse=""), colnames(incl.cov)), collapse="  "), "\n")
            sep.row <- paste(rep("-", nchar.rownames + 7*ncol(incl.cov) + nchar.nrow + 2), collapse="")
            cat(sep.row, "\n")
            
            if (essentials) {
                for (i in seq(nrow(incl.cov.e))) {
                    cat(paste(prettyNums.e[i], paste(c(rownames(incl.cov.e)[i], incl.cov.e[i, ]), collapse="  "), "\n"), sep="  ")
                }
                cat(sep.row, "\n")
            }
            
            for (i in seq(nrow(incl.cov))) {
                cat(paste(prettyNums[i], paste(c(rownames(incl.cov)[i], incl.cov[i, ]), collapse="  "), sep="  "), "\n")
            }
            cat(sep.row, "\n")
            
            if (sol.exists) {
                for (i in seq(nrow(sol.incl.cov))) {
                    cat(paste(paste(rep(" ", nchar.nrow), collapse = ""), paste(c(rownames(sol.incl.cov)[i], sol.incl.cov[i, ]), collapse="  "), sep="  "), "\n")
                }
            }
            
            cat("\n", paste(paste(rep(" ", nchar.rownames + nchar.nrow + 2), collapse=""), "cases"), "\n")
            cat(paste(rep("-", nchar.rownames + 7 + nchar.nrow + 2), collapse=""), "\n")
            if (essentials) {
                for (i in seq(nrow(incl.cov.e))) {
                    cat(paste(prettyNums.e[i], paste(rownames(incl.cov.e)[i], " "), sep="  "))
                    cases <- unlist(strsplit(incl.cov.e.cases[i], split="; "))
                    cat(prettyString(cases, getOption("width") - nchar.rownames - nchar.nrow - 4, nchar.rownames + nchar.nrow + 4, "; "))
                    cat("\n")
                }
                cat(paste(rep("-", nchar.rownames + nchar.nrow + 9), collapse=""), "\n")
            }
            for (i in seq(nrow(incl.cov))) {
                cat(paste(prettyNums[i], paste(rownames(incl.cov)[i], " "), sep="  "))
                cases <- unlist(strsplit(incl.cov.cases[i], split="; "))
                cat(prettyString(cases, getOption("width") - nchar.rownames - - nchar.nrow - 4, nchar.rownames + nchar.nrow + 4, "; "))
                cat("\n")
            }
            cat(paste(rep("-", nchar.rownames + nchar.nrow + 9), collapse=""), "\n\n")
        }
    }
    else {
        
         # the number of characters from all columns exceed a normal length line
         # therefore the entire matrix will be printed on chunks of columns
        ncols <- floor((line.length - nchar.rownames)/7)
        chunks <- ceiling(ncol(incl.cov)/ncols)
        colsplits <- seq(1, ncol(incl.cov), by=ncols)
        
        if (essentials) {
            incl.cov.e <- incl.cov.e[, seq(1, ncols)]
        }
        
        for (chunk in seq(chunks)) {
            incl.cov.temp <- incl.cov[, seq(colsplits[chunk], ifelse(chunk == chunks, ncol(incl.cov), colsplits[chunk + 1] - 1))]
            
            if (chunk < chunks) {
                cat(paste(c("\n", rep(ifelse(chunk == 1, " ", "-"), nchar.rownames + nchar.nrow + 18), rep("-", 7*(ncols - 2) - 2)), collapse=""), "\n")
                cat(paste(c(paste(rep(" ", nchar.rownames + nchar.nrow + 2), collapse=""), colnames(incl.cov.temp)), collapse="  "), "\n")
                sep.row <- paste(rep("-", nchar.rownames + 7*ncol(incl.cov.temp)  + nchar.nrow + 2), collapse="")
                cat(sep.row, "\n")
                
                if (chunk == 1 & essentials) {
                    for (i in seq(nrow(incl.cov.e))) {
                        cat(paste(prettyNums.e[i], paste(c(rownames(incl.cov.e)[i], incl.cov.e[i, ]), collapse="  "), sep="  "), "\n")
                    }
                    cat(sep.row, "\n")
                }
                
                for (i in seq(nrow(incl.cov.temp))) {
                    cat(paste(prettyNums[i], paste(c(rownames(incl.cov.temp)[i], incl.cov.temp[i, ]), collapse="  "), sep="  "), "\n")
                }
                
                cat(sep.row, "\n")
                
                if (chunk == 1 & sol.exists) {
                    for (i in seq(nrow(sol.incl.cov))) {
                        cat(paste(paste(rep(" ", nchar.nrow), collapse = ""), paste(c(rownames(sol.incl.cov)[i], sol.incl.cov[i, ]), collapse="  "), sep="  "), "\n")
                    }
                }
                cat("\n")
            }
            else {
                max.chars <- nchar.rownames + 7*ncol(incl.cov.temp)
                sep.row <- paste(c(rep("-", max.chars)), collapse="")
                if (cases.column) {
                    max.chars <- max.chars + max.nchar.cases
                }
                
                if (max.chars < line.length) {
                    cat(sep.row, "\n")
                    
                    sep.row <- paste(sep.row, ifelse(cases.column, paste(rep("-", max.nchar.cases + 2), collapse=""), ""), sep="")
                    
                    colnames.row <- paste(c(paste(rep(" ", nchar.rownames + nchar.nrow + 2), collapse=""), colnames(incl.cov.temp)), collapse="  ")
                    cat(paste(colnames.row, ifelse(cases.column, "  cases", ""), sep=""), "\n")
                    
                    cat(sep.row, "\n")
                    
                    for (i in seq(nrow(incl.cov.temp))) {
                        i.row <- paste(prettyNums[i], paste(c(rownames(incl.cov.temp)[i], incl.cov.temp[i, ]), collapse="  "), sep="  ")
                            if (cases.column) {
                                i.row <- paste(i.row, incl.cov.cases[i], sep="  ")
                            }
                        cat(i.row, "\n")
                    }
                    
                    cat(sep.row, "\n")
                }
                else {
                    cat(sep.row, "\n")
                    
                    cat(paste(c(paste(rep(" ", nchar.rownames + nchar.nrow + 2), collapse=""), colnames(incl.cov.temp)), collapse="  "), "\n")
                    
                    cat(sep.row, "\n")
                    
                    for (i in seq(nrow(incl.cov.temp))) {
                        cat(paste(prettyNums[i], paste(c(rownames(incl.cov.temp)[i], incl.cov.temp[i, ]), collapse="  "), sep="  "), "\n")
                    }
                    
                    cat(sep.row, "\n")
                    cat("\n", paste(paste(rep(" ", nchar.rownames + nchar.nrow + 2), collapse=""), "cases"), "\n")
                    cat(paste(rep("-", nchar.rownames + nchar.nrow + 9), collapse=""), "\n")
                    
                    for (i in seq(nrow(incl.cov.temp))) {
                        cat(paste(prettyNums[i], paste(rownames(incl.cov.temp)[i], " "), sep="  "))
                        cases <- unlist(strsplit(incl.cov.cases[i], split="; "))
                        cat(prettyString(cases, getOption("width") - nchar.rownames - 2, nchar.rownames + 2, "; "))
                        cat("\n")
                    }
                    cat(paste(rep("-", nchar.rownames + nchar.nrow + 9), collapse=""), "\n")
                }
                cat("\n")
            }
        }
        
    }
    
    
}



`print.ss` <-
function(x, ...) {
    
    if (x$use.letters) {
        conditions <- names(x$letters)
        xletters <- as.vector(x$letters)
        if (!all(conditions %in% xletters)) {
            cat("\n")
            for (i in seq(length(xletters))) {
                cat("    ", paste(xletters[i], ": ", sep=""), conditions[i], "\n", sep="")
            }
        }
    }
    
    
    
    incl.cov <- x$incl.cov
    cat("\n")
    prettyNums <- format(seq(nrow(incl.cov)))
    rownames(incl.cov) <- format(rownames(incl.cov))
    colnames(incl.cov) <- format(colnames(incl.cov), width=5)
    for (i in seq(ncol(incl.cov))) {
        NAs <- is.na(incl.cov[, i])
        incl.cov[!NAs, i] <- formatC(incl.cov[!NAs, i], digits=3, format="f")
        incl.cov[NAs, i] <- "  -  "
    }
    
    nchar.rownames <- nchar(rownames(incl.cov)[1])
    cat(paste(c(paste(rep(" ", nchar.rownames + nchar(nrow(incl.cov)) + 2), collapse=""), format(colnames(incl.cov))), collapse="  "), "\n")
    sep.row <- paste(rep("-", nchar.rownames + nchar(nrow(incl.cov)) + 23), collapse="")
    cat(sep.row, "\n")
    
    for (i in seq(nrow(incl.cov))) {
        cat(paste(prettyNums[i], paste(c(rownames(incl.cov)[i], incl.cov[i, ]), collapse="  "), sep="  "), "\n")
    }
    cat(sep.row, "\n")
    cat("\n")
}



`print.fctr` <-
function(x, ...) {
    xprint <- function(fx) {
        for (i in seq(length(fx))) {
            cat("S: ")
            preamblength <- nchar(prettyNumsSol[i]) + 3
            cat(prettyString(names(fx)[i], getOption("width") - 3, 3, " + "), "\n\n")
            
            if (!is.null(fx[[i]])) {
                for (j in seq(length(fx[[i]]))) {
                    prettyNumsFact <- formatC(seq(length(fx[[i]])), digits = nchar(length(fx[[i]])) - 1, flag = 0)
                    cat(paste("F", prettyNumsFact[j], ": ", sep=""))
                    flength <- nchar(prettyNumsFact[j]) + 3
                    strvctr <- unlist(strsplit(fx[[i]][[j]], split=" + "))
                    cat(prettyString(strvctr, getOption("width") - flength, flength, " + "), "\n")
                }
                cat("\n")
            }
            else {
                cat("No factorization possible.\n")
            }
            cat("\n")
        }
    }
    
    prettyNumsSol <- formatC(seq(length(x)), digits = nchar(length(x)) - 1, flag = 0)
    cat("\n")
    if (names(x)[1] == "i.sol") {
        for (isol in seq(length(x$i.sol))) {
            cat(paste("    p.sol:", names(x$i.sol)[isol]), "\n\n")
            for (xf in x$i.sol) {
                xprint(xf)
            }
        }
    }
    else {
        xprint(x)
    }
    
}




`print.aE` <-
function(x, ...) {
    aE <- x$aE
    rownames(aE) <- format(seq.nrow <- seq(nrow(aE)))
    if (x$raw) {
        aE[aE >= 0] <- paste("", aE[aE >= 0])
    }
    else {
        aE[aE < 0] <- " "
    }
    
    cat("\n")
    for (i in seq.nrow) {
        cat(paste(c(rownames(aE)[i], aE[i, ]), collapse=ifelse(x$raw,"   ", "    ")), "\n")
    }
    cat("\n")
}










