`eqmcc` <-
function(mydata, outcome = "", neg.out = FALSE, conditions = c(""), n.cut = 1,
         incl.cut1 = 1, incl.cut0 = 1, explain = "1", include = c(""), omit = c(),
         direxp = c(), rowdom = TRUE, details = FALSE, show.cases = FALSE,
         use.tilde = FALSE, use.letters = FALSE) {
    
    print.truth.table <- details & !is.tt(mydata)
    if (all(include == "")) {
        include <- explain
    }
    
    if (!is.tt(mydata)) {
        if (all(conditions == c(""))) {
            conditions <- names(mydata)[-which(names(mydata)==outcome)]
        }
        
        mydata <- mydata[, c(conditions, outcome)]
        verify.qca(mydata, outcome, conditions, explain, include, use.letters, direxp)
        
        indata <- mydata
        tt <- truthTable(mydata, outcome, conditions, show.cases=show.cases, n.cut=n.cut,
                         incl.cut1=incl.cut1, incl.cut0=incl.cut0, use.letters=use.letters, neg.out=neg.out)
        
        recdata <- tt$recoded.data
        conditions <- toupper(conditions)
        outcome <- toupper(outcome)
    }
    else {
        chexplain <- c(0, 1)[which(0:1 %in% explain)]
        chinclude <- c(0, 1)[which(0:1 %in% include)]
        if (length(chinclude) > 0) {
            if (any(chinclude != chexplain)) {
                chinclude <- chinclude[which(chinclude != chexplain)]
                cat("\n")
                stop(paste("You cannot include ", chinclude, " since you want to explain ", chexplain, ".\n\n", sep=""), call. = FALSE)
            }
        }
        tt <- mydata
        indata <- tt$initial.data
        recdata <- tt$recoded.data
        conditions <- names(recdata)[seq(length(tt$noflevels))]
        outcome <- names(recdata)[ncol(recdata)]
        if (any(tt$tt$OUT == "?")) {
            missings <- which(tt$tt$OUT == "?")
            tt$tt <- tt$tt[-missings, ]
        }
        if (!is.null(direxp)) {
            if (length(direxp) != length(conditions)) {
                cat("\n")
                stop("Number of expectations does not match number of conditions.\n\n", call. = FALSE)
            }
        }
        neg.out <- tt$neg.out
    }
    
    getSolution <- function() {
        
        PI <- writePrimeimp(expressions, collapse=collapse, uplow=uplow, use.tilde=use.tilde)
        rownames(expressions) <- PI
        PI.sort <- sortVector(PI, collapse=collapse)
        
        expressions <- expressions[match(sortVector(PI.sort, collapse=collapse), PI), , drop=FALSE]
        
         # create the prime implicants chart
        mtrx <- createChart(expressions, inputt)
        reduced <- list(expressions = expressions, mtrx = mtrx)
        
        if (rowdom) {
            reduced <- rowDominance2(mtrx, expressions)
        }
        
        PI.red <- writePrimeimp(reduced$expressions, collapse=collapse, uplow=uplow, use.tilde=use.tilde)
        PI.red.sort <- sortVector(PI.red, collapse=collapse)
        
        mtrx <- reduced$mtrx[match(PI.red.sort, PI.red), , drop=FALSE]
        
        rownames(mtrx) <- PI.red.sort
        colnames(mtrx) <- initial
        
        
        
        sol.matrix <- solveChart(mtrx)
        
        k <- sol.matrix[[1]]
        sol.matrix <- matrix(rownames(mtrx)[sol.matrix[[2]]], nrow=k)
        
        all.PIs <- sortVector(unique(as.vector(sol.matrix)), collapse=collapse)
        # mtrx <- mtrx[rownames(mtrx) %in% all.PIs, , drop=FALSE]
        reduced$expressions <- reduced$expressions[sortVector(rownames(reduced$expressions)[rownames(reduced$expressions) %in% all.PIs], collapse=collapse), , drop=FALSE]
        
        all.PIs <- all.PIs[all.PIs %in% rownames(mtrx)]
        
        solution.list <- writeSolution(sol.matrix, mtrx)
        
        return(list(k=k, mtrx=mtrx, reduced=reduced, all.PIs=all.PIs, solution.list=solution.list))
    }   
    
    
    val.outcome <- indata[, outcome]
    sum.outcome <- sum(val.outcome)
    uplow <- TRUE
    noflevels <- tt$noflevels
    
     # check if the column names are not already letters
    alreadyletters <- sum(nchar(colnames(recdata)[-ncol(recdata)])) == ncol(recdata) - 1
    
    output <- list()
    output$tt <- tt
    output$opts$print.truth.table <- print.truth.table
    
    tt$tt[, seq(length(conditions))] <- as.data.frame(lapply(tt$tt[, seq(length(conditions))], function(x) {
        x[x %in% c("-", "dc")] <- -1
        return(as.numeric(x))
    }))
    
    expl.incl <- unique(c(explain, include))
    subset.tt <- tt$tt[, "OUT"] %in% expl.incl
    expl.matrix <- as.matrix(tt$tt[subset.tt, seq(length(noflevels))])
    expl.matrix <- matrix(as.numeric(expl.matrix), ncol=length(noflevels)) + 1
    rownames(expl.matrix) <- tt$indexes[subset.tt]
    
    subset.tt <- !tt$tt[, "OUT"] %in% expl.incl
    excl.matrix <- as.matrix(tt$tt[subset.tt, seq(length(noflevels))])
    excl.matrix <- matrix(as.numeric(excl.matrix), ncol=length(noflevels)) + 1
    
    subset.tt <- tt$tt[, "OUT"] %in% explain
    inputt <- as.matrix(tt$tt[subset.tt, seq(length(noflevels))])
    rownms <- rownames(inputt)
    inputt <- matrix(as.numeric(inputt), ncol=length(noflevels)) + 1
    inputcases <- tt$cases[tt$cases != ""][subset.tt]
    
    nofcases1 <- sum(tt$tt$n[tt$tt$OUT == 1])
    nofcases0 <- sum(tt$tt$n[tt$tt$OUT == 0])
    nofcasesC <- sum(tt$tt$n[tt$tt$OUT == "C"])
    
    tomit <- logical(nrow(expl.matrix))
    tomitinputt <- logical(nrow(inputt))
    if (is.matrix(omit)) {
        cnoflevels <- noflevels
        for (i in seq(ncol(omit))) {
            if (any(omit[, i] < 0)) {
                omit[, i][omit[, i] < 0] <- noflevels[i]
                cnoflevels[i] <- noflevels[i] + 1
            }
        }
        omitrows <- drop(rev(c(1, cumprod(rev(cnoflevels))))[-1] %*% t(omit)) + 1
        tomit <- rownames(expl.matrix) %in% omitrows
        tomitinputt <- rownms %in% omitrows
        excl.matrix <- rbind(excl.matrix, omit + 1)
    }
    else if (is.vector(omit)) {
        tomit <- rownames(expl.matrix) %in% omit
        tomitinputt <- rownms %in% omit
        excl.matrix <- unique(rbind(excl.matrix, getRow(noflevels, as.numeric(omit)) + 1))
    }
    
    output$excluded <- sort(drop(rev(c(1, cumprod(rev(noflevels))))[-1] %*% t(excl.matrix - 1)) + 1)
    expl.matrix <- expl.matrix[!tomit, , drop=FALSE]
    inputt <- inputt[!tomitinputt, , drop=FALSE]
    inputcases <- inputcases[!tomitinputt]
    
    if (nrow(expl.matrix) == 0) {
        cat("\n")
        stop("Nothing to explain. Please check the truth table.\n\n", call. = FALSE)
    }
    
    incl.rem <- "?" %in% include
    if (nrow(excl.matrix) == 0 & incl.rem) {
        cat("\n")
        stop(paste("All combinations have been included into analysis. The solution is 1.\n",
                   "Please check the truth table.", "\n\n", sep=""), call. = FALSE)
    }
    
     # expl.matrix needs to be unaltered for the incl.rem argument
    expressions <- expl.matrix
    
    recdata[, conditions] <- as.data.frame(lapply(recdata[, conditions], function(x) {
        x[x %in% c("-", "?", "dc")] <- -1
        return(as.numeric(x))
    }))
    
     # check if the data has multiple values
    if (any(recdata[, seq(ncol(recdata) - 1)] > 1)) {
        uplow <- FALSE
        use.tilde <- FALSE
    }
    
    if (use.tilde) {
        uplow <- FALSE
    }
    
    collapse <- ifelse(alreadyletters & uplow | use.tilde, "", "*")
    changed <- FALSE
    
    
     # if not already letters and user specifies using letters for conditions, change it
    if (use.letters & !alreadyletters) {
        colnames(expressions) <- colnames(inputt) <- colnames(expl.matrix) <- LETTERS[seq(ncol(inputt))]
        changed <- TRUE
        collapse <- ifelse(!uplow | use.tilde, "*", "")
    }
    else {
        colnames(expressions) <- colnames(inputt) <- colnames(expl.matrix) <- colnames(recdata[, seq(ncol(inputt))])
        if (use.tilde) {
            collapse <- "*"
        }
    }
    
    output$initials <- writePrimeimp(inputt, collapse=collapse, uplow=uplow, use.tilde=use.tilde)
    initial <- drop(rev(c(1, cumprod(rev(noflevels))))[-1] %*% t(inputt - 1)) + 1
    
    minExpressions <- function(expressions) {
        minimized <- TRUE
        while (any(minimized)) {
            minimized <- logical(nrow(expressions))
            distance <- dist(expressions, method="manhattan")
            distance <- as.matrix(distance)
            distance[!upper.tri(distance)] <- NA
             # tbc means "to be compared"
            tbc <- as.matrix(which(distance == 1, arr.ind=TRUE))
            
            if (nrow(tbc) > 0) {
                differences <- t(apply(tbc, 1, function(idx) expressions[idx[1], ] != expressions[idx[2], ]))
                result <- matrix(0, nrow=0, ncol=ncol(differences))
                for (i in seq.int(nrow(differences))) {
                    stable.values <- expressions[tbc[i, 1], !differences[i, , drop=FALSE], drop=FALSE]
                    subset.explain <- apply(expressions[, !differences[i, , drop=FALSE], drop=FALSE], 1, function(x) all(x == stable.values))
                    if (sum(subset.explain) == noflevels[differences[i, ]]) {
                        minimized[subset.explain] <- TRUE
                        minimization.result <- expressions[tbc[i, 1], , drop=FALSE]
                        minimization.result[differences[i, ]] <- 0
                        result <- rbind(result, as.vector(minimization.result))
                    }
                }
            }
            
            if (sum(minimized) > 0) {
                expressions <- rbind(expressions[!minimized, ], unique(result))
            }
        }
        return(expressions)
    }
    
    expressions <- minExpressions(expressions)
    
    c.sol <- p.sol <- getSolution()
    
    
    if (incl.rem) {
        expressions <- sort(setdiff(findSupersets(noflevels + 1, expl.matrix), findSupersets(noflevels + 1, excl.matrix)))
        index <- 0
        while ((index <- index + 1) < length(expressions)) {
            expressions <- setdiff(expressions, findSubsets(noflevels + 1, expressions[index], max(expressions)))
        }
        expressions <- getRow(noflevels + 1, expressions)
        colnames(expressions) <- colnames(inputt)
        p.sol <- getSolution()
    }
    
    output$PIs <- p.sol$all.PIs
    if (incl.rem) {
        output$PIchart$p.sol <- structure(list(p.sol$mtrx), class="pic")
    }
    else {
        output$PIchart$c.sol <- structure(list(p.sol$mtrx), class="pic")
    }
    output$primes <- p.sol$expressions
    output$solution <- p.sol$solution.list[[1]]
    output$essential <- p.sol$solution.list[[2]]
    
    if (details) {
        
        expr.cases <- rep(NA, nrow(p.sol$reduced$expressions))
        if (show.cases) {
            
            tt.rows <- createString(inputt - 1, collapse=collapse, uplow, use.tilde)
            
            mtrxlines <- demoChart(rownames(p.sol$reduced$expressions), tt.rows, ifelse((use.letters & uplow) | (alreadyletters & uplow), "", "*"))
            for (l in seq(length(expr.cases))) {
                expr.cases[l] <- paste(inputcases[mtrxlines[l, ]], collapse="; ")
            }
        }
        
        if (length(p.sol$solution.list[[1]]) == 1) {
            listIC <- pof(p.sol$reduced$expressions - 1, indata, outcome, showc=show.cases, cases=expr.cases, relation = "sufficiency", neg.out=neg.out, via.eqmcc=TRUE)
        }
        else {
            listIC <- pof(p.sol$reduced$expressions - 1, indata, outcome, showc=show.cases, cases=expr.cases, neg.out=neg.out,
                            solution.list=output$solution, essential=output$essential, relation = "sufficiency", via.eqmcc=TRUE)
        }
        
        if (incl.rem) {
            output$pims$p.sol <- listIC$pims
        }
        else {
            output$pims$c.sol <- listIC$pims
        }
        
        listIC$pims <- NULL
        output$IC <- listIC
    }
    
    output$numbers <- c(OUT1=nofcases1, OUT0=nofcases0, OUTC=nofcasesC, Total=nofcases1 + nofcases0 + nofcasesC)
    
    output$opts$warn1conf <- ifelse(nrow(expl.matrix) == 1 & !incl.rem, TRUE, FALSE)
    mtrx <- p.sol$mtrx[p.sol$all.PIs, , drop=FALSE]
    
    output$inputcases <- inputcases
    output$opts$details <- details
    output$opts$show.cases <- show.cases
    output$opts$use.letters <- use.letters
    output$opts$collapse <- collapse
    mbase <- rev(c(1, cumprod(rev(noflevels + 1))))[-1]
    output$SA <- lapply(p.sol$solution.list[[1]], function(x) {
        p.expressions <- p.sol$reduced$expressions[x, , drop=FALSE]
        
        temp <- apply(p.expressions, 1, function(pr) {
            indices <- rev(which(!pr))
            
            SA <- NULL
            for (k in indices) {
                if (is.null(SA)) {
                    SA <- drop(mbase %*% pr) + sum(mbase[!pr])
                }
                tempSA <- SA
                for (lev in seq(noflevels[k] - 1)) {
                    tempSA <- c(tempSA, SA + mbase[k]*lev)
                }
                SA <- tempSA
            }
            return(SA)
        })
        
        if (all(is.null(temp))) {
            return(NULL)
        }
        else {
            temp <- sort(unique(as.vector(unlist(temp))))
            temp <- temp[!temp %in% drop(mbase %*% t(inputt))]
            if (length(temp) > 0) {
                SA <- getRow(noflevels + 1,  temp + 1) - 1
                colnames(SA) <- colnames(inputt)
                rownames(SA) <- drop(c(rev(cumprod(rev(noflevels))), 1)[-1] %*% t(SA)) + 1
                return(SA)
            }
            else {
                return(NULL)
            }
        }
    })
    prettyNums <- formatC(seq(length(p.sol$solution.list[[1]])), digits = nchar(length(p.sol$solution.list[[1]])) - 1, flag = 0)
    names(output$SA) <- paste("S", prettyNums, sep="")
    
    
    if (all(!is.null(direxp)) & all(include != c(""))) {
        i.sol <- output$pims$i.sol <- vector("list", length(c.sol$solution.list[[1]])*length(p.sol$solution.list[[1]]))
        index <- 1
        for (c.s in seq(length(c.sol$solution.list[[1]]))) {
            c.expressions <- c.sol$reduced$expressions[c.sol$solution.list[[1]][[c.s]], , drop=FALSE]
            
            for (p.s in seq(length(p.sol$solution.list[[1]]))) {
                
                constraint <- p.sol$reduced$expressions[p.sol$solution.list[[1]][[p.s]], , drop=FALSE]
                
                names(i.sol)[index] <- names(output$pims$i.sol)[index] <- paste("C", c.s, "P", p.s, sep="")
                
                cntfs <- subcols <- sexpr <- matrix(ncol=length(conditions), nrow=0)
                colnames(cntfs) <- colnames(inputt)
                direxp <- gsub("dc", -1, direxp)
                for (cons in seq(nrow(constraint))) {
                    for (i in seq(nrow(c.expressions))) {
                        subset.columns <- logical(length(direxp))
                        cons.index <- constraint[cons, ]
                        check.cons <- c.expressions[i, cons.index >= 1] == cons.index[cons.index >= 1]
                        
                        if (all(check.cons)) {
                            subset.columns[cons.index >= 1] <- check.cons
                            
                            if (any(cons.index < 1)) {
                                for (drxp in seq(length(direxp))[cons.index < 1]) {
                                    subset.columns[drxp] <- subset.columns[drxp] | ((c.expressions[i, drxp] - 1) %in% unlist(strsplit(direxp[drxp], ";")))
                                    subset.columns[direxp == -1] <- TRUE
                                }
                            }
                            
                            if (any(subset.columns)) {
                                cexpr.sub <- c.expressions[i, subset.columns] - 1
                                SArows <- apply(output$SA[[p.s]], 1, function(x) {
                                    x <- x[subset.columns]
                                    all(x[cexpr.sub >= 0] == cexpr.sub[cexpr.sub >= 0])
                                })
                                subSA <- output$SA[[p.s]][SArows, , drop=FALSE]
                                cntfs <- rbind(cntfs, subSA[!rownames(subSA) %in% rownames(cntfs), , drop=FALSE])
                            }
                        }
                    }
                }
                
                i.sol[[index]]$cntfs <- cntfs[order(as.numeric(rownames(cntfs))), ]
                
                expl.matrix.i.sol <- unique(rbind(expl.matrix, i.sol[[index]]$cntfs + 1))
                
                tomit <- logical(nrow(expl.matrix.i.sol))
                if (is.matrix(omit)) {
                    cnoflevels <- noflevels
                    for (i in seq(ncol(omit))) {
                        if (any(omit[, i] < 0)) {
                            omit[, i][omit[, i] < 0] <- noflevels[i]
                            cnoflevels[i] <- noflevels[i] + 1
                        }
                    }
                    tomit <- rownames(expl.matrix) %in% (drop(rev(c(1, cumprod(rev(cnoflevels))))[-1] %*% t(omit)) + 1)
                }
                else if (is.vector(omit)) {
                    if (is.numeric(omit)) {
                        tomit <- rownames(expl.matrix) %in% as.character(omit)
                    }
                }
                
                expl.matrix.i.sol <- expl.matrix.i.sol[!tomit, , drop=FALSE]
                
                expressions <- minExpressions(expl.matrix.i.sol)
                
                i.sol.index <- getSolution()
                output$PIchart$i.sol[[names(i.sol)[index]]] <- structure(list(i.sol.index$mtrx), class="pic")
                i.sol[[index]]$c.sol <- c.sol$solution.list[[1]][[c.s]]
                i.sol[[index]]$p.sol <- p.sol$solution.list[[1]][[p.s]]
                i.sol[[index]]$solution <- i.sol.index$solution.list[[1]]
                i.sol[[index]]$essential <- i.sol.index$solution.list[[2]]
                
                expr.cases <- rep(NA, nrow(i.sol.index$reduced$expressions))
                if (show.cases) {
                    
                    tt.rows <- createString(inputt - 1, collapse=collapse, uplow, use.tilde)
                    
                    mtrxlines <- demoChart(rownames(i.sol.index$reduced$expressions), tt.rows, ifelse((use.letters & uplow) | (alreadyletters & uplow), "", "*"))
                    for (l in seq(length(expr.cases))) {
                        expr.cases[l] <- paste(inputcases[mtrxlines[l, ]], collapse="; ")
                    }
                }
                
                if (length(i.sol.index$solution.list[[1]]) == 1) {
                    i.sol[[index]]$IC <- pof(i.sol.index$reduced$expressions - 1, indata, outcome, showc=show.cases, cases=expr.cases, relation = "sufficiency", neg.out=neg.out, via.eqmcc = TRUE)
                }
                else {
                    i.sol[[index]]$IC <- pof(i.sol.index$reduced$expressions - 1, indata, outcome, showc=show.cases, cases=expr.cases, relation = "sufficiency",
                                                  solution.list=i.sol.index$solution.list[[1]], essential=i.sol.index$solution.list[[2]], neg.out=neg.out, via.eqmcc = TRUE)
                }
                output$pims$i.sol[[index]] <- i.sol[[index]]$IC$pims
                i.sol[[index]]$IC$pims <- NULL
                index <- index + 1
            }
        }
        output$i.sol <- i.sol
    }
    return(structure(output, class="qca"))
}
