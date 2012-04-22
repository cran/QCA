`superSubset` <-
function(mydata, outcome = "", neg.out = FALSE, conditions = c(""), relation = "necessity",
         incl.cut = 1, cov.cut = 0, use.tilde = FALSE, use.letters = FALSE) {
    
    if (all(conditions == c(""))) {
        conditions <- names(mydata)[-which(names(mydata) == outcome)]
    }
    
    verify.data(mydata, outcome, conditions)
    
    if (!relation %in% c("necessity", "sufficiency")) {
        stop("\nThe relationship should be either \"necessity\" or \"sufficiency\".\n\n", call. = FALSE)
    }
    
    colnames(mydata) <- toupper(colnames(mydata))
    conditions <- replacements <- toupper(conditions)
    outcome <- toupper(outcome)
    
    mydata <- mydata[, c(conditions, outcome)]
    nofconditions <- length(conditions)
    
    if (neg.out) {
        mydata[, outcome] <- 1 - mydata[, outcome]
    }
    
    uplow <- !use.tilde
    
    fuzzy.cc <- apply(mydata[, conditions], 2, function(x) any(x %% 1 > 0))
    if (mv.data <- any(mydata[, conditions] > 1)) {
        uplow <- use.tilde <- FALSE
    }
    
    alreadyletters <- sum(nchar(conditions)) == length(conditions)
    
    collapse <- ifelse(alreadyletters & uplow | use.tilde, "", "*")
    
    if (use.letters & !alreadyletters) {
        replacements <- LETTERS[seq(length(conditions))]
        names(replacements) <- conditions
        colnames(mydata)[seq(length(conditions))] <- conditions <- replacements
        collapse <- ifelse(!uplow | use.tilde, "*", "")
    }
    
            
    check.equal <- function(x, y) {
        check.vector <- as.logical(unlist(lapply(x, all.equal, y)))
        check.vector[is.na(check.vector)] <- FALSE
        return(check.vector)
    }
    
    noflevels <- apply(mydata[, conditions], 2, max) + 1
    noflevels[fuzzy.cc] <- 2
    
    nk <- createMatrix(noflevels + 1)
    colnames(nk) <- conditions
    nk <- nk[-1, ] # first row is always empty
    
    minmat <- maxmat <- matrix(NA, nrow=nrow(mydata), ncol=nrow(nk))
    rownames(minmat) <- rownames(maxmat) <- rownames(mydata)
    
    
    for (i in seq(nrow(mydata))) {
        row.i <- as.numeric(mydata[i, conditions])
        minmax <- apply(nk, 1, function(x, values=row.i) {
            if (any(onex3k <- x[fuzzy.cc] == 1)) {
                values[fuzzy.cc][onex3k] <- 1 - values[fuzzy.cc][onex3k]
            }
            copy.values <- values[!fuzzy.cc] + 1 # move it into the nk space
            if (length(copy.values) > 0) {
                values[!fuzzy.cc][x[!fuzzy.cc] != copy.values] <- 0
                values[!fuzzy.cc][x[!fuzzy.cc] == copy.values] <- 1
            }
            return(range(values[x != 0]))
        })
        minmat[i, ] <- minmax[1, ]
        maxmat[i, ] <- minmax[2, ]
    }
    
    expressions <- colnames(minmat) <- colnames(maxmat) <- rownames(nk) <- seq_len(nrow(nk)) + 1 # plus 1 because first row of the nk matrix was deleted
    
    sum.outcome <- sum(mydata[, outcome])
    val.outcome <- mydata[, outcome]
    incov <- apply(minmat, 2, function(x) sum(pmin(x, val.outcome))/c(sum(x), sum.outcome))
    pri <- apply(minmat, 2, function(x) {
        prisum <- sum(pmin(x, val.outcome, 1 - val.outcome))
        if (relation == "necessity") {
            prisum <- sum(pmin(x, val.outcome, 1 - x))
        }
        (sum(pmin(x, val.outcome)) - prisum)/(ifelse(relation == "necessity", sum.outcome, sum(x)) - prisum)
    })
    
    na.minscores <- is.na(incov[1, ])
    incov <- incov[, !na.minscores]
    expressions <- expressions[!na.minscores]
    pri <- pri[!na.minscores]
    
    if (relation == "sufficiency") {
        incl <- incov[1, ]
        cov.r  <- incov[2, ]
    }
    else {
        incl <- incov[2, ]
        cov.r  <- incov[1, ]
    }
    
    expressions <- expressions[(incl > incl.cut | check.equal(incl, incl.cut)) & (cov.r > cov.cut | check.equal(cov.r, cov.cut))]  
    
    if (length(expressions) > 0) {
        index <- 0
        if (relation == "sufficiency") {
            while ((index <- index + 1) < length(expressions)) {
                expressions <- setdiff(expressions, findSubsets(noflevels + 1, expressions[index], max(expressions)))
            }
        }
        
        
        if (length(expressions) == 0) {
            stop("\nThere are no groupings that match given criteria.\n\n", call. = FALSE)
        }
        
        result.matrix <- getRow(noflevels + 1, expressions)
        rownames(result.matrix) <- expressions
        colnames(result.matrix) <- conditions
        result.matrix <- sortMatrix(result.matrix)
        sum.zeros <- apply(result.matrix, 1, function(idx) sum(idx == 0))
        result.matrix <- result.matrix[order(sum.zeros, decreasing=TRUE), , drop=FALSE]
        #collapsign <- "*"
        result <- data.frame(incl=incl[rownames(result.matrix)],
                             PRI=pri[rownames(result.matrix)],
                             cov.r=cov.r[rownames(result.matrix)],
                             stringsAsFactors=FALSE,
                             row.names=writePrimeimp(result.matrix, collapse=collapse, uplow=uplow, use.tilde=use.tilde))
    }
    else { # there is no combination which exceeds incl.cut
        if (relation == "necessity") {
            expressions <- seq_len(nrow(nk)) + 1
            # val.outcome and sum.outcome are already initialized
            incov <- apply(maxmat, 2, function(x) sum(pmin(x, val.outcome))/c(sum(x), sum.outcome))
            pri <- apply(maxmat, 2, function(x) {
                prisum <- sum(pmin(x, 1 - x, val.outcome))
                (sum(pmin(x, val.outcome)) - prisum)/(sum(val.outcome) - prisum)
            })
            
            na.maxscores <- is.na(incov[1, ])
            incov <- incov[, !na.maxscores]
            expressions <- expressions[!na.maxscores]
            pri <- pri[!na.maxscores]
            
            incl <- incov[2, ]
            cov.r  <- incov[1, ]
            
            expressions <- expressions[(incl > incl.cut | check.equal(incl, incl.cut)) & (cov.r > cov.cut | check.equal(cov.r, cov.cut))]
            
            index <- 0 # !!!!!!!!!!!!!!
            while ((index <- index + 1) < length(expressions)) {
                expressions <- setdiff(expressions, findSubsets(noflevels + 1, expressions[index], max(expressions)))
            }
            
            if (length(expressions) == 0) {
                stop("\nThere are no groupings that match given criteria.\n\n", call. = FALSE)
            }
            
            result.matrix <- getRow(noflevels + 1, expressions)
            rownames(result.matrix) <- expressions
            colnames(result.matrix) <- conditions
            result.matrix <- sortMatrix(result.matrix)
            sum.zeros <- apply(result.matrix, 1, function(idx) sum(idx == 0))
            result.matrix <- result.matrix[order(sum.zeros, decreasing=TRUE), , drop=FALSE]
            collapse <- "+"
            result <- data.frame(incl=incl[rownames(result.matrix)],
                                 PRI=pri[rownames(result.matrix)],
                                 cov.r=cov.r[rownames(result.matrix)],
                                 stringsAsFactors=FALSE,
                                 row.names=writePrimeimp(result.matrix, collapse=collapse, uplow=uplow, use.tilde=use.tilde))
        }
        else {
            stop("\nThere are no groupings that match given criteria.\n\n", call. = FALSE)
        }
    }
    
    expressions <- result.matrix
    fc <- apply(mydata[, conditions], 2, function(x) any(x %% 1 > 0))
    mins <- apply(expressions, 1, function(e) {
        apply(mydata[, conditions], 1, function(v) {
            
            if (any(ox <- e[fc] == 1)) {
                v[fc][ox] <- 1 - v[fc][ox]
            }
            
            if (length(cp <- v[!fc]) > 0) {
                v[!fc][e[!fc] != cp + 1] <- 0
                v[!fc][e[!fc] == cp + 1] <- 1
            }
            if (collapse == "+") {
                return(max(v[e != 0]))
            }
            else {
                return(min(v[e != 0]))
            }
        })
    })
    colnames(mins) <- rownames(result)
    mins <- as.data.frame(mins)
    return(structure(list(incl.cov=result, coms=mins, use.letters=use.letters, letters=replacements), class="ss"))
}




