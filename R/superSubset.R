`superSubset` <-
function(mydata, outcome = "", conditions = c(""), relation = "necessity",
         incl.cut = 1, cov.cut = 0, neg.out = FALSE, use.tilde = FALSE, use.letters = FALSE) {
    
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
        (sum(pmin(x, val.outcome)) - prisum)/(sum(x) - prisum)
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




##################################################################################################
##################################################################################################
##################################################################################################



`superSubsetOld` <-
function(mydata, outcome = "", conditions = c(""), relation = "", incl.cut = 1, cov.cut = 0,
         index = 1, neg.out = FALSE, use.tilde = FALSE) {
    
    if (all(conditions == c(""))) {
        conditions <- names(mydata)[-which(names(mydata) == outcome)]
    }
    
    verify.tt(mydata, outcome, conditions)
    
    if (relation == "" | is.na(match(relation, c("necessity", "sufficiency")))) {
        relation <- "necessity"
    }
    else if (length(relation) > 1) {
        relation <- relation[1]
    }
    
    colnames(mydata) <- toupper(colnames(mydata))
    conditions <- toupper(conditions)
    outcome <- toupper(outcome)
    
    mydata <- mydata[, c(conditions, outcome)]
    nofconditions <- length(conditions)
    
    uplow <- !use.tilde
    
    fuzzy.cc <- apply(mydata[, conditions], 2, function(x) any(x %% 1 > 0))
    if (mv.data <- any(mydata[, conditions] > 1)) {
        uplow <- use.tilde <- FALSE
    }
            
    check.equal <- function(x, y) {
        check.vector <- as.logical(unlist(lapply(x, all.equal, y)))
        check.vector[is.na(check.vector)] <- FALSE
        return(check.vector)
    }
    
    findClosest <- function(start.line, noflevels, mbase) {
        closest <- vector(length=sum(noflevels))
        clindex <- 1
        for (i in seq(length(noflevels))) {
            for (j in seq(noflevels[i])) {
                closest[clindex] <- j*mbase[i] + 1
                clindex <- clindex + 1
            }
        }
        return(closest + start.line)
    }
    
    getInclusion <- function(line.number, relation, connector, fuzzy.cc) {
        minscores <- maxscores <- vector(length=nrow(mydata))
        for (i in seq(nrow(mydata))) {
            values <- as.numeric(mydata[i, conditions])
            x <- getRow(noflevels + 1, line.number)
            
            if (any(onex3k <- x[fuzzy.cc] == 1)) {
                values[fuzzy.cc][onex3k] <- 1 - values[fuzzy.cc][onex3k]
            }
            
            copy.values <- values[!fuzzy.cc]
            if (length(copy.values) > 0) {
                values[!fuzzy.cc][x[!fuzzy.cc] != copy.values + 1] <- 0
                values[!fuzzy.cc][x[!fuzzy.cc] == copy.values + 1] <- 1
            }
            minscores[i] <- min(values[x != 0])
            maxscores[i] <- max(values[x != 0])
        }
        
        if (relation == "sufficiency") {
                inclusion <- sum(pmin(minscores, mydata[, outcome]))/sum(minscores)
                coverage  <- sum(pmin(minscores, mydata[, outcome]))/sum(mydata[, outcome])
        }
        else if (relation == "necessity") {
                inclusion <- sum(pmin(minscores, mydata[, outcome]))/sum(mydata[, outcome])
                coverage  <- sum(pmin(minscores, mydata[, outcome]))/sum(minscores)
            if (connector == "or") {
                inclusion <- sum(pmin(maxscores, mydata[, outcome]))/sum(mydata[, outcome])
                coverage  <- sum(pmin(maxscores, mydata[, outcome]))/sum(maxscores)
            }
        }
        
        inclusion[is.na(inclusion)] <- NA
        coverage[is.na(coverage)] <- NA
        return(c(inclusion, coverage))
    }
    
    extract <- function(closest, complex.list, noflevels.local, mbase.local, conditions.local, relation, connector, fuzzy.cc, uplow, use.tilde) {
        for (i in seq(length(closest))) {
            complex.list[[i]] <- vector("list", 2)
            row3k <- getRow(noflevels + 1, closest[i])
            colnames(row3k) <- conditions
            
            incov <- getInclusion(closest[i], relation, connector, fuzzy.cc)
            term <- writePrimeimp(row3k, uplow=uplow, use.tilde=use.tilde)
            if (relation == "necessity" & connector == "or") {
                term <- gsub("\\*", "+", term)
            }
            
            complex.list[[i]][[1]] <- c(row3k, tline=closest[i], term, inclusion=incov[1], coverage=incov[2])
            if (!is.na(incov[1])) {
                
                relation.inclusion <- FALSE
                if (relation == "sufficiency" | (relation == "necessity" & connector == "or")) {
                    relation.inclusion <- (incov[1] < incl.cut) & (length(noflevels.local) > 1)
                }
                else if (relation == "necessity") {
                    relation.inclusion <- (incov[1] > incl.cut | check.equal(incov[1], incl.cut)) & length(noflevels.local) > 1
                }
                
                if (relation.inclusion) {
                    noflevels.index <- which(cumsum(noflevels.local) >= i)[1]
                    next.closest <- findClosest(closest[i] - 1, noflevels.local[-noflevels.index], mbase.local[-noflevels.index])
                    match.vector <- match(next.closest, term.vector)
                    next.closest <- next.closest[!is.na(match.vector)]
                    match.vector <- match.vector[!is.na(match.vector)]
                    if (any(match.vector)) {
                        term.vector <<- term.vector[-match.vector]
                        complex.list[[i]][[2]] <- vector("list", length(next.closest))
                        complex.list[[i]][[2]] <- Recall(next.closest,
                                                         complex.list[[i]][[2]],
                                                         noflevels.local[-noflevels.index],
                                                         mbase.local[-noflevels.index],
                                                         conditions.local[-noflevels.index],
                                                         relation,
                                                         connector,
                                                         fuzzy.cc,
                                                         uplow,
                                                         use.tilde)
                    }
                }
                else {
                    complex.list[[i]] <- complex.list[[i]][-2]
                }
            }
            else {
                complex.list[[i]] <- complex.list[[i]][-2]
            }
        }
        return(complex.list)
    }
    
    if (neg.out) {
        mydata[, outcome] <- 1 - mydata[, outcome]
    }
    
    noflevels <- rep(2, nofconditions)
    mbase <- rev(c(1, cumprod(rev(noflevels + 1))))[-1]
        
    mostmin <- findClosest(0, noflevels, mbase)
    term.vector <- seq_len(prod(noflevels + 1))
    connector <- "and"
    result <- as.data.frame(matrix(unlist(extract(mostmin,
                                                  vector("list", length(mostmin)),
                                                  noflevels,
                                                  mbase,
                                                  conditions,
                                                  relation,
                                                  connector,
                                                  fuzzy.cc,
                                                  uplow,
                                                  use.tilde)), ncol=length(conditions) + 4, byrow=T))
    result[, ncol(result) - 1] <- as.numeric(as.character(result[, ncol(result) - 1]))
    result <- result[!is.na(result[, ncol(result) - 1]), ]
    result <- result[result[, ncol(result) - 1] > incl.cut | check.equal(result[, ncol(result) - 1], incl.cut), ]
    
    if (nrow(result) == 0) {
        connector <- "or"
        result <- as.data.frame(matrix(unlist(extract(mostmin,
                                                      vector("list", length(mostmin)),
                                                      noflevels,
                                                      mbase,
                                                      conditions,
                                                      relation,
                                                      connector,
                                                      fuzzy.cc,
                                                      uplow,
                                                      use.tilde)), ncol=length(conditions) + 4, byrow=T))
        result <- result[!is.na(result[, ncol(result) - 1]), ]
        result[, ncol(result) - 1] <- as.numeric(as.character(result[, ncol(result) - 1]))
        result <- result[result[, ncol(result) - 1] > incl.cut | check.equal(result[, ncol(result) - 1], incl.cut), ]
    }
    
    for (i in seq(ncol(result))) {
        result[, i] <- as.character(result[, i])
    }
    
    names(result) <- c(conditions, "tline", "grouping", "incl", "cov")
    result$tline <- as.numeric(result$tline)
    result$incl <- as.numeric(result$incl)
    result$cov <- as.numeric(result$cov)
    
    index <- 1
    while (index <= nrow(result)) {
        subs <- findSupersets(noflevels + 1, result$tline[index])
        subs.match <- match(subs, result$tline[-index])
        subs.match <- subs.match[!is.na(subs.match)]
        if (any(subs.match)) {
            result <- result[-index, ]
        }
        else {
            index <- index + 1
        }
    }
    
    result <- result[result$cov > cov.cut, ]
    
    if (nrow(result) > 0) {
        rownames(result) <- seq(nrow(result))
        return(structure(list(result=result[, seq(ncol(result) - 2, ncol(result))]), class="ss"))
    }
    else {
        cat("\nThere are no groupings that match given criteria.\n\n")
    }
}

