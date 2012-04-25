`pof` <-
function(setms, mydata, outcome="", neg.out=FALSE, relation = "necessity", ...) {
    
    other.args <- list(...)
    
    recursive <- "recursive" %in% names(other.args)
    individual <- "individual" %in% names(other.args)
    via.eqmcc <- "via.eqmcc" %in% names(other.args)
    
    conditions <- names(mydata)[-which(names(mydata) == outcome)]
    
    if (!recursive) {
        
        noflevels <- truthTable(mydata, outcome=outcome, via.pof=TRUE)
        
        if (!relation %in% c("necessity", "sufficiency")) {
            stop("\nThe relationship should be either \"necessity\" or \"sufficiency\".\n\n", call. = FALSE)
        }
        
        colnames(mydata) <- toupper(colnames(mydata))
        conditions <- toupper(conditions)
        outcome <- toupper(outcome)
        
        mydata <- mydata[, c(conditions, outcome)]
    }
    
    pims <- FALSE
    if (is.data.frame(setms)) {
        if (nrow(setms) == nrow(mydata)) {
            if (all(rownames(setms) == rownames(mydata))) {
                pims <- TRUE
            }
            else {
                cat("\n")
                stop("The row names of the \"setms\" component should be the same as the data row names.\n\n", call. = FALSE)
            }
        }
    }
    else if (is.matrix(setms)) {
        if (!recursive & !individual) {
            if (ncol(setms) == length(conditions)) {
                setms[setms < 0] <- -1
                setms <- setms + 1
            }
            else {
                cat("\n")
                stop("The number of columns in the \"setms\" does not match the number of conditions.\n\n", call. = FALSE)
            }
        }
    }
    else if (is.vector(setms)) {
        if (!is.null(names(setms))) {
            if (all(names(setms) == rownames(mydata))) {
                setms <- data.frame(setms)
                colnames(setms) <- "COM"
                pims <- TRUE
            }
            else {
                cat("\n")
                stop("The \"setms\" argument should be either a matrix of crisp set representation of groupings or a data frame of \"min\" scores.\n\n", call. = FALSE)
            }
        }
        else if (all(!is.na(as.numeric(setms)))) {
            setms <- getRow(noflevels + 1, as.numeric(setms))
        }
        else {
            cat("\n")
            stop("The \"setms\" argument should be either a matrix of crisp set representation of groupings or a data frame of \"min\" scores.\n\n", call. = FALSE)
        }
    }
    else {
        cat("\n")
        stop("The \"setms\" argument should be either a matrix of crisp set representation of groupings or a data frame of \"min\" scores.\n\n", call. = FALSE)
    }
    
    
    
    if (is.matrix(setms) & !recursive) {
        if (is.null(colnames(setms))) {
            colnames(setms) <- conditions
        }
        
        if (is.null(rownames(setms))) {
            use.tilde <- FALSE
            if ("use.tilde" %in% names(other.args)) {
                rownames(setms) <- writePrimeimp(setms, uplow=all(noflevels == 2), use.tilde=other.args$use.tilde)
            }
            else {
                rownames(setms) <- writePrimeimp(setms, uplow=all(noflevels == 2))
            }
        }
    }
    
    
    hastime <- logical(length(conditions))
    for (i in seq(length(conditions))) {
        if (any(mydata[, i] %in% c("-", "dc", "?"))) {
            hastime[i] <- TRUE
        }
    }
    if (!pims) {
        setms <- setms[, !hastime, drop=FALSE]
    }
    mydata[, which(hastime)] <- NULL
    conditions <- conditions[!hastime]
    
    
    val.outcome <- mydata[, outcome]
    if (neg.out) {
        val.outcome <- 1 - val.outcome
    }
    sum.outcome <- sum(val.outcome)
    
    if (pims) {
        mins <- setms
        incl.cov <- matrix(NA, nrow=ncol(setms), ncol=4)
        length.expr <- ncol(setms)
    }
    else {
        fc <- apply(mydata[, conditions], 2, function(x) any(x %% 1 > 0))
        incl.cov <- matrix(NA, nrow=nrow(setms), ncol=4)
        
        length.expr <- nrow(setms)
        
        mins <- apply(setms, 1, function(e) {
            apply(mydata[, conditions], 1, function(v) {
                
                if (any(ox <- e[fc] == 1)) {
                    v[fc][ox] <- 1 - v[fc][ox]
                }
                
                if (length(cp <- v[!fc]) > 0) {
                    v[!fc][e[!fc] != cp + 1] <- 0
                    v[!fc][e[!fc] == cp + 1] <- 1
                }
                
                min(v[e != 0])
            })
        })
    }
    
    pmins <- apply(mins, 2, pmin, val.outcome)
    primins <- apply(mins, 2, function(x) pmin(x, 1 - val.outcome, val.outcome))
    
    if (relation == "necessity") {
        primins <- apply(mins, 2, function(x) pmin(x, 1 - x, val.outcome))
    }
    
    incl.cov[, 1] <- colSums(pmins)/colSums(mins)
    incl.cov[, 2] <- (colSums(pmins) - colSums(primins))/(colSums(mins) - colSums(primins))
    incl.cov[, 3] <- colSums(pmins)/sum.outcome
    
    
    if (relation == "necessity") {
        incl.cov[, 1] <- colSums(pmins)/sum.outcome
        incl.cov[, 2] <- (colSums(pmins) - colSums(primins))/(sum.outcome - colSums(primins))
        incl.cov[, 3] <- colSums(pmins)/colSums(mins)
    }
    
    maxmins <- apply(mins, 1, max)
    inclusions <- apply(apply(mins, 2, function (x) pmin(x, val.outcome)), 1, max)
    prisol <- pmin(maxmins, 1 - val.outcome, val.outcome)
    
    if (relation == "necessity") {
        prisol <- pmin(maxmins, 1 - maxmins, val.outcome)
    }
    
    if (recursive) {
        return(sum(inclusions)/sum.outcome)
    }
    
    for (i in seq(length.expr)) {
        sol.cov.without <- 0
        if (length.expr > 1) {
            if (pims) {
                sol.cov.without <- Recall(setms[, -i, drop=FALSE], mydata, outcome, recursive=TRUE, via.eqmcc=via.eqmcc, neg.out=neg.out)
            }
            else {
                sol.cov.without <- Recall(setms[-i, , drop=FALSE], mydata, outcome, recursive=TRUE, via.eqmcc=via.eqmcc, neg.out=neg.out)
            }
        }
        incl.cov[i, 4] <- sum(inclusions)/sum.outcome - sol.cov.without
    }
    
    colnames(incl.cov) <- c("incl", "PRI", "cov.r", "cov.u")
    
    if (pims) {
        rownames(incl.cov) <- colnames(setms)
    }
    else {
        rownames(incl.cov) <- rownames(setms)
    }
    
    incl.cov <- as.data.frame(incl.cov)
    
    # solution incl, pri and cov
    sol.incl <- sum(inclusions)/sum(maxmins)
    sol.pri <- (sum(inclusions) - sum(prisol))/(sum(maxmins) - sum(prisol))
    sum.cov <- sum(inclusions)/sum.outcome
    
    
    result.list <- list(incl.cov=incl.cov, relation=relation)
    
    if (!pims & via.eqmcc) {
        result.list$sol.incl.cov <- c(sol.incl, sol.pri, sum.cov)
        result.list$pims <- as.data.frame(mins)
    }
    
    if ("individual" %in% names(other.args)) {
        return(result.list)
    }
    
    if ("showc" %in% names(other.args)) {
        if (other.args$showc) {
            result.list$incl.cov <- cbind(result.list$incl.cov, cases = other.args$cases, stringsAsFactors=FALSE)
        }
    }
    
    if ("solution.list" %in% names(other.args)) {
        solution.list <- other.args$solution.list
        length.solution <- length(solution.list)
        individual <- vector("list", length=length.solution)
        
        for (i in seq(length.solution)) {
            if (pims) {
                individual[[i]] <- Recall(setms[ , solution.list[[i]], drop=FALSE], mydata, outcome, individual=TRUE, via.eqmcc=TRUE, individual=TRUE, relation="sufficiency", neg.out=neg.out)
            }
            else {
                individual[[i]] <- Recall(setms[solution.list[[i]], , drop=FALSE], mydata, outcome, individual=TRUE, via.eqmcc=TRUE, individual=TRUE, relation="sufficiency", neg.out=neg.out)
            }
        }
        return(structure(list(overall=result.list, individual=individual, essential=other.args$essential, pims=as.data.frame(mins), relation=relation), class="pof"))
    }
    else {
        return(structure(result.list, class="pof"))
    }
}

