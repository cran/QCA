`getInfo` <- function(data, conditions = "", outcome = "") {
    if (is.matrix(data)) {
        data <- as.data.frame(data)
    }
    if (identical(outcome, "")) {
        if (identical(conditions, "")) {
            conditions <- colnames(data)
        }
        else {
            if (is.null(colnames(data))) {
                cat("\n")
                stop(simpleError("Unspecified column names in the data.\n\n"))
            }
            outcome <- setdiff(colnames(data), conditions)
            if (length(outcome) == 0) {
                cat("\n")
                stop(simpleError("Not enough columns in the data.\n\n"))
            }
            outcome <- outcome[1]
        }
    }
    else {
        if (identical(conditions, "")) {
            conditions <- setdiff(colnames(data), outcome)
        }
    }
    dc.code <- unique(unlist(lapply(data, function(x) {
        if (is.numeric(x)) {
            return(x[x < 0])
        }
        else {
            return(as.character(x[x %in% c("-", "dc")]))
        }
    })))
    if (length(dc.code) == 0) {
        dc.code <- -1
    }
    else if (length(dc.code) > 1) {
        cat("\n")
        stop(simpleError("Multiple \"Don't care\" codes found.\n\n"))
    }
    colnms <- colnames(data)
    data <- as.data.frame(lapply(data, function(x) {
        x <- as.character(x)
        x[x == dc.code] <- -1
        return(asNumeric(x))
    }))
    colnames(data) <- colnms
    data[data < 0] <- -1
    fuzzy.cc <- apply(data[, conditions, drop = FALSE], 2, function(x) any(x %% 1 > 0))
    hastime <- logical(length(conditions))
    for (i in seq(length(conditions))) {
        if (!fuzzy.cc[i]) {
            copy.cc <- data[, i]
            if (any(copy.cc < 0)) {
                hastime[i] <- TRUE
                copy.cc[copy.cc < 0] <- max(copy.cc) + 1
                data[, i] <- copy.cc
            }
        }
    }
    noflevels <- apply(data[, conditions, drop = FALSE], 2, max) + 1
    noflevels[noflevels == 1] <- 2
    noflevels[fuzzy.cc] <- 2
    noflevels <- as.integer(noflevels)
    if (length(conditions) == ncol(data)) {
        return(noflevels)
    }
    return(list(data = data, fuzzy.cc = fuzzy.cc, hastime = hastime, dc.code = dc.code, noflevels = as.numeric(noflevels)))
}
