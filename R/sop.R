`sop` <- function(expression, snames = "", use.tilde = FALSE, noflevels) {
    syscalls <- unlist(lapply(sys.calls(), deparse))
    if (any(withdata <- grepl("with\\(", syscalls))) {
        snames <- get(unlist(strsplit(gsub("with\\(", "", syscalls), split = ","))[1], envir = length(syscalls) - which(withdata))
    }
    snames <- splitstr(snames)
    multivalue <- any(grepl("[{|}]", expression))
    if (multivalue) {
        expression <- toupper(gsub("[*]", "", expression))
        verify.multivalue(expression, snames = snames, noflevels = noflevels) 
    }
    sl <- ifelse(identical(snames, ""), FALSE, ifelse(all(nchar(snames) == 1), TRUE, FALSE))
    getbl <- function(expression) {
        bl <- splitMainComponents(gsub("[[:space:]]", "", expression))
        bl <- splitBrackets(bl)
        bl <- removeSingleStars(bl)
        bl <- splitPluses(bl)
        blu <- unlist(bl)
        bl <- splitStars(bl, ifelse((sl | any(hastilde(blu) & !tilde1st(blu))) & !grepl("[*]", expression) & !multivalue, "", "*"))
        bl <- solveBrackets(bl)
        bl <- simplifyList(bl)
        return(bl)
    }
    bl <- list()
    if (any(hastilde(expression))) {
        use.tilde <- TRUE
    }
    for (i in seq(length(expression))) {
        bl <- c(bl, lapply(getbl(expression[i]), function(x) {
            x <- unlist(x)
            if (multivalue) {
                outx <- toupper(curlyBrackets(x, outside=TRUE))
                inx <- curlyBrackets(x)
                x <- paste(outx, "{", inx, "}", sep = "")
            }
            x <- cx <- unique(unlist(x))
            tx <- which(hastilde(x))
            if (!multivalue) {
                if (any(tx)) {
                    x <- notilde(x)
                    uptx <- x[tx] %in% toupper(x)
                    lotx <- x[tx] %in% tolower(x)
                    x[tx[uptx]] <- tolower(x[tx[uptx]])
                    x[tx[lotx]] <- toupper(x[tx[lotx]])
                }
            }
            cx <- cx[!duplicated(x)]
            if (any(duplicated(toupper(notilde(cx))))) {
                return(NULL)
            }
            else {
                if (use.tilde) {
                    tx <- hastilde(cx)
                    x <- notilde(cx)
                    lotx <- x %in% tolower(x)
                    tx[lotx] <- !tx[lotx]
                    x <- toupper(x)
                    x[tx] <- paste("~", x[tx], sep = "")
                    cx <- x
                }
                return(cx)
            }
        }))
    }
    bl <- unique(bl[!unlist(lapply(bl, is.null))])
    redundants <- logical(length(bl))
    if (length(bl) > 1) {
        for (i in seq(length(bl) - 1)) {
            for (j in seq(i + 1, length(bl))) {
                if (all(bl[[i]] %in% bl[[j]]) & length(bl[[i]]) < length(bl[[j]])) {
                    redundants[j] <- TRUE
                }
                if (all(bl[[j]] %in% bl[[i]]) & length(bl[[j]]) < length(bl[[i]])) {
                    redundants[i] <- TRUE
                }
            }
        }
    }
    bl <- bl[!redundants]
    if (length(bl) > 0) {
        bl <- bl[order(unlist(lapply(bl, length)))]
    }
    if (!identical(snames, "")) {
        bl <- unique(unlist(lapply(bl, function(x) {
            paste(x[order(match(toupper(notilde(x)), toupper(snames)))], collapse = "*")
        })))
        if (any(blsn <- is.element(toupper(notilde(bl)), toupper(snames)))) {
            bl[blsn] <- bl[blsn][order(match(toupper(notilde(bl[blsn])), toupper(snames)))]
        }
    }
    else {
        bl <- unique(unlist(lapply(bl, function(x) paste(x[order(notilde(x))], collapse = "*"))))
    }
    if (multivalue) {
        blt <- as.vector(apply(translate(bl, snames = snames, noflevels = noflevels), 1, function(x) {
            x <- x[x >= 0]
            return(as.vector(paste(names(x), "{", x, "}", sep = "", collapse = "*")))
        }))
        if (identical(bl, blt)) {
            if (sl) {
                bl <- gsub("[*]", "", bl)
            }
            return(paste(bl, collapse = " + "))
        }
        return(Recall(paste(blt, collapse = " + ")))
    }
    else {
        if (sl) {
            bl <- gsub("[*]", "", bl)
        }
        return(paste(bl, collapse = " + "))
    }
}
