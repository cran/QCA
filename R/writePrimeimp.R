`writePrimeimp` <-
function(mymat, mv = FALSE, use.tilde = FALSE, collapse = "*", snames = "") {
    if (any(mymat > 2)) {
        mv <- TRUE
    }
    if (identical(snames, "")) {
        snames <- colnames(mymat)
    }
    else {
        mymat <- t(mymat)
    }
    chars <- snames[col(mymat)]
    if (mv) {
        chars <- matrix(paste(chars, "{", mymat - 1, "}", sep=""), nrow = nrow(mymat))
    }
    else {
        lochars <- if (use.tilde) paste0("~", chars) else tolower(chars)
        chars <- ifelse(mymat == 1L, lochars, chars)
    }
    keep <- mymat > 0L
    as.vector(unlist(lapply(split(chars[keep], row(chars)[keep]), paste, collapse = collapse)))
}
