# Copyright (c) 2019, Adrian Dusa
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, in whole or in part, are permitted provided that the
# following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * The names of its contributors may NOT be used to endorse or promote products
#       derived from this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL ADRIAN DUSA BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

`simplify` <- function(expression, snames = "", noflevels = NULL, use.tilde = FALSE, ...) {
    if (length(expression) > 1) {
        expression <- expression[1]
    }
    other.args <- list(...)
    syscalls <- unlist(lapply(sys.calls(), deparse))
    enter   <- ifelse (is.element("enter",   names(other.args)), "",  "\n") 
    all.sol <- if     (is.element("explain", names(other.args))) other.args$explain else FALSE
    if (!is.character(expression)) {
        cat(enter)
        stop(simpleError(paste0("The expression input should be character.", enter, enter)))
    }
    if (any(withdata <- grepl("with\\(", syscalls))) {
        snames <- get(unlist(strsplit(gsub("with\\(", "", syscalls), split = ","))[1], envir = length(syscalls) - which(withdata))
    }
    snames <- admisc::splitstr(snames)
    multivalue <- any(grepl("[{|}]", expression))
    if (multivalue) {
        expression <- toupper(gsub("[*]", "", expression))
        admisc::checkMV(expression, snames = snames, noflevels = noflevels) 
    }
    if (!grepl("[+]", expression) & grepl("[,]", expression)) {
        if (multivalue) {
            values <- curlyBrackets(expression)
            atvalues <- paste("@", seq(length(values)), sep = "")
            for (i in seq(length(values))) {
                expression <- gsub(values[i], atvalues[i], expression)
            }
            expression <- gsub(",", "+", expression)
            for (i in seq(length(values))) {
                expression <- gsub(atvalues[i], values[i], expression)
            }
        }
        else {
            oldway <- unlist(strsplit(gsub("[-|;|,|[:space:]]", "", expression), split = ""))
            if (!possibleNumeric(oldway) & length(oldway) > 0) {
                expression <- gsub(",", "+", expression)
            }
        }
    }
    `remred` <- function(x) {
        if (nrow(x) > 1) {
            redundant <- logical(nrow(x))
            for (i in seq(nrow(x) - 1)) {
                if (!redundant[i]) {
                    for (j in seq(i + 1, nrow(x))) {
                        if (!redundant[j]) {
                            subsetrow <- admisc::checkSubset(x[c(i, j), , drop = FALSE])
                            if (!is.null(subsetrow)) {
                                redundant[c(i, j)[subsetrow]] <- TRUE
                            }
                        }
                    }
                }
            }
            x <- x[!redundant, , drop = FALSE]
        }
        return(x)
    }
    `dnf` <- function(x, noflevels = NULL) {
        if (is.null(noflevels)) noflevels <- rep(2, length(x))
        result <- matrix(nrow = 0, ncol = ncol(x))
        for (i in seq(nrow(x))) {
            xi <- x[i, ]
            wxi <- which(xi == 0)
            if (length(wxi) > 0) {
                rest <- createMatrix(noflevels[wxi]) + 1
                colnames(rest) <- wxi
                basemat <- matrix(rep(xi[-wxi], nrow(rest)), nrow = nrow(rest), byrow = TRUE)
                colnames(basemat) <- seq(ncol(result))[-wxi]
                resmat <- cbind(basemat, rest)
                resmat <- resmat[, order(colnames(resmat)), drop = FALSE]
            }
            else {
                resmat <- matrix(xi, ncol = ncol(x))
            }
            result <- rbind(result, resmat)
        }
        colnames(result) <- colnames(x)
        return(unique(result))
    }
    `classify` <- function(x) {
        class(x) <- c("character", "simplify")
        return(x)
    }
    if (any(grepl("[(|)]", expression))) {
        bl <- admisc::expandBrackets(expression, snames = snames, noflevels = noflevels)
    }
    else {
        bl <- expression
    }
    if (grepl("~", bl)) {
        use.tilde <- TRUE
    }
    if (identical(bl, "")) {
        return(classify(""))
    }
    tlist <- list(expression = bl, snames = snames)
    if (!is.null(noflevels)) {
        tlist$noflevels <- noflevels
    }
    tlist$noflevels <- noflevels
    bl <- tryCatch(do.call(translate, tlist), error = function(e) e)
    if (is.list(bl)) {
        return(classify(""))
    }
    sl <-  all(nchar(colnames(bl)) == 1)
    expressions <- matrix(nrow = 0, ncol = ncol(bl))
    colnames(expressions) <- colnames(bl)
    for (i in seq(nrow(bl))) {
        expressions <- rbind(expressions, as.matrix(expand.grid(lapply(bl[i, ], function(x) {
            admisc::asNumeric(admisc::splitstr(x)) + 1
        }))))
    }
    expressions <- remred(expressions)
    if (is.null(noflevels)) noflevels <- rep(2, ncol(expressions))
    dnfexp <- dnf(expressions, noflevels = noflevels)
    if (nrow(dnfexp) == prod(noflevels)) {
        expressions <- ""
    }
    else {
        sols <- minimize(cbind(dnfexp - 1, OUTCOME_COLUMN = 1), simplify = TRUE,
            outcome = "OUTCOME_COLUMN", use.tilde = use.tilde, all.sol = all.sol)
        expressions <- unlist(lapply(sols$solution, paste, collapse = " + "))
    }
    if (sl) {
        expressions <- gsub("[*]", "", expressions)
    }
    return(classify(expressions))
}
`sop` <- function(...) {
    .Deprecated(msg = "Function sop() is deprecated, and has been renamed to simplify()\n")
    simplify(...)
}
