# Copyright (c) 2018, Adrian Dusa
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
