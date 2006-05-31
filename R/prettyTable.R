"prettyTable" <-
function(mytable) {
    mytable <- as.matrix(mytable)
    for (i in 1:(ncol(mytable) - any(colnames(mytable) == "lines"))) {
        if (sum(nchar(colnames(mytable)[i])) == 1) {
            colnames(mytable)[i] <- paste("", colnames(mytable)[i], "")
            for (j in 1:nrow(mytable)) {mytable[j, i] <- paste("", mytable[j, i], "")}
            }
        else {
            for (j in 1:nrow(mytable)) {
                if (nchar(mytable[j, i]) < nchar(colnames(mytable)[i])) {
                    middle <- floor(nchar(colnames(mytable)[i])/2) - 1 + nchar(colnames(mytable)[i]) %% 2
                    mytable[j,i] <- paste(paste(rep(" ", middle), collapse=""),
                                          mytable[j, i], sep="")
                    }
                }
            }
        }
    noquote(mytable)
    }

