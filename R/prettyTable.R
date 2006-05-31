"prettyTable" <-
function(mytable) {
    mytable <- as.matrix(mytable)
    if (is.logical(mytable)) {
        mytable2 <- mytable
        mytable[mytable2]  <- "x"
        mytable[!mytable2] <- "-"
        }
    
    if(is.null(colnames(mytable))) {
        nchar.colnames <- rep(4, ncol(mytable))
        }
    else {
        nchar.colnames <- nchar(colnames(mytable))
        }
        
    for (i in 1:(ncol(mytable) - any(colnames(mytable) == "lines"))) {
        if (nchar.colnames[i] == 1) {
            colnames(mytable)[i] <- paste("", colnames(mytable)[i], "")
            for (j in 1:nrow(mytable)) mytable[j, i] <- paste("", mytable[j, i], "")
            }
        else {
            for (j in 1:nrow(mytable)) {
                if (nchar(mytable[j, i]) < nchar.colnames[i]) {
                    middle <- floor(nchar.colnames[i]/2) - 1 + nchar.colnames[i] %% 2
                    mytable[j,i] <- paste(paste(rep(" ", middle), collapse=""),
                                          mytable[j, i], sep="")
                    }
                }
            }
        }
    noquote(mytable)
    }

