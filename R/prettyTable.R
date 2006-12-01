"prettyTable" <-
function(mytable) {
    mytable <- as.matrix(mytable)
    
    if (is.logical(mytable)) {
        mytable2 <- mytable
        mytable[mytable2]  <- "x"
        mytable[!mytable2] <- "-"
        }
    
    if(is.null(colnames(mytable))) colnames(mytable) <- rep(" ", ncol(mytable))
    
    nchars <- nchar(colnames(mytable))
    colnames(mytable)[nchars == 1] <- format(colnames(mytable)[nchars == 1], width=3, justify="centre")
    nchars[nchars == 1] <- 3
    
    for (i in 1:(ncol(mytable) - any(colnames(mytable) == "lines"))) {
        mytable[, i] <- format(format(mytable[, i]), width=nchars[i], justify="centre")
        }
    
    noquote(mytable)
    }

