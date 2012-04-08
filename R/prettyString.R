#`prettyString` <-
#function(preamble, mystring, blanks="") {
#    blankchars <- nchar(preamble) + nchar(blanks)
#    paste(paste(strwrap(paste(preamble, mystring),
#                        prefix=paste(rep(" ", blankchars), collapse=""),
#                        width=floor(getOption("width")*0.95), initial=""),
#                collapse="\n"), "\n", sep="")
#}


`prettyString` <-
function(string.vector, string.width=80, repeat.space=5, separator=",") {
    if (length(string.vector) > 1) {
        string <- string.vector[1]
        startpoint <- 1
        for (j in 2:length(string.vector)) {
            if (nchar(paste(string.vector[startpoint:j], collapse=separator)) > string.width) {
                string <- paste(paste(string, "\n", sep=separator), 
                                paste(rep(" ", repeat.space), collapse=""), 
                                string.vector[j], sep="")
                startpoint <- j
            }
            else {
                string <- paste(string, string.vector[j], sep=separator)
            }
        }
        string
    }
    else {
        string.vector
    }
}


