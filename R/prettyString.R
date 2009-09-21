`prettyString` <-
function(preamble, mystring, blanks="") {
    paste(paste(strwrap(paste(preamble, mystring),
                        prefix=paste(rep(" ", nchar(blanks) + nchar(preamble) + 1), collapse=""),
                        width=81, initial=""),
                collapse="\n"), "\n", sep="")
}

