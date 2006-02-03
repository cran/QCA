"pretty.string" <-
function(string.vector, string.width=60, repeat.space=5, separator=";") { 
    if (length(string.vector) > 1) {
        string <- string.vector[1]
        startpoint <- 2
        for (j in 2:length(string.vector)) {
            if (nchar(paste(string.vector[startpoint:j], collapse=separator)) < string.width) {
                string <- paste(string, string.vector[j], sep=separator)
            } else {
                string <- paste(paste(string, "\n", sep=separator), 
                                paste(rep(" ", repeat.space), collapse=""), 
                                string.vector[j], sep="")
                startpoint <- j
            }
        }
        string
    } else {
        string.vector
    }
}

