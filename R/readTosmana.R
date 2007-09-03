

`readTosmana` <- function(filename) {
    require(XML)
    root <- xmlTreeParse(filename)
    metadata   <- xmlElementsByTagName(root$doc$children[[1]], "Meta")
    MVQCAtable <- xmlElementsByTagName(root$doc$children[[1]], "MVQCA-Table")
    varnames <- thresholds <- NULL 
    for (i in 1:length(metadata)) {
        varnames <- c(varnames, xmlValue(xmlElementsByTagName(metadata[[i]], "name")[[1]]))
        thresholds <- c(thresholds, xmlValue(xmlElementsByTagName(metadata[[i]], "thresholds")[[1]]))
        }
    MVQCAdata <- list() 
    for (i in 1:length(varnames)) {
        for (j in 1:length(MVQCAtable)) {
            MVQCAdata[[varnames[i]]][j] <- xmlValue(xmlElementsByTagName(MVQCAtable[[j]], varnames[i])[[1]])
            }
        }
    
    case.descriptor <- any(thresholds == ":I")
    
    if (case.descriptor) {
        case.descriptor <- MVQCAdata[[which(thresholds == ":I")]]
        MVQCAdata <- MVQCAdata[-which(thresholds == ":I")]
        MVQCAdata <- as.data.frame(lapply(MVQCAdata, as.numeric))
        rownames(MVQCAdata) <- case.descriptor
        }
    else {
        MVQCAdata <- as.data.frame(lapply(MVQCAdata, as.numeric))
        }
    return(MVQCAdata)
    }

