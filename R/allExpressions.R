"allExpressions" <-
function(no.conditions, inside=FALSE, arrange=FALSE) {
    create <- function(idx) {
        rep.int(c(rep.int(NA,3^(idx-1)), rep.int(0,3^(idx-1)), rep.int(1,3^(idx-1))),
                3^no.conditions/3^idx)
        }
    return.matrix <- sapply(no.conditions:1, create)[-1, ]
    
    if (arrange) {
        sortMatrix <- function(submatrix) {
            if (all(is.na(submatrix)) | all(!is.na(submatrix)) | !is.matrix(submatrix)) {
                submatrix
                }
            else {
                submatrix <- submatrix[order(!is.na(submatrix[ ,ncol(submatrix)] )), ]
                lastcol <- ncol(submatrix)
                nas <- is.na(submatrix[, lastcol])
                if (any(nas)) {
                    submatrix[which(!nas), -lastcol] <- Recall(submatrix[which(!nas), -lastcol])
                    submatrix[which(nas) , -lastcol] <- Recall(submatrix[which(nas) , -lastcol])
                    }
                submatrix
                }
            }
        
        return.matrix <- sortMatrix(return.matrix)
        sum.nas <- apply(return.matrix, 1, function(idx) sum(is.na(idx)))
        return.matrix <- return.matrix[order(sum.nas, decreasing=TRUE), ]
        }
    
    
    if (inside) {
        return.matrix
        }
    else {
        return.matrix[is.na(return.matrix)] <- ""
        print(prettyTable(return.matrix))
        }
    }

