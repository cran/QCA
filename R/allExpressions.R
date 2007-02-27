"allExpressions" <-
function(no.conditions, inside=TRUE, arrange=FALSE) {
    return.matrix <- createMatrix(rep(3, no.conditions))
    return.matrix <- return.matrix - 1
    return.matrix[return.matrix < 0] <- NA
    if (arrange) {
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

