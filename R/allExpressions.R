"allExpressions" <-
function(no.conditions, inside=TRUE, arrange=FALSE) {
    create <- function(idx) {
        rep.int(c(rep.int(NA,3^(idx-1)), rep.int(0,3^(idx-1)), rep.int(1,3^(idx-1))),
                3^no.conditions/3^idx)
        }
    return.matrix <- sapply(no.conditions:1, create)[-1, ]
    
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

