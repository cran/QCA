base3rows <- function(noflevels) {
    splitRows <- function(start.row, end.row, conds) {
        jump <- ceiling((3^conds - 3)/18)*3
        block.length <- (end.row - start.row - jump)/2
        end.first <- start.row + block.length
        start.third <- end.row - block.length
        mylist[[1]] <- c(end.first, start.third)
        conds <- conds - 1
        if (conds > 2) {
            mylist[[2]] <- Recall(start.row, end.first, conds)
            mylist[[3]] <- Recall(start.third, end.row, conds)
            }
        return(mylist)
        }
    rows <- 3^length(noflevels)
    start.row <- (rows + 1)/2
    end.row <- rows - 1
    mylist <- list()
    mylist <- splitRows(start.row, end.row, length(noflevels))
    mylist <- c(start.row, unique(unlist(mylist)), end.row)
    return(sort(c(mylist, mylist + 1)))
    }

