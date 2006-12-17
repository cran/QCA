`factorize` <- 
function(my.string, use.letters=TRUE, 
         sort.by.literals=FALSE, sort.by.number=FALSE) {
             
    trimst <- function(string) gsub("^[[:space:]]+|[[:space:]]+$", "", string)
    my.string <- trimst(unlist(strsplit(my.string, "\\+")))
    if (length(my.string) == 1) return(my.string)
    
    ifelse(use.letters, collapsemethod <- "", collapsemethod <- "*")
    ifelse(use.letters, splitmethod <- "", splitmethod <- "\\*")
    
     # check if use.letters should be TRUE
    if (!use.letters & !any(unlist(sapply(my.string, strsplit, "")) == "*")) {
        cat("\n")
        stop("Cannot find the separator character between conditions.\n",
             "      Are you sure conditions' names are not simple letters?\n\n",
             call. = FALSE)
        }
    
    # create a list with all prime implicants splitted by literals
    list.my.string <- sapply(my.string, strsplit, splitmethod)
    
     # check if use.letters should be FALSE
    if (use.letters & any(unlist(lapply(list.my.string, "==", "*")))) {
         # try and split by "*", see what the result is
        list.my.string2 <- sapply(my.string, strsplit, "\\*")
         # check if all splitted elements have length 1 (one literal)
        one.literal <- all(lapply(list.my.string2, function(x) all(sapply(x, nchar) == 1)))
        if (one.literal) {
            collapsemethod <- "*"
            star.indices <- lapply(list.my.string, function(x) which(x == "*"))
            for (i in 1:length(list.my.string)) {
                list.my.string[[i]] <- list.my.string[[i]][-star.indices[[i]]]
                }
            }
        else {
            cat("\n")
            stop("Conditions' names are _not_ simple letters.\n\n", call. = FALSE)
            }
        }
    
     # create a matrix with all combinations of prime implicants to be compared for similar literals
    all.combs <- as.matrix(combn(length(list.my.string), 2))
    
     # create a list with matched literals between prime implicants
    match.list <- as.list(apply(all.combs, 2, function(x) {
        matches <- match(list.my.string[[x[1]]], list.my.string[[x[2]]])
        if (!all(is.na(matches))) list.my.string[[x[1]]][which(matches > 0)] else NA
        }))
    names(match.list) <- lapply(match.list, paste, collapse=collapsemethod)
    
     # see wich comparisons didn't yield similar literals
    null.branches <- unlist(lapply(match.list, function(x) all(is.na(x))))
     # erase those branches from the list
    match.list <- match.list[!null.branches]
     # and from all combinations
    all.combs <- all.combs[, !null.branches, drop=FALSE]
    
    if (sort.by.literals) {
        sort.by.number <- FALSE
        lengths.vector <- as.numeric(unlist(lapply(match.list, length)))
        match.list <- match.list[rev(order(lengths.vector))]
        all.combs <- all.combs[, rev(order(lengths.vector))]
        }
    
     # prepare a vector showing which columns from all.combs have been used
     # to extract common.factor factors
    selected.cols <- rep(FALSE, ncol(all.combs))
    complex.list <- vector("list", length(selected.cols))
    
    
    extract <- function(match.list, all.combs, complex.list, my.string.index) {
        initial.index <- my.string.index
        for (i in 1:length(match.list)) {
            common.factor <- match.list[[i]]
             # see which other branches contain all common.factor literals from the current branch
            similar.branches <- unlist(lapply(match.list[-i], function (x) all(common.factor %in% x)))
            
            if (any(similar.branches)) {
                 # see which are the other similar branches
                similar.index <- (1:length(match.list))[-i][similar.branches]
                 # see which are the prime implicants with similar common.factor factors
                my.string.index <- sort(unique(c(all.combs[, c(i, similar.index)])))
                }
            else {
                 # see which are the prime implicants with similar common.factor factors
                my.string.index <- c(all.combs[, i])
                }
                
             # paste the other literals from each index, separated by " + "
            sol <- paste(sapply(my.string.index, function(x) {
                    paste(list.my.string[[x]][!(list.my.string[[x]] %in% common.factor)], collapse=collapsemethod)
                    }), collapse=" + ")
            
            common.factor <- paste(match.list[[i]], collapse=collapsemethod)
            
             # then combine everything having the common.factor in front of the paranthesys
            factor.sol <- paste(common.factor, collapsemethod, "(", sol, ")", sep="")
            selected.cols <- apply(all.combs, 2, function(x) any(x %in% my.string.index))
            
            if (!is.null(initial.index)) my.string.index <- sort(unique(c(initial.index, my.string.index)))
            
            if (sum(!selected.cols) == 0) {
                 # no other comparison can be made; add all other prime implicants that have not been used
                if (length(my.string[-my.string.index]) > 0) {
                        factor.sol <- paste(factor.sol, paste(my.string[-my.string.index], collapse=" + "), sep=" + ")
                    }
                names(complex.list)[i] <- factor.sol
                complex.list[[i]] <- factor.sol
                }
            else {
                sift <- function(x, y, z) {
                    sift.list <- list(match.list=NULL, all.combs=NULL)
                    sift.list[[1]] <- x[!z]
                    sift.list[[2]] <- y[, which(!z), drop=FALSE]
                    sift.list
                    }
                sift.list <- sift(match.list, all.combs, selected.cols)
                
                names(complex.list)[i] <- factor.sol
                complex.list[[i]] <- vector("list", length(sift.list$match.list))
                complex.list[[i]] <- Recall(sift.list$match.list, sift.list$all.combs, complex.list[[i]], my.string.index)
                }
            }
        complex.list
        }
    
    
    my.string.index <- NULL
    complex.list <- extract(match.list, all.combs, complex.list, my.string.index)
    
    final.solution <- unique(names(unlist(complex.list)))
    
    if (length(final.solution) > 1) {
        final.solution.list <- strsplit(final.solution, "\\.")
        
        if (sort.by.number) {
            order.vector <- order(unlist(lapply(lapply(final.solution.list, "[", 1), nchar)), decreasing=TRUE)
            final.solution.list <- final.solution.list[order.vector]
            final.solution <- final.solution[order.vector]
            }
        
        all.combs <- as.matrix(combn(length(final.solution.list), 2))
        
        match.list <- apply(all.combs, 2, function(x) {
             # compare only solutions with the same length
            if (length(final.solution.list[[x[1]]]) == length(final.solution.list[[x[2]]])) {
                 # return x (the indices from final.solution) if all items are equivalent
                if (all(final.solution.list[[x[1]]] %in% final.solution.list[[x[2]]])) x
                }
            })
        
        # see if there are any null branches
        null.branches <- unlist(lapply(match.list, is.null))
        
        if (!all(null.branches)) {
             # remove those branches from match.list
            match.list <- match.list[-which(null.branches)]
             # the remaining branches contain equivalent (duplicated) solutions
            equivalent.solutions <- unlist(lapply(match.list, "[", 2))
             # remove equivalent solutions from final.solution
            final.solution <- final.solution[-equivalent.solutions]
            }
        
        final.solution <- gsub("\\.", " + ", final.solution)
        
        cat("\nMultiple solutions found:\n\n")
        for (i in 1:length(final.solution)) {
            cat(paste("Solution ", i, ":", sep=""), final.solution[i], "\n")
            }
        cat("\n")
        }
    else {
        cat("\nSolution:", final.solution, "\n\n")
        }
    }
