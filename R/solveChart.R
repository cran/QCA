"solveChart" <-
function(chart) {
    if (!is.logical(chart)) {
        cat("\n")
        stop("Please use a TRUE/FALSE matrix. See createChart's output.", call. = FALSE)
        }
    if (nrow(chart) > 1 & ncol(chart) > 1) {
         ## solution provided by Gabor Grothendieck
         ## the function lp (from package lpSolve) finds one (guaranteed minimum) solution
         # k will be the minimum number of prime implicants necessary to cover all columns
        k <- sum(lp("min", rep(1, nrow(chart)), t(chart), ">=", 1)$solution)
         # create a matrix with all possible combinations of k prime implicants
        combos <- as.matrix(combn(nrow(chart), k))
         # sol.matrix will be a subset of the chart matrix with all minimum solutions
        sol.matrix <- combos[, apply(combos, 2, function(idx) {
                                                    if (is.matrix(chart[idx, ])) {
                                                        all(colSums(chart[idx, ]))
                                                        }
                                                    else {
                                                        all(chart[idx, ])
                                                        }
                                                    })]
        }
    else {
        sol.matrix <- 1:nrow(chart)
        }
    return(sol.matrix)
    }
