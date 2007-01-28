`createChart` <- 
function (input, initial, rows, cols) {
	
	input2 <- matrix(logical(length(input)), dim(input))
	input2[input > 0] <- TRUE
	
	result <- t(sapply(1:nrow(input), function(x) {
		apply(initial, 1, function(y) {
			all(input[x, input2[x,]] == y[input2[x,]])
			})
		}))
    
    rownames(result) <- rows
    colnames(result) <- cols
    return(result)
}

