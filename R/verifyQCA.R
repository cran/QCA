"verify.qmcc" <-
function(mydata, outcome = "", conditions = c(""), incl.rem = FALSE,
         expl.1 = FALSE, expl.0 = FALSE, expl.ctr = FALSE, expl.mo = FALSE,
         incl.1 = FALSE, incl.0 = FALSE, incl.ctr = FALSE, incl.mo = FALSE,
         quiet = FALSE, details = FALSE, chart = FALSE, use.letters = TRUE,
         show.cases = FALSE, tt = FALSE) {

     # check if the data has column names
    if (is.null(colnames(mydata))) {
        cat("\n")
        stop("Please specify the column names for your data.\n\n", call. = FALSE)
        }
    
     # check the outcome specified by the user
    if (nchar(outcome) == 0) {
        cat("\n")
        stop("You haven't specified the outcome variable.\n\n", call. = FALSE)
        }
    else if (! outcome %in% colnames(mydata)) {
        cat("\n")
        stop("The outcome's name is not correct.\n\n", call. = FALSE)
        }
    
     # subset the data with the conditions specified
    if (length(conditions) > 1) {
        if (outcome %in% conditions) {
            cat("\n")
            stop('Variable "', outcome, '" cannot be both outcome _and_ condition!\n\n', call. = FALSE)
             }
        if (!all(conditions %in% names(mydata))) {
            cat("\n")
            stop("The conditions' names are not correct.\n\n", call. = FALSE)
            }
        else {
            mydata <- mydata[, c(conditions, outcome)]
            }
        }
    else if (nchar(conditions) > 0) {
        if (outcome %in% conditions) {
            cat("\n")
            stop('Variable "', outcome, '" cannot be both outcome _and_ condition!\n\n', call. = FALSE)
             }
        else {
            cat("\n")
            stop("Cannot find a solution with only one condition.\n\n", call. = FALSE)
            }
        }
    
     # check if all cases have been included in analysis
    if ((expl.0 | incl.0) & (expl.1 | incl.1) & (expl.ctr | incl.ctr) & incl.rem) {
        cat("\n")
        stop("You have included all cases in the analysis!\n\n", call. = FALSE)
        }
    
     # if more than 26 conditions (plus one outcome), we cannot use letters
    if (use.letters & ncol(mydata) > 27) {
        cat("\n")
        stop("Cannot use letters. There are more than 26 conditions.\n\n", call. = FALSE)
        }
    
     # check if the user specifies something to explain
    if (sum(expl.0, expl.1, expl.ctr) == 0 ) {
        cat("\n")
        stop("You have not specified what to explain.\n\n", call. = FALSE)
        }
    }

         
"verify.truthTable" <-
function(mydata, outcome = "", conditions = c(""), inside = FALSE,
         complete = FALSE, show.cases = FALSE) {
    
    if (nchar(outcome) == 0) {
        cat("\n")
        stop("You haven't specified the outcome variable.\n\n", call. = FALSE)
        }
    else if (! outcome %in% colnames(mydata)) {
        cat("\n")
        stop("The outcome's name is not correct.\n\n", call. = FALSE)
        }
    
     # subset the data with the conditions specified
    if (length(conditions) > 1) {
        if (outcome %in% conditions) {
            cat("\n")
            stop('Variable "', outcome, '" cannot be both outcome _and_ condition!\n\n', call. = FALSE)
             }
        if (!all(conditions %in% names(mydata))) {
            cat("\n")
            stop("The conditions' names are not correct.\n\n", call. = FALSE)
            }
        else {
            mydata <- mydata[, c(conditions, outcome)]
            }
        }
    else if (nchar(conditions) > 0) {
                cat("\n")
                stop("Cannot create a truth table with only one condition.\n\n", call. = FALSE)
        }
    }
