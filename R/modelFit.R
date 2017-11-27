`modelFit` <-
function(model, theory) {
    if (!(methods::is(model, "qca") | methods::is(model, "deMorgan"))) {
        cat("\n")
        stop(simpleError("The model should be a minimization object or its negation.\n\n"))
    }
    if (is.character(theory)) {
        if (length(theory) != 1) {
            cat("\n")
            stop(simpleError("Theory should be a single character expression.\n\n"))
        }
    }
    else {
        cat("\n")
        stop(simpleError("Theory should be a character expression or its negation.\n\n"))
    }
    noflevels <- model$tt$noflevels
    snames <- model$tt$options$conditions
    if (model$tt$options$use.letters) {
        snames <- LETTERS[seq(length(snames))]
    }
    use.tilde <- model$options$use.tilde
    pims <- model$pims
    if ("i.sol" %in% names(model)) {
        pims <- lapply(model$i.sol, function(x) x$pims)
        names(pims) <- NULL
        pims <- do.call("cbind", pims)
        solutions <- lapply(model$i.sol, function(x) x$solution)
    }
    else {
        solutions <- list(model$solution)
    }
    models <- unlist(lapply(solutions, function(x) unlist(lapply(x, paste, collapse = " + "))))
    slengths <- unlist(lapply(solutions, length))
    if (is.null(names(solutions))) {
        names(models) <- "M"
        if (slengths > 1) {
            names(models) <- paste("M", seq(slengths), sep = "")
        }
    }
    else {
        mnum <- unlist(lapply(slengths, function(x) {
            mnum <- ""
            if (x > 1) {
                mnum <- seq(x)
            }
            paste("M", mnum, sep = "")
        }))
        names(models) <- paste(mnum, rep(names(solutions), each = slengths), sep = "-")
    }
    result <- intersections <- vector(mode = "list", length = length(models))
    arglist <- list(snames = snames, use.tilde = use.tilde, noflevels = noflevels)
    for (i in seq(length(models))) {
        expression <- models[i]
        cpims <- pims[, unlist(strsplit(expression, split = " \\+ ")), drop = FALSE]
        cpims$MODEL <- compute(expression, data = model$tt$initial.data)
        cpims$THEORY <- compute(theory, data = model$tt$initial.data)
        intersections <- rep("", 4)
        intersections[1] <- do.call("intersection", c(list(theory, expression), arglist))
        intersections[2] <- do.call("intersection", c(list(negate(theory, snames = snames), expression), arglist))
        intersections[3] <- do.call("intersection", c(list(theory, negate(expression, snames = snames)), arglist))
        intersections[4] <- do.call("intersection", c(list(negate(theory, snames = snames), negate(expression, snames = snames)), arglist))
        intnms <- c("MODEL*THEORY", "MODEL*theory", "model*THEORY", "model*theory")
        for (nm in seq(4)) {
            int <- intersections[nm]
            if (int == "") {
                cpims[[intnms[nm]]] <- rep(0, nrow(model$tt$initial.data))
            }
            else {
                cpims[[intnms[nm]]] <- compute(int, data = model$tt$initial.data)
            }
        }
        intersections[intersections == ""] <- "-"
        names(intersections) <- intnms
        pofobj <- pof(cpims, model$tt$initial.data[, model$tt$options$outcome], relation = "sufficiency")
        pofobj$incl.cov <- pofobj$incl.cov[, 1:3]
        pofobj$incl.cov[is.na(pofobj$incl.cov[, 1]), 3] <- NA
        pofobj$modelfit <- list(model = expression, theory = theory, intersections = intersections)
        result[[i]] <- pofobj
    }
    if (length(result) == 1) {
        return(result[[1]])
    }
    return(structure(result, class = "modelFit"))
}
