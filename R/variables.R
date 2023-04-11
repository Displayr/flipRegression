#' @importFrom stats resid residuals
#' @export
resid.Regression <- function(object, ...)
{
    residuals(object, ...)
}

#' @importFrom flipTransformations UnclassIfNecessary
#' @importFrom flipU IsRServer
#' @importFrom stats residuals
#' @export
residuals.Regression <- function(object, type = "raw", ...)
{
    notValidForPartial(object, "residuals")
    notValidForCrosstabInteraction(object, "residuals")
    if (isTRUE(object$stacked) && IsRServer())
        stop("Saving residuals is currently not supported for stacked data.")
    if (type == "raw" & object$type %in% c("Ordered Logit", "Multinomial Logit", "Binary Logit"))
    {
        observed <- Observed(object)
        levs <- levels(observed)
        observed <- UnclassIfNecessary(observed, FALSE)
        predicted <- match(predict(object), levs) # Dealing with situations where the predictions omit a class.
        return(observed - predicted)
    }
    resids <- residuals(object$original, ...)
    fillInMissingRowNames(rownames(object$model), resids)
}


computePoissonEsqueProbabilities <- function(xs, lambdas, density)
{
    n <- length(lambdas)
    k <- length(xs)
    result <- matrix(density(rep(xs[-k], rep(n, k - 1)), lambdas), nrow = n)
    result <- cbind(result, 1 - apply(result, 1, sum))
    dimnames(result) <- list(rownames(lambdas), c(xs[-k], paste(">=", xs[k])))
    result
}

#' \code{predict.Regression}
#'
#' Predicts a model outcome based on \code{newdata} and a fitted Regression \code{object}.
#' NA is returned for cases with unfitted factor levels.
#' @param object A \code{Regression} object.
#' @param newdata Optionally, a data frame including the variables used to fit the model.
#' If omitted, the \code{data} supplied to \code{Regression()} is used after any filtering.
#' @param na.action Function determining what should be done with missing values in \code{newdata}.
#' The default is to predict \code{NA}.
#' @param ... Additional arguments to pass to predict.LDA.

#' @export
#' @importFrom stats predict.glm
#' @importFrom flipData Observed CheckPredictionVariables
#' @importFrom flipU IsRServer
predict.Regression <- function(object, newdata = object$model, na.action = na.pass, ...)
{
    if (object$test.interaction)
        stop("Cannot predict from regression model with Crosstab interaction.")
    if (isTRUE(object$stacked) && IsRServer())
        stop("Saving predicted values is currently not supported for stacked data.")
    newdata <- CheckPredictionVariables(object, newdata)
    notValidForPartial(object, "predict")
    notValidForCrosstabInteraction(object, "predict")
    predicted <- if (any(class(object$original) == "glm"))
        suppressWarnings(predict.glm(object$original, newdata = newdata, na.action = na.action, type = "response"))
    else if ("polr" %in% class(object$original))
    {
        original.coef <- object$original$coefficients
        if (length(original.coef) == 0L)
            predict(object$original, newdata = newdata, na.action = na.action)
        else
        {
            ## Make ordered logit work when variables have been removed due to
            ## colinearity
            .cleanNames <- function(v)
            {
                cnames <- sub("^[[:print:]]*[$](Variables|Questions)[$]", "", v)
                cnames <- sub("^`", "", cnames)
                cnames <- sub("`$", "", cnames)
                make.names(cnames)
            }
            original.coef <- object$original$coefficients
            coef.names <- .cleanNames(coefNamesBeforeOmitting(object))
            new.coef <- numeric(length(coef.names))
            names(new.coef) <- coef.names

            new.coef[.cleanNames(names(original.coef))] <- original.coef
            ## names(new.coef) <- RemoveBackticks(names(new.coef))
            reg.model <- object$original
            object$original$coefficients <- new.coef
            reg.model$coefficients <- new.coef
            names(reg.model$model) <- paste0("`", .cleanNames(names(reg.model$model)), "`")
            predict(reg.model, newdata = newdata, na.action = na.action)
        }
    }
    else
        predict(object$original, newdata = newdata, na.action = na.action)

    # if (flipU::IsCount(object$type))
    #      return(floor(predicted))
    if (object$type == "Binary Logit" || object$type == "Multinomial Logit")
    {
        levs <- levels(Observed(object))
        predicted <- if(object$type == "Binary Logit")
            as.integer(predicted >= 0.5) + 1
        else
            match(predicted, levs)
        predicted <- factor(predicted, levels = 1:length(levs), labels = levs)
    }
    predicted
}

coefNamesBeforeOmitting <- function(object)
{
    tl <- attr(object$original$terms, "term.labels")
    coef.names <- CleanBackticks(tl)
    has.backticks <- tl[1L] != coef.names[1L]
    coef.names <- as.list(coef.names)

    for (i in 1:length(coef.names))
    {
        nm <- coef.names[[i]]
        element <- object$original$contrasts[[nm]]
        if (!is.null(element) && element == "contr.treatment")
        {
            vname <- nm
            if (has.backticks)
                vname <- paste0("`", vname, "`")
            coef.names[[i]] <- paste0(vname, levels(object$original$model[[nm]])[-1])
        }
    }

    unlist(coef.names)
}

#' @importFrom stats fitted
#' @importFrom flipU IsRServer
#' @export
fitted.Regression <- function(object, ...)
{
    if (isTRUE(object$stacked) && IsRServer())
        stop("Saving fitted values is currently not supported for stacked data.")
    notValidForPartial(object, "fitted")
    notValidForCrosstabInteraction(object, "fitted")
    fitted.values <- fitted(object$original)
    fillInMissingRowNames(rownames(object$model), fitted.values)
}

#' This function ensures that residuals and fitted values
#' from the resid and fitted methods have the same length
#' and names as the original data including missing values,
#' i.e. the behaviour of stats::napass
#' residuals may have been stripped of their names
#' if they are identical to 1,...,n (i.e. no missing values)
#' to reduce output size in reduceOutputSize()
#' @noRd
fillInMissingRowNames <- function(row.names, variable)
{
    if(is.matrix(variable))
    {
        if (is.null(rownames(variable)))
            rownames(variable) <- seq_len(nrow(variable))
        return(variable[match(row.names, rownames(variable)), ])
    }else if (is.null(names(variable)))
        names(variable) <- seq_len(length(variable))
    result <- variable[match(row.names, names(variable))]
    names(result) <- row.names
    result
}

#' @export
#' @importFrom stats fitted.values
fitted.values.Regression <- function(object, ...)
{
    fitted(object, ...)
}

#' \code{Observed.Regression}
#'
#' Observed values used in fitting a model with an outcome variable.
#' @param x A 'Regression' model.
#' @importFrom flipData Observed
#' @export
Observed.Regression <- function(x)
{
    x$model[[x$outcome.name]]
}


#' \code{Observed.FitRegression}
#'
#' Observed values used in fitting a model with an outcome variable.
#' @param x A 'FitRegression' model.
#' @importFrom flipData Observed
#' @export
Observed.FitRegression <- function(x)
{
    ## use stats::terms() output because it expands
    ## dot in formulae, so don't need to supply data
    x$original$model[, OutcomeName(x$original$terms)]
}

#' \code{probabilities}
#'
#' @param object A model of some kind.
#' @details Computes probabilities that are applicable from the relevant model. For exmaple, probabilities
#' of class membership from a regression model. This is included for backwards compatibilty.
#' @export
probabilities <- function(object)
{
    Probabilities.Regression(object)
}


#' @importFrom flipData Probabilities
#' @export
flipData::Probabilities


#' \code{Probabilities.Regression}
#'
#' @param object A \code{Regression} object.
#' @param newdata Optionally, a data frame including the variables used to fit the model.
#'     If omitted, \code{object$model} is used after any filtering.
#' @param ... Additional arguments (not used).
#' @importFrom stats na.pass dpois
#' @importFrom flipData Probabilities Observed
#' @importFrom flipU IsRServer
#' @details Computes probabilities that are applicable from the relevant model. For exmaple, probabilities
#' of class membership from a regression model.
#' @export
Probabilities.Regression <- function(object, newdata, ...)
{
    notValidForPartial(object, "probabilities")
    notValidForCrosstabInteraction(object, "probabilities")
    if (missing(newdata) || is.null(newdata))
        newdata <- object$model
    if (object$type == "Linear")
        stop("'probabilities' is not applicable to linear regression models.")
    if (isTRUE(object$stacked) && IsRServer())
        stop("Saving probabilitiles is currently not supported for stacked data.")
    if (object$type %in% c("Ordered Logit", "Multinomial Logit"))
    {
        probs <- suppressWarnings(predict(object$original, newdata = newdata,
                                          na.action = na.pass, type = "probs"))
        if (object$type == "Multinomial Logit" && NCOL(probs) == 1L)
            probs <- cbind(1 - probs, probs)
        if (is.null(colnames(probs)))
            colnames(probs) <- levels(object$estimation.data[, object$outcome.name])
        return(probs)
    }

    if (object$type == "Binary Logit")
    {
        probs <- suppressWarnings(predict(object$original, newdata = newdata, na.action = na.pass, type = "response"))
        outcome.levels <- levels(Observed(object))
        if (length(outcome.levels) == 1L)
        {
            warning("The Outcome variable only has a single category for this Binary Logit Regression model. ",
                    "The computed probabilities here are very likely to be uninformative and the outcome variable ",
                    "of the original Binary Logit model inspected. It should have the second category added and ",
                    "the Binary Logit model recomputed. The computed probabilities are all near zero as it ",
                    "is attempting to compute the probability of observing a category that wasn't included in the ",
                    "original data.")
            outcome.levels <- c(paste0("Not ", outcome.levels),
                                outcome.levels)
        }
        probs <- cbind(1 - probs, probs)
        colnames(probs) <- outcome.levels
        return(probs)
    }
    xs <- 0:max(Observed(object), na.rm = TRUE)
    if (object$type == "Poisson")
    {
        log.lambdas <- suppressWarnings(predict(object$original, newdata = newdata, na.action = na.pass, type = "link"))
        lambdas <- exp(log.lambdas)
        return(computePoissonEsqueProbabilities(xs, lambdas, dpois))
    }
    stop("Probabilities are not computed for models of type '", object$type, ".")
}

#' Adds dummy variable adjustment information for the estimation data template
#' @param regression.model A Regression object, at least a partially created one,
#'                         it doesn't need to be of Regression class, but needs to have,
#'                         at the very least, a list with the the original model fit,
#'                         in the "original" slot and and the "estimation.data.template"
#' @return The estimation.data.template element with dummy variable adjustment information added
#' @importFrom stats coefficients
appendDummyAdjustmentsToTemplate <- function(regression.model) {
    if (!inherits(regression.model, "Regression"))
        stop("appendDummyAdjustmentsToTemplate only works with Regression models.")
    original.model <- regression.model[["original"]]
    template <- regression.model[["estimation.data.template"]]
    if (is.null(template))
        stop("appendDummyAdjustmentsToTemplate only works with Regression models that ",
             "have an estimation.data.template.")
    coefficients <- coefficients(original.model)
    # Coefficients are either a vector or matrix depending on model type
    model.type <- getModelType(original.model)
    # Get the names of the coefficients (either column names or names, depending on model type)
    coefNamesFunc <- if (model.type == "Multinomial Logit") colnames else names
    coefficient.names <- coefNamesFunc(coefficients)
    dummy.adjusted.coefs <- grepDummyVars(coefficient.names)
    # Nothing to do if there are no dummy variable adjustments
    if (!any(dummy.adjusted.coefs)) return(template)
    # Extract the estimation data and model data
    estimation.data <- regression.model[["estimation.data"]]
    model.data <- regression.model[["model"]]
    dummy.adjusted.coefs <- coefficient.names[dummy.adjusted.coefs]
    # Construct the dummy variable templates and update the matched variables
    # with their imputed values (replacement values for NA)
    dummy.template <- mapply(function(variable, variable.name) {
        predictors.matching.dummy <- attr(variable, "predictors.matching.dummy")
        first.replaced <- which.max(variable)
        for (predictor in predictors.matching.dummy)
            template[[predictor]][["imputed.value"]] <<- dummyAdjustment(
                model.data[[predictor]],
                first.replaced
            )
        list(type = "numeric", default.value = 0)
    }, estimation.data[dummy.adjusted.coefs], dummy.adjusted.coefs, SIMPLIFY = FALSE)
    structure(
        c(template, dummy.template),
        outcome.name = attr(template, "outcome.name")
    )
}

# determines the appropriate dummy variable adjustment "imputed value",
# if factor, it is just the baseline (first) level, if numeric, it is the
# mean of the observed values, this is determined by looking up the
# the first replaced value (2nd arg has this index) from the input variable
dummyAdjustment <- function(variable, first.replacement) {
    if (is.factor(variable)) return(levels(variable)[1L])
    variable[first.replacement]
}
