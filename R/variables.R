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

fillInMissingRowNames <- function(row.names, variable)
{
    if(is.matrix(variable))
        return(variable[match(row.names, rownames(variable)), ])
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
        return(suppressWarnings(predict(object$original, newdata = newdata, na.action = na.pass, type = "probs")))
    if (object$type == "Binary Logit")
    {
        probs <- suppressWarnings(predict(object$original, newdata = newdata, na.action = na.pass, type = "response"))
        probs <- cbind(1 - probs, probs)
        colnames(probs) <- levels(Observed(object))
        return(probs)
    }
    xs <- 0:max(Observed(object), na.rm = TRUE)
    if (object$type == "Poisson")
    {
        log.lambdas <- suppressWarnings(predict(object$original, newdata = newdata, na.action = na.pass, type = "link"))
        lambdas <- exp(log.lambdas)
        return(computePoissonEsqueProbabilities(xs, lambdas, dpois))
    }
    stop(paste0("Probabilities are not computed for models of type '", object$type, "."))
}
