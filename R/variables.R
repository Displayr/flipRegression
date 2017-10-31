#' @importFrom stats resid residuals
#' @export
resid.Regression <- function(object, ...)
{
    residuals(object, ...)
}

#' @importFrom flipTransformations UnclassIfNecessary
#' @importFrom stats residuals
#' @export
residuals.Regression <- function(object, type = "raw", ...)
{
    notValidForPartial(object, "residuals")
    notValidForCrosstabInteraction(object, "residuals")
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
predict.Regression <- function(object, newdata = object$model, na.action = na.pass, ...)
{
    if (object$test.interaction)
        stop("Cannot predict from regression model with Crosstab interaction.")

    newdata <- CheckPredictionVariables(object, newdata)
    notValidForPartial(object, "predict")
    notValidForCrosstabInteraction(object, "predict")
    predicted <- if (any(class(object$original) == "glm"))
        suppressWarnings(predict.glm(object$original, newdata = newdata, na.action = na.action, type = "response"))
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


#' @importFrom stats fitted
#' @export
fitted.Regression <- function(object, ...)
{
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
#' of class membership from a refression model. This is included for backwards compatibilty.
#' @export
probabilities <- function(object)
{
    Probabilities.Regression(object)
}


#' \code{Probabilities.Regression}
#'
#' @param object A model of some kind.
#' @importFrom stats na.pass dpois
#' @importFrom flipData Probabilities
#' @details Computes probabilities that are applicable from the relevant model. For exmaple, probabilities
#' of class membership from a regression model.
#' @export
Probabilities.Regression <- function(object)
{
    notValidForPartial(object, "probabilities")
    notValidForCrosstabInteraction(object, "probabilities")
    if (object$type == "Linear")
        stop("'probabilities' is not applicable to linear regression models.")
    if (object$type %in% c("Ordered Logit", "Multinomial Logit"))
        return(suppressWarnings(predict(object$original, newdata = object$model, na.action = na.pass, type = "probs")))
    if (object$type == "Binary Logit")
        return(suppressWarnings(predict(object$original, newdata = object$model, na.action = na.pass, type = "response")))[, 2]
    xs <- 0:max(Observed(object), na.rm = TRUE)
    if (object$type == "Poisson")
    {
        log.lambdas <- suppressWarnings(predict(object$original, newdata = object$model, na.action = na.pass, type = "link"))
        lambdas <- exp(log.lambdas)
        return(computePoissonEsqueProbabilities(xs, lambdas, dpois))
    }
    stop(paste0("Probabilities are not computed for models of type '", object$type, "."))
}
