#' @export
resid.Regression <- function(object, ...)
{
    residuals(object, ...)
}
#' @export
residuals.Regression <- function(object, type = "raw", ...)
{
    notValidForPartial(object, "residuals")
    if (type == "raw" & object$type %in% c("Ordered Logit", "Multinomial Logit", "Binary Logit"))
        return(flipU::UnclassIfNecessary(Observed(object)) - flipU::UnclassIfNecessary(predict(object)))
    resids <- residuals(object$original, ...)
    fillInMissingRowNames(rownames(object$model), resids)
}

#' @export
probabilities <- function(x, ...)
{
    notValidForPartial(x, "probabilities")
    if (x$type == "Linear")
        stop("'probabilities' is not applicable to linear regression models.")
    if (x$type %in% c("Ordered Logit", "Multinomial Logit"))
        return(suppressWarnings(predict(x$original, newdata = x$model, na.action = na.pass, type = "probs")))
    if (x$type == "Binary Logit")
        return(suppressWarnings(predict(x$original, newdata = x$model, na.action = na.pass, type = "response")))[, 2]
    xs <- 0:max(Observed(x), na.rm = TRUE)
    if (x$type == "Poisson"){
        log.lambdas <- suppressWarnings(predict(x$original, newdata = x$model, na.action = na.pass, type = "link"))
        lambdas <- exp(log.lambdas)
        return(computePoissonEsqueProbabilities(xs, lambdas, dpois))
    }
    stop(paste0("Probabilities are not computed for models of type '", x$type, "."))
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

#' @export
predict.Regression <- function(object, newdata = object$model, na.action = na.pass, ...)
{
    notValidForPartial(object, "predict")
    predicted <- if (any(class(object$original) == "glm"))
        suppressWarnings(predict.glm(object$original, newdata = newdata, na.action = na.action, type = "response"))
    else
        predict(object$original, newdata = newdata, na.action = na.action)
    # if (flipU::IsCount(object$type))
    #      return(floor(predicted))
    if (object$type == "Binary Logit")
        return(factor(as.integer(predicted >= 0.5) + 1, labels = levels(Observed(object))))
    predicted
}


#' @export
fitted.Regression <- function(object, ...)
{
    notValidForPartial(object, "fitted")
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
fitted.values.Regression <- function(object, ...)
{
    fitted(object, ...)
}

#' \code{observed} Observed values used in fitting a model with an outcome variable.
#'
#' @export
Observed <- function(x) UseMethod("Observed", x)

#' #' @export
#' Observed.default <- function(x)
#' {
#'     fitted(x)
#' }

##' #@method observed Regression
#' @export
Observed.Regression <- function(x)
{
    x$model[[x$outcome.name]]
}


