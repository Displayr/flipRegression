#' Goodness-of-Fit
#'
#' \code{GoodnessOfFit} summary is a generic function used to produce result summaries of the
#' results of the model object. The function invokes particular \code{\link{methods}}
#' which depend on the \code{\link{class}} of the first argument.
#' reports the goodness-of-fit of an object.
#' @param object An object for which a summary is desired..
#' @param digits Minimal number of significant digits, see \code{\link{print.default}}.
#' @param ... Additional arguments affecting the goodness-of-fit displayed.
#' #@examples
#'
#' # linear regression
#' x <- rnorm(10)
#' y <- rnorm(10) + x
#' mod <- lm(y ~ x)
#' GoodnessOfFit(mod)
#'
#' # MDS - square
#' library(smacof)
#' data(breakfastDissimilarities)
#' mdsInterval <- smacofSym(breakfastDissimilarities[[4]],
#'     type = "interval", eps = 1e-12, itmax = 100000)
#' GoodnessOfFit(mdsInterval)
#' # MDS - rectangular
#' data(breakfast)
#' GoodnessOfFit(smacofRect(breakfast))
#' @export
GoodnessOfFit <- function(object, digits = max(3L, getOption("digits") - 3L), ...) {
    UseMethod("GoodnessOfFit")
}

#' @describeIn GoodnessOfFit  Default goodness-of-fit \eqn{R^2}.
#' @importFrom stats cor
#' @export
GoodnessOfFit.default = function(object, digits = max(3L, getOption("digits") - 3L), ...) {

    # special case for Ordered Logit
    if (class(object) == "polr")
    {
        fitted <- UnclassIfNecessary(predict(object), FALSE)
        observed <- UnclassIfNecessary(Observed(object), FALSE)

    } else
    {
        obs.fit <- FittedAndObserved(object)
        fitted <- obs.fit$fitted
        observed <- obs.fit$observed
    }
    r2 = cor(fitted, observed, use = "complete.obs")^2
    names(r2) <- "R-squared"
    description <- list("Variance explained: ",
                        formatC(100 * r2, digits = digits),
                        "%\n(R-squared * 100)")
    GoodnessOfFitInternal(r2, description, object$call)
}




#' Packages up the object goodness of fit,
#' \code{GoodnessOfFitInternal} puts together the various bits of \code{GoodnessOfFit}.  These should usually not
#' be used directly unless by experienced users.
#' @param value The computed goodness-of-fit.
#' @param description Text elements used to construct the print statment.
#' @param call The original call used to create the object for which goodness-of-fit is being computed.
#' @param ... Additional arguments affecting the goodness-of-fit displayed.
#' @return A \code{\link{list}} with components
#' @return value The computed goodness-of-fit.
#' @return description Text elements used to construct the print statment.
#' @return call The original call used to create the object for which goodness-of-fit is being computed.
#' @export
GoodnessOfFitInternal <- function(value, description, call) {
    result <- list(call = call, value = value, description = description)
    class(result) <- "flipGOF"
    result
}

#' @describeIn GoodnessOfFit  Goodness-of-fit for a Regression object. Computed as the \eqn{R^2} statistic.
#' With factors, the index value of the factor is used. With unordered factors, this will often be
#' grieviously-downward biased.
#' @importFrom flipStatistics Correlation
#' @importFrom flipTransformations UnclassIfNecessary
#' @importFrom stats predict
#' @export
GoodnessOfFit.Regression <- function(object, digits = max(3L, getOption("digits") - 3L), ...) {
    if (object$missing == "Use partial data (pairwise correlations)")
        r2 <- object$original$original$R2
    else if (object$type == "Linear" & is.null(object$weights))
        r2 <- object$summary$r.square
    else
    {
        set.seed(1223)
        predicted <- UnclassIfNecessary(predict(object)[object$subset], FALSE)
        if (sd(predicted) == 0)
            r2 <- 0
        else
        {
            observed <- UnclassIfNecessary(Observed(object)[object$subset], FALSE)
            cor <- Correlation(predicted, observed, object$weights[object$subset])
            r2 <- cor * cor
        }
    }
    names(r2) <- "R-squared"
    description <- list("Variance explained: ",
                        formatC(100 * r2, digits = digits),
                        "%\n(R-squared * 100)")
    GoodnessOfFitInternal(r2, description, object$call)
}



#' Extracts fitted and observed.
#' \code{FittedAndObserved} extracts vectors of fitted (or predicted) and observed values from an object for use in computing
#' \code{GoodnessOfFit}.
#' @param object An object for which a summary is desired..
#' @return fitted Fitted values from the object.
#' @return observed Observed values from the object.
#' @export
FittedAndObserved = function(object) {
    fitted = fitted(object)
    if(is.null(fitted))
        fitted = predict(object)
    observed = fitted + resid(object)
    list(fitted = fitted, observed = observed)
}



nullDeviance <- function(x)
{
    null.d <- x$original$null.deviance
    if (!is.null(null.d))
        return(null.d)
    observed <- if (x$type %in% c("Ordered Logit", "Multinomial Logit") & !is.null(x$weights))
        as.vector(by(x$weights[x$subset], Observed(x)[x$subset], sum))
    else
        table(Observed(x)[x$subset])
    observed <- observed[observed > 0 & !is.na(observed)]
    ll <- sum(observed * log(prop.table(observed)))
    -2 * ll
}

#' \code{McFaddensRhoSquared}
#'
#' @param x A 'Regression' model.
#' @details 1 - the deviance divided by the null deviance.
#' McFadden, D. (1974) "Conditional logit analysis of qualitative choice behavior."
#' Pp. 105-142 in P. Zarembka (ed.), Frontiers in Econometrics
#' @importFrom stats deviance
#' @export
McFaddensRhoSquared <- function(x)
{
    if (x$type == "Linear")
        stop("McFadden's rho-squared statistic is not computed for models of type 'Linear'.")
    1 - deviance(x$original) / nullDeviance(x)
}

#' @importFrom stats logLik
#' @export
logLik.Regression <- function(object, ...)
{
    logLik(object$original, ...)
}

#' @importFrom stats AIC
#' @export
AIC.Regression <- function(object, ...)
{
    aic <- if (is.null(object$original$aic))
        AIC(object$original, ...)
    else
        object$original$aic
    names(aic) <- "AIC"
    aic
}

#' @importFrom stats extractAIC
#' @export
extractAIC.Regression <- function(fit, scale, k = 2, ...)
{
    aic <- AIC.Regression(fit, ...)
    df <- if (is.null(fit$original$df))
        extractAIC(fit$original, scale, k, ...)[1]
    else
        fit$original$df
    c(df = unname(df), AIC = unname(aic))
}
