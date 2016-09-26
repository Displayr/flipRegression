#' \code{Regression} Generalized Regression.
#'
#' @param formula An object of class \code{\link{formula}} (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#'   The details of type specification are given under \sQuote{Details}.
#' @param data A \code{\link{data.frame}}.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param missing How missing data is to be treated in the regression. Options:
#'   \code{"Error if missing data"},
#'   \code{"Exclude cases with missing data"},
#'   \code{"Use partial data (pairwise correlations)"},
#'   \code{"Imputation (replace missing values with estimates)"}, and
#'   \code{"Multiple imputation"}.
#' @param type Defaults to \code{"linear"}. Other types are: \code{"Poisson"},
#'   \code{"Quasi-Poisson"}, \code{"Binary Logit"}, \code{"NBD"},
#'   \code{"Ordered Logit"}, and \code{"Multinomial Logit"}
#' @param robust.se Computes standard errors that are robust to violations of
#'   the assumption of constant variance, using the HC1 (degrees of freedom)
#'   modification of White's (1980) estimator (Long and Ervin, 2000). This parameter is ignored
#'   if weights are applied (as weights already employ a sandwich estimator).
#' @param detail More detailed outputs.
#' @param method The method to be used; for fitting. This will only do something if
#' method = "model.frame", which returns the model frame.
#' @param m The number of imputed samples, if using multiple imputation.
#' @param seed The random number seed used in imputation.
#' @param statistical.assumptions A Statistical Assumptions object.
#' @param auxiliary.data A \code{\link{data.frame}} containing additional variables
#'  to be used in imputation (if required). While adding more variables will improve
#'  the quality of the imputation, it will dramatically slow down the time to estimate.
#'  Factors and Character variables with a large number of categories should not be included,
#'  as they will both slow down the data and are unlikely to be useful
#' @param show.labels Shows the variable labels, as opposed to the names, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param ... Additional argments to be past to  \code{\link{lm}} or, if the
#'   data is weighted,  \code{\link[survey]{svyglm}}.
#' @details "Imputation (replace missing values with estimates)". All selected
#'   outcome and predictor variables are included in the imputation, along with
#'   all \code{auxiliary.data}, excluding cases that are excluded via subset or
#'    have invalid weights, but including cases with missing values of the outcome variable.
#'   Then, cases with missing values in the outcome variable are excluded from
#'   the analysis (von Hippel 2007). See \code{\link[flipImputation]{Imputation}}.
#' @references von Hippel, Paul T. 2007. "Regression With Missing Y's: An
#'   Improved Strategy for Analyzing Multiply Imputed Data." Sociological
#'   Methodology 37:83-117. White, H. (1980), A heteroskedastic-consistent
#'   covariance matrix estimator and a direct test of heteroskedasticity.
#'   Econometrica, 48, 817-838. Long, J. S. and Ervin, L. H. (2000). Using
#'   heteroscedasticity consistent standard errors in the linear regression
#'   model. The American Statistician, 54(3): 217-224.
#' @importFrom flipData GetData CleanSubset CleanWeights EstimationData DataFormula
#' @importFrom flipU OutcomeName IsCount
#' @importFrom flipFormat Labels
#' @importFrom stats glm lm poisson quasipoisson binomial pt quasibinomial
#' @importFrom survey svyglm
#' @importFrom MASS polr glm.nb
#' @importFrom nnet multinom
#' @importFrom flipTransformations AsNumeric
#' @importFrom lmtest coeftest
#' @importFrom car hccm
#' @importFrom flipData CalibrateWeight
#' @importFrom flipTransformations CreatingBinaryDependentVariableIfNecessary
#' @export
Regression <- function(formula,
                       data = NULL,
                       subset = NULL,
                       weights = NULL,
                       missing = "Exclude cases with missing data",
                       type = "Linear",
                       robust.se = FALSE,
                       method = "default",
                       detail = TRUE,
                       m = 10,
                       seed = 12321,
                       statistical.assumptions,
                       auxiliary.data = NULL,
                       show.labels = FALSE,
                       ...)
{
    cl <- match.call()
    if(!missing(statistical.assumptions))
        stop("'statistical.assumptions' objects are not yet supported.")
    input.formula <- formula # Hack to work past scoping issues in car package: https://cran.r-project.org/web/packages/car/vignettes/embedding.pdf.
    subset.description <- if (is.null(substitute(subset))) NULL else deparse(substitute(subset))
    subset <- eval(substitute(subset), data, parent.frame())
    if (!is.null(subset) & !is.null(subset.description))
        attr(subset, "description") <- subset.description
    if (!is.null(list(...)$weights))
        weights <- list(...)$weights
    weight.name <- deparse(substitute(weights))
    weights <- eval(substitute(weights), data, parent.frame())
    data <- GetData(input.formula, data, auxiliary.data)
    if (method == "model.frame")
        return(data)
    outcome.name <- OutcomeName(input.formula)
    outcome.variable <- data[[outcome.name]]
    if(sum(outcome.name == names(data)) > 1)
        stop("The 'Outcome' variable has been selected a 'Predictor'. It must be one or the other, but may not be both.")
    if (!is.null(weights) & length(weights) != nrow(data))
        stop("'weights' and 'data' are required to have the same number of observations. They do not.")
    if (!is.null(subset) & length(subset) > 1 & length(subset) != nrow(data))
        stop("'subset' and 'data' are required to have the same number of observations. They do not.")
    if (type == "Binary Logit")
    {
        data <- CreatingBinaryDependentVariableIfNecessary(input.formula, data)
        outcome.variable <- data[[outcome.name]]
    }
    else if (type == "Ordered Logit")
        data[, outcome.name] <- ordered(outcome.variable)
    else if (type == "Multinomial Logit")
    {
        data[, outcome.name] <- factor(outcome.variable)
        if (!detail)
        {
            warning("The 'Detailed output' option has not been selected. Only detailed output is available
                  with Multinomial Logit.")
            detail = TRUE
        }
    }
    else if (IsCount(type) & !IsCount(outcome.variable))
        stopNotCount()
    else if (is.factor(outcome.variable))
    {
        WarningFactorToNumeric()
        data[, outcome.name] <- outcome.variable <- AsNumeric(outcome.variable, binary = FALSE)
    }
    row.names <- rownames(data)
    partial <- missing == "Use partial data (pairwise correlations)"
    if (robust.se & (partial | missing == "Multiple imputation"))
        stop(paste0("Robust standard errors cannot be computed with 'missing' set to ", missing, "."))
    if (robust.se & (type != "Linear" & type != "Poisson"))
        stop("Robust standard errors may only be computed using Linear or Poisson regressions.")
    if (partial)
    {
        subset <- CleanSubset(subset, nrow(data))
        unfiltered.weights <- weights <- CleanWeights(weights)
        if (type != "Linear")
            stop(paste0("'Use partial data (pairwise)' can only be used with 'type' of 'Linear'."))
        result <- list(original = LinearRegressionFromCorrelations(input.formula, data, subset,
                                                               weights, outcome.name, ...),
                   call = cl)
        result$sample.description <- result$original$sample.description
    }
    else
    {
        processed.data <- EstimationData(input.formula, data, subset, weights, missing, m = m, seed = seed)
        if (missing == "Multiple imputation")
        {
            models <- lapply(processed.data$estimation.data,
                FUN = function(x) Regression(formula,
                    data = x,
                    missing = "Error if missing data",
                    weights = processed.data$weights,
                    type = type,
                    robust.se = FALSE,
                    detail = detail,
                    show.labels = show.labels))
            final.model <- models[[1]]
            final.model$outcome.label <- if(show.labels) Labels(outcome.variable) else outcome.name
            coefs <- MultipleImputationCoefficientTable(models)
            if (show.labels)
            {
                if(type == "Multinomial Logit")
                {
                    coef.labels <- colnames(final.model$summary$coefficients)
                    kc <- length(coef.labels)
                    alt.labels <- rownames(final.model$summary$coefficients)
                    kr <- length(alt.labels)
                    rownames(coefs) <- paste(alt.labels, coef.labels[rep(1:kc, rep(kr, kc))])
                }
            }
            final.model$coefficient.table <- coefs
            final.model$summary$coefficients  <- coefs[, -4]
            final.model$coef <- final.model$original$coef <- coefs[, 1]
            final.model$missing = "Multiple imputation"
            final.model$sample.description <- processed.data$description
            return(final.model)
        }
        unfiltered.weights <- processed.data$unfiltered.weights
        .estimation.data <- processed.data$estimation.data
        n <- nrow(.estimation.data)
        if (n < ncol(.estimation.data) + 1)
            stop(warningSampleSizeTooSmall())
        post.missing.data.estimation.sample <- processed.data$post.missing.data.estimation.sample
        .weights <- processed.data$weights
        subset <-  processed.data$subset
        .formula <- DataFormula(input.formula)
        .design <- NULL
        if (is.null(.weights))
        {
            if (type == "Linear")
            {
                original <- lm(.formula, .estimation.data, model = TRUE)
                original$aic <- AIC(original)
            }
            else if (type == "Poisson" | type == "Quasi-Poisson" | type == "Binary Logit")
                original <- glm(.formula, .estimation.data, family = switch(type,
                                                                        "Poisson" = poisson,
                                                                        "Quasi-Poisson" = quasipoisson,
                                                                        "Binary Logit" = binomial(link = "logit")))
            else if (type == "Ordered Logit")
            {
                original <- polr(.formula, .estimation.data, Hess = TRUE, ...)
                original$aic <- AIC(original)
            }
            else if (type == "Multinomial Logit")
            {
                original <- multinom(.formula, .estimation.data, Hess = TRUE, trace = FALSE, maxit = 10000, ...)
                original$aic <- AIC(original)
            }
            else if (type == "NBD")
                original <- glm.nb(.formula, .estimation.data)
            else
                stop("Unknown regression 'type'.")
        }
        else
        {
            if (robust.se)
                warning(warningRobustInappropriate())
            if (type == "Linear")
            {
                .design <- weightedSurveyDesign(.estimation.data, .weights)
                original <- svyglm(.formula, .design)
                if (all(original$residuals == 0)) # perfect fit
                    original$df <- NA
                else
                {
                    assign(".design", .design, envir=.GlobalEnv)
                    aic <- extractAIC(original)
                    remove(".design", envir=.GlobalEnv)
                    original$df <- aic[1]
                    original$aic <- aic[2]
                }
            }
            else if (type == "Ordered Logit")
            {
                .estimation.data$weights <- CalibrateWeight(.weights)
                original <- polr(.formula, .estimation.data, weights = weights, Hess = TRUE, ...)
                original$aic <- AIC(original)
            }
            else if (type == "Multinomial Logit")
            {
                .estimation.data$weights <- CalibrateWeight(.weights)
                original <- multinom(.formula, .estimation.data, weights = weights, Hess = TRUE, trace = FALSE, maxit = 10000, ...)
                original$aic <- AIC(original)
            }
            else if (type == "NBD")
            {
                .estimation.data$weights <- CalibrateWeight(.weights)
                original <- glm.nb(.formula, .estimation.data, weights = weights, ...)
            }
            else
            {
                .design <- weightedSurveyDesign(.estimation.data, .weights)
                original <- switch(type,
                                   "Binary Logit" = svyglm(.formula, .design, family = quasibinomial()),
                                   "Poisson" = svyglm(.formula, .design, family = poisson()),
                                   "Quasi-Poisson" = svyglm(.formula, .design, family = quasipoisson()))
                assign(".design", .design, envir=.GlobalEnv)
                aic <- extractAIC(original)
                remove(".design", envir=.GlobalEnv)
                original$df <- aic[1]
                original$aic <- aic[2]
            }
        }
        result <- list(original = original, call = cl)
        if (!is.null(.design))
            result$design <- .design
        requireNamespace("car")
        if (missing == "Imputation (replace missing values with estimates)")
            data <- processed.data$data
        result$subset <- row.names %in% rownames(.estimation.data)
        result$sample.description <- processed.data$description
        result$n.predictors <- ncol(.estimation.data) - 1
        result$n.observations <- n
        result$estimation.data <- .estimation.data
    }
    class(result) <- "Regression"
    result$summary <- summary(result$original)
    if (robust.se)
    {
        if(is.null(weights))
        {
            robust.coef <-  coeftest(original, vcov. = hccm(result$original, type = "hc1"))
            colnames(robust.coef)[2] <- "Robust SE"
            class(robust.coef) <- "matrix" # Fixing weird bug where robust.se changes class of matrix.
            result$summary$coefficients <- robust.coef
        }
        else
            robust.se = FALSE
    }
    else if (type == "Ordered Logit" & missing != "Multiple imputation")
    {   #Tidying up Ordered Logit coefficients table to be consistent with the rest of R.
        coefs <-  result$summary$coefficients
        ps <- 2 * pt(-abs(coefs[, 3]), df = result$summary$df.residual)
        colnames(coefs)[1] <- "Estimate"
        result$summary$coefficients <- cbind(coefs, p = ps)
    }
    # Replacing the variables with their labels
    result$outcome.label <- result$outcome.name <- outcome.name
    if (show.labels)
    {
        if(type == "Multinomial Logit")
        {
            nms <- colnames(result$summary$coefficients)
            colnames(result$summary$coefficients) <- colnames(result$summary$standard.errors) <- Labels(data, nms)
        }
        else
        {
            nms <- rownames(result$summary$coefficients)
            rownames(result$summary$coefficients) <- Labels(data, nms)
        }
        result$outcome.label <- Labels(outcome.variable)
    }
    result$summary$call <- cl
    result$formula <- input.formula
    # Inserting the coefficients from the partial data.
    result$model <- data
    result$robust.se <- robust.se
    result$type = type
    result$weights <- unfiltered.weights
    result$detail <- detail
    result$show.labels <- show.labels
    result$missing <- missing
    result$terms <- result$original$terms
    result$coef <- coef(result$original)
    result$r.squared <- GoodnessOfFit(result)$value
    if (type == "Ordered Logit")
        result$coef <- c(result$coef, result$original$zeta)
    return(result)
}


#' #' @export
#' coef.Regression <- function(object, ...)
#' {
#'     object$coef
#' }

#' @param object A Regression object
#' @param The regression ethod.
notValidForPartial <- function(object, method)
{
    ms <- "Use partial data (pairwise correlations)"
    if (object$missing == ms)
        stop(paste0("'", method, "' not available when 'missing' = ",ms, "'." ))
}

#' @importFrom stats vcov
#' @export
vcov.Regression <- function(object, ...)
{
    vcov(object$original, ...)
}

#' @importFrom stats coef
#' @export
coef.Regression <- function(object, ...)
{
    coef(object$original, ...)
}

#' @importFrom survey svydesign
#' @export
weightedSurveyDesign <- function(data, weights)
{
    svydesign(ids = ~ 1, weights = weights, data = data)
}

#' @importFrom stats nobs
#' @export
nobs.Regression <- function(object, ...)
{
    object$n.observations
}
