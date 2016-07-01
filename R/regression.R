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
#'   modification of White's (1980) estimator (Long and Ervin, 2000).
#' @param detail More detailed outputs.
#' @param method The method to be used; for fitting. This will only do something if
#' method = "model.frame", which returns the model frame.
#' @param m The number of imputed samples, if using multiple imputation.
#' @param seed The random number seed used in imputation.
#' @param ... Additional argments to be past to  \code{\link{lm}} or, if the
#'   data is weighted,  \code{\link[survey]{svyglm}}.
#' @details "Imputation (replace missing values with estimates)". All selected
#'   outcome and predictor variables are included in the imputation, excluding
#'   cases that are excluded via subset or have invalid weights, but including
#'   cases with missing values of the outcome variable.
#'   Then, cases with missing values in the outcome variable are excluded from
#'   the analysis (von Hippel 2007). See \code{\link[flipImputation]{Imputation}}.
#' @references von Hippel, Paul T. 2007. "Regression With Missing Y's: An
#'   Improved Strategy for Analyzing Multiply Imputed Data." Sociological
#'   Methodology 37:83-117. White, H. (1980), A heteroskedastic-consistent
#'   covariance matrix estimator and a direct test of heteroskedasticity.
#'   Econometrica, 48, 817-838. Long, J. S. and Ervin, L. H. (2000). Using
#'   heteroscedasticity consistent standard errors in the linear regression
#'   model. The American Statistician, 54(3): 217-224.
#' @importFrom flipData GetData CleanSubset CleanWeights EstimationData
#' @importFrom flipU OutcomeName IsCount
#' @importFrom flipFormat GetLabels
#' @importFrom stats glm lm poisson quasipoisson binomial pt quasibinomial
#' @importFrom survey svyglm
#' @importFrom MASS polr glm.nb
#' @importFrom nnet multinom
#' @importFrom lmtest coeftest
#' @importFrom car hccm
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
                       ...)
{
  cl <- match.call()
  .formula <- formula # Hack to work past scoping issues in car package: https://cran.r-project.org/web/packages/car/vignettes/embedding.pdf.
  subset.description <- if (is.null(substitute(subset))) NULL else deparse(substitute(subset))
  subset <- eval(substitute(subset), data, parent.frame())
  if (!is.null(subset.description))
       attr(subset, "description") <- subset.description
  weights <- eval(substitute(weights), data, parent.frame())
  data <- GetData(.formula, data)
  if (method == "model.frame")
    return(data)
  mt <- attr(data, "terms")
  outcome.name <- OutcomeName(.formula)
  outcome.variable <- data[[outcome.name]]
  if (!is.null(weights) & length(weights) != nrow(data))
    stop("'weights' and 'data' are required to have the same number of observations. They do not.")
  if (!is.null(subset) & length(subset) > 1 & length(subset) != nrow(data))
    stop("'subset' and 'data' are required to have the same number of observations. They do not.")
  if (type == "Binary Logit")
  {
    data <- CreatingBinaryDependentVariableIfNecessary(.formula, data)
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
    data[, outcome.name] <- outcome.variable <- unclass(outcome.variable)
  }
  row.names <- rownames(data)
  if (robust.se & (missing == "Use partial data (pairwise correlations)" | missing == "Multiple imputation"))
      stop(paste0("Robust standard errors cannot be computed with 'missing' set to ", missing, "."))
  if (missing == "Use partial data (pairwise correlations)")
  {
    subset <- CleanSubset(subset, nrow(data))
    unfiltered.weights <- weights <- CleanWeights(weights)
    if (type != "Linear")
      stop(paste0("'Use partial data (pairwise)' can only be used with 'type' of 'Linear'."))
    result <- list(original = LinearRegressionFromCorrelations(.formula, data, subset,
                                                               weights, outcome.name, ...),
                   call = cl)
    result$sample.description <- result$original$sample.description
  }
    else
    {
        processed.data <- EstimationData(.formula, data, subset, weights, missing, m = m, seed = seed)
        if (missing == "Multiple imputation")
        {
            models <- lapply(processed.data$estimation.data,
                FUN = function(x) Regression(formula,
                    data = x,
                    missing = "Error if missing data",
                    weights = processed.data$weights,
                    type = type,
                    robust.se = FALSE,
                    detail = detail))
            final.model <- models[[1]]
            coefs <- MultipleImputationCoefficientTable(models)
            final.model$coefficient.table <- coefs
            final.model$summary$coefficients  <- coefs[, -4]
            final.model$original$coef <- coefs[, 1]
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
        weights <- processed.data$weights
        subset <-  processed.data$subset
        if (is.null(weights))
        {
          if (type == "Linear")
            original <- lm(.formula, .estimation.data, model = TRUE)
          else if (type == "Poisson" | type == "Quasi-Poisson" | type == "Binary Logit")
            original <- glm(.formula, .estimation.data, family = switch(type,
                                                                        "Poisson" = poisson,
                                                                        "Quasi-Poisson" = quasipoisson,
                                                                        "Binary Logit" = binomial(link = "logit")))
          else if (type == "Ordered Logit")
            original <- polr(.formula, .estimation.data, Hess = TRUE, ...)
          else if (type == "Multinomial Logit")
            original <- multinom(.formula, .estimation.data, Hess = TRUE, trace = FALSE, maxit = 10000, ...)
          else if (type == "NBD")
            original <- glm.nb(.formula, .estimation.data)
          else
            stop("Unknown regression 'type'.")

          if (robust.se)
          {
            original$robust.coefficients <- coeftest(original, vcov. = hccm(original, type = "hc1"))
            colnames(original$robust.coefficients)[2] <- "Robust SE"
          }
        }
        else
        {
          if (robust.se)
            warningRobustInappropriate()
          if (type == "Linear")
            original <- svyglm(.formula, weightedSurveyDesign(.estimation.data, weights))
          else if (type == "Ordered Logit")
          {
            .estimation.data$weights <- weights
            original <- polr(.formula, .estimation.data, weights = weights, Hess = TRUE, ...)
          }
          else if (type == "Multinomial Logit")
          {
            .estimation.data$weights <- weights
            original <- multinom(.formula, .estimation.data, weights = weights, Hess = TRUE, trace = FALSE, maxit = 10000, ...)
          }
          else if (type == "NBD")
          {
            .estimation.data$weights <- weights
            original <- glm.nb(.formula, .estimation.data, weights = weights, ...)
          }
          else
          {
            wgt.svy.des <- weightedSurveyDesign(.estimation.data, weights)
            original <- switch(type,
                               "Binary Logit" = svyglm(.formula, wgt.svy.des, family = quasibinomial()),
                               "Poisson" = svyglm(.formula, wgt.svy.des, family = poisson()),
                               "Quasi-Poisson" = svyglm(.formula, wgt.svy.des, family = quasipoisson()))
            assign("wgt.svy.des", wgt.svy.des, envir=.GlobalEnv)
            original$aic <- AIC(original)[2]
            remove("wgt.svy.des", envir=.GlobalEnv)

          }
        }
        result <- list(original = original, call = cl)
        requireNamespace("car")
        if (missing == "Imputation (replace missing values with estimates)")
          data <- processed.data$data
        result$subset <- row.names %in% rownames(.estimation.data)
        result$sample.description <- processed.data$description
        result$n.predictors <- ncol(.estimation.data) - 1
        result$n.observations <- n
        result$estimation.data <- .estimation.data
  }
  result$summary <- summary(result$original)
  # Replacing the variables with their labelsz$s
  rownames(result$summary$coefficients) <- GetLabels(rownames(result$summary$coefficients), data)
  result$summary$call <- cl
  result$formula <- .formula
  # Inserting the coefficients from the partial data.
  result$model <- data #over-riding the data that is automatically saved (which has had missing values removed).
  result$robust.se <- robust.se
  class(result) <- "Regression"
  result$type = type
  result$weights <- unfiltered.weights
  result$detail <- detail
  result$outcome.name <- outcome.name
  result$missing <- missing
  result$terms <- mt
  result$coef <- result$original$coef
  # if (type != "Multinomial Logit")
  #     result$residuals <- unclassIfNecessary(outcome.variable) -
  #     unclassIfNecessary(result$predicted.values)#Note this occurs after summary, to avoid stuffing up summary, but before Breusch Pagan, for the same reason.
  if (robust.se)
    result$summary$coefficients <- result$original$robust.coefficients
  return(result)
  }


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
weightedSurveyDesign <- function(data, weights)
{
    svydesign(ids = ~ 1, weights = weights, data = data)
}


