#' \code{GrandMean}
#'
#' @param model A 'Regression'  model.
#' @details Computes the mean of the dependent variable, taking missing values, weights,
#' and the need to transform into numeric variables into account.
#' #' @importFrom flipTransformations AsNumeric
#' @importFrom flipStatistics Mean
#' @importFrom flipTransformations AsNumeric
#' @export
GrandMean <- function(model)
{
    if (model$type != "Linear")
        warning("GrandMean function has not been checked for non-Linear models.")
    subset <- model$subset
    y <- AsNumeric(Observed(model)[subset])
    w <- model$weights[subset]
    Mean(y, w)
}





numberParameters <- function(x)
{
  if("Regression" %in% class(x))
    return(x$n.predictors + 1)
  length(x$coefficients)
}

numberObservations <- function(x)
{
  if("Regression" %in% class(x))
    return(x$n.observations)
  attr(logLik(x), "nobs")
}

#' \code{DurbinWatson}
#'
#' @param model A 'Regression'  model.
#' @param n.permutations Number of permutations used in computing the p-value.
#' @details "Computes the Durbin-Watson statistic. A permutation test is used for
#' computing the p-value. Tests to a lag of 1 (two-sided).
#'
#' Durbin, J., Watson, G. S. (1950). 'Testing for Serial Correlation in Least Squares
#' Regression, I'. Biometrika 37, (pp. 3 - 4.
#' @export
DurbinWatson <- function(model, n.permutations = 1000)
{
  set.seed(123)
  residuals <- resid(model)
  if("Regression" %in% class(model))
    residuals <- residuals[model$subset]
  r <- residuals[!is.na(residuals)]
  n <- length(residuals)
  if (n <= 2)
  {
    d = NA
    p = NA

  }
  else
  {
    .dW <- function(x)
    {
      sum((x[-n] - x[-1]) ^ 2) / sum(x^2)
    }
    .permute <- function(x) { .dW(sample(x)) }
    d <- .dW(r)
    replicates <- replicate(n.permutations, .permute(r))
    p = sum(if (d < 2) d > replicates else d < replicates) / n.permutations * 2
    result <- list(data.name = paste(deparse(model$call), sep = "\n", collapse = "\n"), statistic = c("d" = d), p.value = p, method = "Durbin-Watson statistic")
    class(result) <- "htest"
  }
  result
}

#' @export
print.DurbinWatson <- function(x, ...)
{
  cat(paste0("Durbin-Watson statistic: ", x$d, "\n"))
  cat(paste0("p-value: ", x$p, "\n"))
}


#' @importFrom stats cooks.distance
#' @export
cooks.distance.Regression <- function(model, ...)
{
  checkAcceptableModel(model, c("lm", "glm"),"'cooks.distance'")
  cooks.distance(model$original)
}

#' @importFrom stats df.residual
#' @export
df.residual.Regression <- function(object, ...)
{
  df.residual(object$original)
}

#' @export
df.residual.multinom <- function(object, ...)
{
  sum(!is.na(object$fitted.values[, 1])) - length(object$coefnames)
}

#' \code{CooksDistance}
#'
#' @param model A 'Regression' model.
#' @details Computes Cook's distance and a threshold value.

#' @importFrom stats quantile qf cooks.distance
#' @importFrom flipFormat FormatAsReal
#' @export
CooksDistance <- function(model)
{
  cat("Cook's distance:\n")
  d <- cooks.distance(model)
  print(structure(zapsmall(quantile(d, na.rm = TRUE), 3), names = c("Min", "1Q", "Median", "3Q", "Max")), digits = 3)
  k <- numberParameters(model)
  n <- numberObservations(model)
  cutoff <- qf(0.5, k, n - k)
  max.d <- max(d, na.rm = TRUE)
  max.is.high <- max.d > cutoff
  description = paste0("The largest Cook's distance is ",
                       FormatAsReal(max.d, 3), ", which is ",
                       if(max.is.high) "" else "not ",
                       "higher than the threshhold of ",
                       FormatAsReal(cutoff, 3), " (the median of the F(k=",
                       k,",n-k=", n - k, ") distribution).\n")
  cat(description)
  cat("\n")
  invisible(list(max.is.high = max.is.high, d = d, cutoff = cutoff, description = description))
}

#' \code{HatValues}
#'
#' @param model A 'Regression' model.
#' @details Computes hat values and a threshold value.
#' @importFrom stats hatvalues
#' @importFrom flipFormat FormatAsReal
#' @export
HatValues <- function(model)
{
  cat("Hat values:\n")
  d <- hatvalues(model)
  print(structure(zapsmall(quantile(d, na.rm = TRUE), 3), names = c("Min", "1Q", "Median", "3Q", "Max")), digits = 3)
  k <- numberParameters(model)
  n <- numberObservations(model)
  cutoff <- 2 * (k + 1) / n
  max.d <- max(d, na.rm = TRUE)
  max.is.high <- max.d > cutoff
  description = paste0("The largest hat value is ",
                       FormatAsReal(max.d, 3), ", which is ",
                       if(max.is.high) "" else "not ",
                       "higher than the threshhold of ",
                       FormatAsReal(cutoff, 3), " = 2 * (k + 1) / n.\n")
  cat(description)
  cat("\n")
  invisible(list(max.is.high = max.is.high, d = d, cutoff = cutoff, description = description))
}

#' \code{OutlierTest}
#'
#' @param model A 'Regression' model.
#' @details Computes studentized residuals.
#' @importFrom car outlierTest
#' @importFrom stats quantile
#' @importFrom flipFormat FormatAsReal FormatAsPValue

#' @export
OutlierTest <- function(model)
{
  cat("Studentized residuals:\n")
  st <- outlierTest(model, cutoff = Inf, n.max = Inf)
  if (length(st$rstudent) > 0) # length will be 0 when the fit is perfect
  {
      qs <- quantile(st$rstudent)
      print(structure(zapsmall(qs, 3), names = c("Min", "1Q", "Median", "3Q", "Max")), digits = 3)
      min.p <- min(st$bonf.p)
      max.is.high <- min.p <= 0.05
      mx <- if(abs(qs[1]) < qs[5]) qs[5] else qs[1]
      description = paste0("The largest studentized residual is ",
                           FormatAsReal(mx, 3), ", which is ",
                           if(max.is.high) "" else "not ",
                           "significant, with a Bonferroni-corrected p-value of ",
                           FormatAsPValue(min.p), ".\n")
      cat(description)
      cat("\n")
      invisible(list(max.is.high = max.is.high, outlierTest = st, description = description))
  }
  else
      invisible(list(max.is.high = FALSE, outlierTest = st))
}


#' @export
outlierTest.Regression <- function(model, ...)
{
  checkAcceptableModel(model, c("glm","lm"), "'outlierTest'")
  diagnosticTestFromCar(model, "outlierTest", ...)
}



#' \code{UnusualObservations}
#'
#' @param model A 'Regression'  model.
#' @details Computes studentized residuals, Cook's distance, and hat values, and reports if any are above the threshhold values.
#' @export
UnusualObservations <- function(model)
{
  message <- "";
  o <- OutlierTest(model)
  if (o$max.is.high)
    message <- o$description
  # Hat values.
  h <- HatValues(model)
  if (h$max.is.high)
    message <- paste0(message, (if(message == "") "" else "\n"), h$description)
  # Cook's distance
  d <- CooksDistance(model)
  if (d$max.is.high)
    message <- paste0(message, (if(message == "") "" else "\n"), d$description)
  if (message == ""){
    cat("No outliers have been identified.\n")
    return(invisible(NULL))
  }
  paste("Unusual observations detected.", message)
}


checkAcceptableModel <- function(x, classes, diagnostic, exclude.partial.data = TRUE)
{
  if (exclude.partial.data & x$missing == "Use partial data (pairwise correlations)")
    stop(paste0("'", diagnostic, "' is not computed for model that are computed using 'Use partial data (pairwise correlations)'"))
  if (!any(classes %in% class(x$original)))
    stop(paste0(diagnostic, " is not computed for models of this type or class."))
}

#' @export
vif.Regression <- function (mod, ...)
{
  checkAcceptableModel(mod, c("lm", "glm"), "'vif'")
  diagnosticTestFromCar(mod, "vif", ...)
}

#' @export
ncvTest.Regression <- function(model, ...)
{
  if (!any("lm" %in% class(model$original)))
    stop(paste0("'ncvTest is not applicable for models with sampling weights."))
  checkAcceptableModel(model, "lm", "'ncvTest'")
  diagnosticTestFromCar(model, "ncvTest", ...)
}


#' @export
residualPlots.Regression <- function(model, ...)
{
    if(inherits(model$original, "svyglm"))
        stop("'residualPlots' does not work with weights. Instead, run the model without weights but including the weight variable as a predictor.")
    checkAcceptableModel(model, c("glm","lm"), "'residualPlots'")
    assign(".estimation.data", model$estimation.data, envir=.GlobalEnv)
    assign(".formula", model$formula, envir=.GlobalEnv)
    t <- residualPlots(model$original)
    remove(".formula", envir=.GlobalEnv)
    remove(".estimation.data", envir=.GlobalEnv)
  t
}


#' @importFrom effects allEffects
#' @export
allEffects.Regression <- function(model, ...)
{
    assign(".estimation.data", model$estimation.data, envir=.GlobalEnv)
    assign(".formula", model$formula, envir=.GlobalEnv)
    assign(".design", model$design, envir=.GlobalEnv)
    attach(.estimation.data)
    effects <- allEffects(model$original, ...)#BreuschPagan(x$original)
    detach(.estimation.data)
    remove(".design", envir=.GlobalEnv)
    remove(".formula", envir=.GlobalEnv)
    remove(".estimation.data", envir=.GlobalEnv)
    effects
}

#' @export
influenceIndexPlot.Regression <- function(model, ...)
{
  checkAcceptableModel(model, c("glm","lm"), "'influenceIndexPlot'")
  diagnosticTestFromCar(model, "influenceIndexPlot", ...)
}

#' @importFrom stats hatvalues
#' @export
hatvalues.Regression <- function(model, ...)
{
  hatvalues(model$original)
}

#' @export
influencePlot.Regression <- function(model, ...)
{
  checkAcceptableModel(model, c("glm","lm"), "'influencePlot'")
  diagnosticTestFromCar(model, "influencePlot", ...)
}


#' \code{infIndexPlot.Regression}
#'
#' @param model A 'Regression'  model.
#' @param ... Other parameters for \code{infIndexPlot}.
#' @importFrom car infIndexPlot
#' @export
infIndexPlot.Regression <- function(model, ...)
{
  checkAcceptableModel(model, c("glm","lm"), "'influenceIndexPlot'")
  model <- model$original
  infIndexPlot(model, ...)
}



#' @import car
diagnosticTestFromCar<- function(x, diagnostic, ...)
{
  model <- x$original
  assign(".estimation.data", x$estimation.data, envir=.GlobalEnv)
  assign(".formula", formula(model), envir=.GlobalEnv)
  txt <- paste0(diagnostic, "(model, ...)")
  t <- eval(parse(text = txt))
  remove(".formula", envir=.GlobalEnv)
  remove(".estimation.data", envir=.GlobalEnv)
  t
}


#' \code{ConfusionMatrixFromVariables}
#'
#' @param observed A \code{factor}.
#' @param predicted A \code{factor}.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @details A contingency table showing the observed versus predicted values from a model.
#' @importFrom flipU IsCount
#' @importFrom stats xtabs
#' @export
ConfusionMatrixFromVariables <- function(observed, predicted, subset = NULL, weights = NULL)
{
  if(IsCount(observed))
    predicted <- floor(predicted)
  #     observed <- factor(observed)
  # if(!is.factor(observed))
  #     predicted <- factor(predicted)
  # levels.observed <- levels(observed)
  # n <- length(levels)
  # # Ensuring that the observed and predicted values line up)
  # predicted <- factor(match(predicted, levels.observed))
  # levels(predicted) <- levels.observed
  if (is.null(weights))
  {
    if (is.null(subset))
      return(xtabs(~ observed + predicted))
    else
      return(xtabs(~ observed + predicted, subset = subset))
  }
  else
  {
    if (is.null(subset))
      return(xtabs(weights ~ observed + predicted))
    else
      return(xtabs(weights ~ observed + predicted, subset = subset))
  }
}


#' \code{ConfusionMatrixFromVariablesLinear}
#'
#' @param observed A \code{factor}.
#' @param predicted A \code{factor}.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#'
#' @details A contingency table showing the observed versus predicted values from a model, for linear models.
#' Prediced values are assigned the value of the closest observed value.
#' @export
ConfusionMatrixFromVariablesLinear <- function(observed, predicted, subset = NULL, weights = NULL)
{
  if (is.factor(observed))
    observed <- as.numeric(observed)
  if (is.factor(predicted))
    predicted <- as.numeric(predicted)
  unique.observed <- unique(observed)
  unique.observed <- unique.observed[!is.na(unique.observed)]
  predicted.na <- is.na(predicted)
  if(any(predicted.na))
    predicted[predicted.na] <- -Inf
  predicted <- sapply(predicted, function(x) unique.observed[which.min(abs(unique.observed - x))])
  predicted[predicted.na] <- NA
  #levels(observed) <- paste("Observed", levels(observed))
  #levels(predicted) <- paste("Predicted", levels(predicted))
  ConfusionMatrixFromVariables(observed, predicted, subset, weights)
}




#' \code{ConfusionMatrix}
#'
#' @param obj A model with an outcome variable.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @details The proportion of observed values that take the same values as the predicted values.
#' Where the outcome
#' variable in the model is not a factor and not a count, predicted values are assigned to the closest observed
#' value.
#' @importFrom stats predict
#' @importFrom methods is
#' @export
ConfusionMatrix <- function(obj, subset = NULL, weights = NULL)
{
  observed <- Observed(obj)
  predicted <- predict(obj)

  if (is(obj,"Regression"))
  {
      if(obj$type == "Linear")
        return(ConfusionMatrixFromVariablesLinear(observed, predicted, subset, weights))
  }
  return(ConfusionMatrixFromVariables(observed, predicted, subset, weights))
}


#' \code{Accuracy}
#'
#' @param obj A model with an outcome variable.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @details The proportion of observed values that take the same values as the predicted values.
#' Where the outcome
#' variable in the model is not a factor and not a count, predicted values are assigned to the closest observed
#' value.
#' @export
Accuracy <- function(obj, subset = NULL, weights = NULL)
{
  cm <- ConfusionMatrix(obj, subset, weights)
  n <- sum(cm)
  rnames <- rownames(cm)
  cnames <- colnames(cm)
  cm <- cm[rnames %in% cnames, cnames %in% rnames]
  correct <- sum(diag(cm))
  correct / n
}







#' #' \code{BreuschPagan} Breusch-Pagan test for non-constant variance.
#' #'
#' #' @param mod An object of class \code{\link{Regression}}.
#' #' @param show.warnings Returns a warning if the sample size is less than 4.
#' #' @details Weights are ignored when conducting the test.
#' #' @export
#' BreuschPagan <- function(Regression.object, show.warnings = TRUE)#, var.formula)
#' {
#'     if (class(Regression.object)[1] != "Regression")
#'         return(car::ncvTest(Regression.object))
#'     #Modified from car::ncvTest, to addess different size of input data.
#'     subset <- Regression.object$subset
#'     if(sum(subset) < 4)
#'     {
#'         if (show.warnings)
#'             warning("Sample size too small for Breusch-Pagan test.")
#'         return(list(ChiSquare = NA, Df = NA, p = 1,
#'             test = "Breusch-Pagan test of Non-constant Variance"))
#'     }
#'     residuals <- residuals(Regression.object)[subset]
#'     fitted.values <- fitted.values(Regression.object)[subset]
#'     squared.residuals <- residuals^2
#'     r.squared <- 1 - var(residuals) / var(fitted.values)
#'     if (r.squared> 0.9999) {
#'         Chisq <- 0.0
#'     }
#'     else
#'     {
#'         U <- squared.residuals / mean(squared.residuals)#mean.squared.error#sum(squared.residuals)
#'         mod <- lm(U ~ fitted.values)#, subset = Regression.object$subset)
#'         SS <- anova(mod)$"Sum Sq"
#'         RegSS <- sum(SS) - SS[length(SS)]
#'         Chisq <- RegSS/2
#'     }
#'     result <- list(#formula = var.formula, formula.name = "Variance",
#'         ChiSquare = Chisq, Df = 1, p = pchisq(Chisq, 1, lower.tail = FALSE),
#'         test = "Breusch-Pagan test of Non-constant Variance")
#'     class(result) <- "chisqTest"
#'     result
#' }
#'
#'
#'
#' @describeIn GoodnessOfFit  Goodness-of-fit for a Regression object. Computed as the \eqn{R^2} statistic.
#' With factors, the index value of the factor is used. With unordered factors, this will often be
#' grieviously-downward biased.
#' @importFrom flipTransformations UnclassIfNecessary
#' @importFrom stats predict lm summary.lm ts
#' @export
GoodnessOfFit.Regression = function(object, digits = max(3L, getOption("digits") - 3L), ...) {
    if (object$missing == "Use partial data (pairwise correlations)")
        r2 <- object$original$lm.cov$R2
    else if (object$type == "Linear" & is.null(object$weights))
        r2 <- object$summary$r.square
    else
    {
        predicted <- UnclassIfNecessary(predict(object)[object$subset])
        if (sd(predicted) == 0)
            r2 <- 0
        else
        {
            observed <- UnclassIfNecessary(Observed(object)[object$subset])
            if (is.null(object$weights))
                r2 <- cor(observed, predicted, use = "complete.obs") ^ 2
            else
            {
                wgts <- object$weights[object$subset]
                r2 <- summary(lm(predicted ~ observed, weights = wgts))$r.square

            }
        }
    }
    names(r2) <- "R-squared"
    description <- list("Variance explained: ",
                        formatC(100 * r2, digits = digits),
                        "%\n(R-squared * 100)")
    GoodnessOfFitInternal(r2, description, object$call)
}

