#' \code{GrandMean}
#'
#' @param model A 'Regression'  model.
#' @details Computes the mean of the dependent variable, taking missing values, weights,
#' and the need to transform into numeric variables into account.
#' #' @importFrom flipTransformations AsNumeric
#' @importFrom flipStatistics Mean
#' @importFrom flipData Observed
#' @importFrom flipTransformations AsNumeric
#' @export
GrandMean <- function(model)
{
    if (inherits(model, "FitRegression"))
    {
        y <- Observed.FitRegression(model)
        w <- model$weights
    }
    else
    {
        if (model$type != "Linear")
            warning("GrandMean function has not been checked for non-Linear models.")
        subset <- model$subset
        y <- AsNumeric(Observed(model)[subset])
        w <- model$weights[subset]
    }
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
#' @param seed The random nmber seed.
#' @details Computes the Durbin-Watson statistic. A permutation test is used for
#' computing the p-value. Tests to a lag of 1 (two-sided).
#'
#' Durbin, J., Watson, G. S. (1950). 'Testing for Serial Correlation in Least Squares
#' Regression, I'. Biometrika 37, (pp. 3 - 4.
#' @importFrom verbs Sum
#' @export
DurbinWatson <- function(model, n.permutations = 1000, seed = 123)
{
  notValidForCrosstabInteraction(model, "DurbinWatson")
  set.seed(seed)
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
      Sum((x[-n] - x[-1]) ^ 2, remove.missing = FALSE) / Sum(x^2, remove.missing = FALSE)
    }
    .permute <- function(x) { .dW(sample(x)) }
    d <- .dW(r)
    replicates <- replicate(n.permutations, .permute(r))
    p = Sum(if (d < 2) d > replicates else d < replicates, remove.missing = FALSE) / n.permutations * 2
    result <- list(data.name = paste(deparse(model$call), sep = "\n", collapse = "\n"), statistic = c("d" = d), p.value = p, method = "Durbin-Watson statistic")
    class(result) <- "htest"
  }
  result
}


#' \code{print.DurbinWatson}
#'
#' @param x Results from the DurbinWatson test.
#' @param ... Other arugments.
#' @method print DurbinWatson
#' @export
print.DurbinWatson <- function(x, ...)
{
  cat(paste0("Durbin-Watson statistic: ", x$d, "\n"))
  cat(paste0("p-value: ", x$p, "\n"))
}


#' \code{cooks.distance.Regression}
#'
#' @param model Regression model.
#' @param ... Other arugments.
#' @importFrom stats cooks.distance
#' @method cooks.distance Regression
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
  st <- suppressWarnings(outlierTest(model, cutoff = Inf, n.max = Inf))
  if (length(st$rstudent) > 0 && !all(is.na(st$rstudent))) # length will be 0 when the fit is perfect
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
  # Bonferroni adjusted residual outlier check
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
  # If user has unusual observations, consider prompts about automated outlier removal
  if (any(c(o$max.is.high, h$max.is.high, d$max.is.high)))
  {
    # Suggest they use the automated outlier tool if they haven't or re-examine the model and amend if they have.
    if (all(model$non.outlier.data))
      message <- paste("Consider re-running the analysis using automated outlier removal with a non-zero setting to",
                       "automatically remove unusual observations that can affect the final Regression model.", message)
    else
      message <- paste("After removing a proportion of the data from the analysis, unusual observations exist in the",
                       "data. Recommend inspecting the model diagnostics and possibly increasing the automatic outlier",
                       "removal if necessary.", message)
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
  checkAcceptableModel(mod, c("lm", "glm", "polr", "svyolr"), "'vif'")
  res <- as.matrix(diagnosticTestFromCar(mod, "vif", ...))
  class(res) <- c(class(res), "visualization-selector")
  res
}

#' @importFrom car Anova
#' @importFrom flipFormat ExtractCommonPrefix
#' @importFrom flipData RemoveBackticks
#' @export
Anova.Regression <- function (mod, white.adjust = FALSE, ...)
{
    if(inherits(mod$original, "svyolr"))
        stop("'Anova' can not be computed for a 'Ordered Logit' model with weights. To compute the Anova, remove the ",
             "weights or select a different regression type such as 'Linear'")
    anova <- diagnosticTestFromCar(mod, "Anova", white.adjust = white.adjust, ...)
    # Updating labels
    if (mod$show.labels)
    {
        row.names <- RemoveBackticks(attr(anova, "row.names"))
        labels <- Labels(mod$model)
        not.residuals <- row.names != "Residuals"
        extracted <- ExtractCommonPrefix(labels[match(row.names[not.residuals], names(labels))])
        row.names[not.residuals] <- extracted$shortened.labels
        attr(anova, "row.names") <- row.names
        if (!is.na(extracted$common.prefix))
            attr(anova, "by.label") <- paste0(" by ", extracted$common.prefix)
    }
    attr(anova, "type") <- mod$type
    attr(anova, "footer") <- mod$footer
    attr(anova, "outcome.label") <- mod$outcome.label
    class(anova) <- c("Anova", class(anova))
    anova
}

#'
#' #' @importMethodsFrom car Anova
#' #' @export
#' setMethod("Anova", "missing", function "<No Object>")

#' ncvTest.Regression
#' @param model A \code{\link{Regression}} model.
#' @param ... Additional parameters to \code{\link{ncvTest}}
#' @export
ncvTest.Regression <- function(model, ...)
{
  if (!any("lm" %in% class(model$original)))
    stop(paste0("'ncvTest is not applicable for this model (it is only appropriate for a model with type of 'Linear' and no sampling weights)."))
  checkAcceptableModel(model, "lm", "'ncvTest'")
  diagnosticTestFromCar(model, "ncvTest", ...)
}


#' residualPlots.Regression
#' @param model A \code{\link{Regression}} model.
#' @param ... Additional parameters to \code{\link{residualPlots}}
#' @export
residualPlots.Regression <- function(model, ...)
{
    if(issvyglm(model))
        stop("'residualPlots' does not work with weights. Instead, run the model without weights but including the weight variable as a predictor.")
    checkAcceptableModel(model, c("glm","lm"), "'residualPlots'")
    assign(".estimation.data", model$estimation.data, envir=.GlobalEnv)
    assign(".formula", model$formula, envir=.GlobalEnv)
    t <- residualPlots(model$original)
    remove(".formula", envir=.GlobalEnv)
    remove(".estimation.data", envir=.GlobalEnv)
  t
}

issvyglm <- function(model)
{
    if (!is.null(model$original))
        model <- model$original
    inherits(model, "svyglm")
}

#' @importFrom effects allEffects
#' @export
allEffects.Regression <- function(mod, ...)
{
    has.aliased <- any(mod$summary$aliased)

    .estimation.data <-  mod$estimation.data
    assign(".estimation.data", .estimation.data, envir = .GlobalEnv)

    if (has.aliased)
        mod <- updateAliasedModel(mod)
    frml <- mod$formula

    assign(".formula", frml, envir = .GlobalEnv)
    assign(".design", mod$design, envir = .GlobalEnv)
    attach(.estimation.data, warn.conflicts = FALSE)
    mod$original$data <- .estimation.data
    effects <- allEffects(mod$original, ...)  # BreuschPagan(x$original)

    on.exit({
        if (".estimation.data" %in% search())
            detach(".estimation.data")
        if (exists(".formula", envir = .GlobalEnv))
            remove(".formula", envir = .GlobalEnv)
        if (exists(".estimation.data", envir = .GlobalEnv))
            remove(".estimation.data", envir = .GlobalEnv)
        if (exists("design", envir = .GlobalEnv))
            remove(".design", envir = .GlobalEnv)
    })
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
  model <- addBackNamesIfNecessary(model)
  infIndexPlot(model, ...)
}

#' @import car
#' @importFrom stats update formula
diagnosticTestFromCar<- function(x, diagnostic, ...)
{
    model <- x$original
    model <- addBackNamesIfNecessary(model)
    assign(".estimation.data", x$estimation.data, envir=.GlobalEnv)
    ## drop aliased/colinear variables
    ## if (any(x$summary$aliased))
    ##     model <- updateAliasedModel(x)
    frml <- formula(model)
    assign(".design", model$design, envir=.GlobalEnv)
    assign(".formula", frml, envir=.GlobalEnv)
    on.exit(
    {
        if (exists(".formula", envir = .GlobalEnv))
            remove(".formula", envir = .GlobalEnv)
        if (exists(".estimation.data", envir = .GlobalEnv))
            remove(".estimation.data", envir = .GlobalEnv)
        if (exists(".design", envir = .GlobalEnv))
            remove(".design", envir = .GlobalEnv)
    })
    txt <- paste0(diagnostic, "(model, ...)")
    eval(parse(text = txt))
}

#' Names may have been stripped from some vector elements
#' from the original model fit to reduce Regression
#' output size in reduceOutputSize(). This
#' function adds back the names to avoid errors
#' when calling diagnostics from the car package
#' @noRd
addBackNamesIfNecessary <- function(model)
{
  if (!is.null(model$residuals) && is.null(names(model$residuals)))
      names(model$residuals) <- seq_along(model$residuals)
  if (is.vector(model$y) && is.null(names(model$y)))
      names(model$y) <- seq_along(model$residuals)
  return(model)
}

#' \code{Accuracy}
#'
#' @param obj A model with an outcome variable.
#' @param subset An optional vector specifying a subset of observations.
#' @param weights An optional vector of sampling weights.
#' @details The proportion of observed values that take the same values as the predicted values.
#' Where the outcome variable in the model is not a factor and not a count, predicted values are
#' assigned to buckets as per \code{\link{ConfusionMatrix}}.
#' @importFrom verbs Sum
#' @export
Accuracy <- function(obj, subset = NULL, weights = obj$weights)
{
  cm <- ConfusionMatrix(obj, subset, weights)
  n <- Sum(cm, remove.missing = FALSE)
  correct <- Sum(diag(cm), remove.missing = FALSE)
  correct / n
}


#' @importFrom flipStatistics GoodnessOfFitPlot
#' @export
flipStatistics::GoodnessOfFitPlot


#' Goodness-of-fit plot for a Regression object
#' @param object An object for which a summary is desired.
#' @param max.points The maximum numner of points to plot.
#' @param ... Additional arguments affecting the goodness-of-fit displayed.
#' @importFrom flipData Observed
#' @importFrom stats complete.cases predict
#' @importFrom flipStandardCharts Chart
#' @export
GoodnessOfFitPlot.Regression = function(object, max.points = 1000, ...) {

    y <- cbind(Observed(object), predict(object))
    y <- y[object$subset & complete.cases(y), ]

    correlation <- cor(y, method = "spearman")

    # Sample randomly if too many rows
    set.seed(1066)
    if (nrow(y) > max.points)
        y <- y[sample(nrow(y), max.points), ]

    title <- paste0(object$type, " Regression - Shepard Diagram - Rank correlation: ", sprintf("%1.2f%%", 100 * correlation[2, 1]))
    chart <- Chart(y = y,
                   type = "Scatterplot",
                   title = title,
                   x.title = "Observed",
                   y.title = "Fitted")
    class(chart) <- c(class(chart), "visualization-selector")
    chart
}

#' for categorical vars, need to take care to get formula term from the
#' regr. coef. names
#' @importFrom stats update formula
#' @noRd
updateAliasedModel <- function(x)
{
    aliased.var <- x$summary$aliased
    fterms <- attr(x$terms, "term.labels")
    ## continuous variable name will match exactly, categorical variable has names
    ## "varname levelname1", "varname levelname2", ...
    patt <- paste0("^", fterms, "$|^", fterms, " ")
    aliased.names <- names(aliased.var)[aliased.var]
    matches <- vapply(patt, grepl, logical(length(aliased.names)), x = aliased.names)
    if (is.matrix(matches))
        matches <- apply(matches, 2, any)

    aterms <- fterms[matches]
    upd.frml <- paste0("~.-`", paste(aterms, collapse = "`-`"), "`")
    frml <- update(formula(x), upd.frml)
    .estimation.data <- x$estimation.data
    attach(.estimation.data, warn.conflicts = FALSE)
    on.exit(detach(.estimation.data))
    x$anova <- x$anova[c(fterms[!matches], "Residuals"), ]
    x$formula <- frml
    x$summary$aliased <- aliased.var[!aliased.var]
    x$original <- suppressMessages(suppressWarnings(update(x$original, frml)))

    return(x)
}
