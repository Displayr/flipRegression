?numberParameters <- function(x)
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
termsToMf <- function(model, terms)
{   # This is a modification of the original function from car. It hacks around problems
    #with svyglm
    gform <- function(formula)
    {
        if (is.null(formula))
        {
            return(list(vars = formula, groups = NULL))
        }
        has.response <- length(formula) == 3
        rhs <- if (has.response) formula[[3]] else formula[[2]]
        if (length(rhs) == 1)
        {
            return(list(vars = formula, groups = NULL))
        }
        if (length(rhs) != 3)
            stop("incorrectly formatted 'terms' argument")
        if (deparse(rhs[[1]] == "|"))
        {
            if (length(rhs[[3]]) > 1)
                stop("Only one conditioning variable is permitted")
            groups <- as.formula(paste("~", deparse(rhs[[3]])))
            rhs <- rhs[[2]]
        }
        else
            groups <- NULL
        vars <- as.formula(paste("~ ", deparse(rhs)))
        list(vars = vars, groups = groups)
    }
    terms <- gform(as.formula(terms))
    mf.vars <- .estimation.data
    if (!is.null(terms$groups)) {
        mf.groups <- try(update(model, terms$groups, method = "model.frame"),
            silent = TRUE)
        if (class(mf.groups) == "try-error")
            mf.groups <- try(update(model, terms$groups, method = "model.frame",
                data = model.frame(model)), silent = TRUE)
        if (class(mf.groups) == "try-error")
            stop("argument 'terms' not interpretable.")
    }
    else {
        mf.groups <- NULL
    }
    list(mf.vars = mf.vars, mf.groups = mf.groups)
}

residualPlot.glm <- function (model, variable = "fitted", type = "pearson", plot = TRUE,
          quadratic = FALSE, smoother = loessLine, smoother.args = list(k = 3),
          ...)
{
    residualPlot.default(model, variable = variable, type = type,
                         plot = plot, quadratic = quadratic, smoother = smoother,
                         smoother.args = smoother.args, ...)
}

residualPlot.lm <- function (model, ...)
{
    residualPlot.default(model, ...)
}

residualPlot.default <- function (model, variable = "fitted", type = "pearson", groups,
          plot = TRUE, linear = TRUE, quadratic = if (missing(groups)) TRUE else FALSE,
          smoother = NULL, smoother.args = list(), col.smooth = palette()[3],
          labels, id.method = "r", id.n = if (id.method[1] == "identify") Inf else 0,
          id.cex = 1, id.col = palette()[1], col = palette()[1], col.quad = palette()[2],
          pch = 1, xlab, ylab, lwd = 1, lty = 1, grid = TRUE, key = !missing(groups),
          ...)
{
    string.capitalize <- function(string) {
        paste(toupper(substring(string, 1, 1)), substring(string,
                                                          2), sep = "")
    }
    if (missing(labels))
        labels <- names(residuals(model)[!is.na(residuals(model))])
    ylab <- if (!missing(ylab))
        ylab
    else paste(string.capitalize(type), "residuals")
    column <- match(variable, names(model$model))
    if (is.na(column) && variable != "fitted")
        stop(paste(variable, "is not a regressor in the mean function"))
    horiz <- if (variable == "fitted")
        predict(model)
    else model$model[[column]]
    lab <- if (variable == "fitted") {
        if (inherits(model, "glm"))
            "Linear Predictor"
        else "Fitted values"
    }
    else variable
    lab <- if (!missing(xlab))
        xlab
    else lab
    if (class(horiz)[1] == "ordered")
        horiz <- factor(horiz, ordered = FALSE)
    ans <- if (inherits(horiz, "poly")) {
        horiz <- horiz[, 1]
        lab <- paste("Linear part of", lab)
        c(NA, NA)
    }
    else if (inherits(horiz, "matrix")) {
        horiz <- try(predict(model, type = "terms"), silent = TRUE)
        if (class(horiz) == "try-error")
            stop("Could not plot spline terms")
        warning("Splines replaced by a fitted linear combination")
        horiz <- horiz[, variable]
        c(NA, NA)
    }
    else if (inherits(horiz, "factor"))
        c(NA, NA)
    else residCurvTest.glm(model, variable)
    if (!missing(groups)) {
        if (is.data.frame(groups)) {
            groups.name <- names(groups)[1]
            groups <- groups[, 1, drop = TRUE]
        }
        else groups.name <- deparse(substitute(groups))
        groups <- if (class(groups)[1] == "factor")
            groups
        else factor(groups, ordered = FALSE)
        if (key) {
            mar3 <- 1.1 + length(levels(groups))
            op <- par(mar = c(5.1, 4.1, mar3, 2.1))
            on.exit(par(op))
        }
        colors <- if (length(col) >= length(levels(groups)))
            col
        else palette()
        col <- colors[as.numeric(groups)]
        pchs <- if (length(pch) >= length(levels(groups)))
            pch
        else 1:length(levels(groups))
        pch <- pchs[as.numeric(groups)]
    }
    theResiduals <- switch(type, rstudent = rstudent(model),
                           rstandard = rstandard(model), residuals(model, type = type))
    if (plot == TRUE) {
        if (class(horiz) == "factor") {
            idm <- if (is.list(id.method)) {
                lapply(id.method, function(x) if (x[1] == "xy")
                    "y"
                    else x)
            }
            else {
                if (id.method[1] == "xy")
                    "y"
            }
            Boxplot(theResiduals, horiz, xlab = lab, ylab = ylab,
                    labels = labels, id.method = idm, id.n = id.n,
                    id.cex = id.cex, id.col = id.col, ...)
            abline(h = 0, lty = 2)
        }
        else {
            plot(horiz, theResiduals, xlab = lab, ylab = ylab,
                 type = "n", ...)
            if (grid) {
                grid(lty = 1, equilogs = FALSE)
                box()
            }
            points(horiz, theResiduals, col = col, pch = pch,
                   ...)
            if (linear) {
                if (missing(groups)) {
                    abline(h = 0, lty = 2, lwd = 2)
                }
                else {
                    for (g in 1:length(levels(groups))) try(abline(lm(theResiduals ~
                                                                          horiz, subset = groups == levels(groups)[g]),
                                                                   lty = 2, lwd = 2, col = colors[g]), silent = TRUE)
                }
            }
            if (quadratic) {
                new <- seq(min(horiz), max(horiz), length = 200)
                if (missing(groups)) {
                    if (length(unique(horiz)) > 2) {
                        lm2 <- lm(theResiduals ~ poly(horiz, 2))
                        lines(new, predict(lm2, list(horiz = new)),
                              lty = 1, lwd = 2, col = col.quad)
                    }
                }
                else {
                    for (g in 1:length(levels(groups))) {
                        if (length(unique(horiz)) > 2) {
                            lm2 <- lm(theResiduals ~ poly(horiz, 2),
                                      subset = groups == levels(groups)[g])
                            lines(new, predict(lm2, list(horiz = new)),
                                  lty = 1, lwd = 1.5, col = colors[g])
                        }
                    }
                }
            }
            if (is.function(smoother))
                if (missing(groups)) {
                    smoother(horiz, theResiduals, col.smooth, log.x = FALSE,
                             log.y = FALSE, spread = FALSE, smoother.args = smoother.args)
                }
            else for (g in 1:length(levels(groups))) {
                sel <- groups == levels(groups)[g]
                smoother(horiz[sel], theResiduals[sel], colors[g],
                         log.x = FALSE, log.y = FALSE, spread = FALSE,
                         smoother.args = smoother.args)
            }
            if (key & !missing(groups)) {
                items <- paste(groups.name, levels(groups), sep = " = ")
                plotArrayLegend("top", items = items, col.items = colors,
                                pch = pchs)
            }
            showLabels(horiz, theResiduals, labels = labels,
                       id.method = id.method, id.n = id.n, id.cex = id.cex,
                       id.col = id.col)
        }
    }
    invisible(ans)
}
#' @export
residualPlots.Regression <- function(model, ...)
{
  checkAcceptableModel(model, c("glm","lm"), "'residualPlots'")
  assign(".estimation.data", model$estimation.data, envir=.GlobalEnv)
  assign(".formula", model$formula, envir=.GlobalEnv)
  #assign(".weights", model$weights[model$subset], envir=.GlobalEnv)
  assign(".design", model$design, envir=.GlobalEnv)
  #attach(.estimation.data)
  t <- suppressWarnings(residualPlotsT(model$original))
  #detach(.estimation.data)
  remove(".design", envir=.GlobalEnv)
  #remove(".weights", envir=.GlobalEnv)
  remove(".formula", envir=.GlobalEnv)
  remove(".estimation.data", envir=.GlobalEnv)
  t
}

residCurvTest.glm <- function (model, variable)
{
    if (variable == "fitted")
        c(NA, NA)
    else {
        if (is.na(match(variable, attr(model$terms, "term.labels"))))
            stop(paste(variable, "is not a term in the mean function"))
        else {
            newmod <- paste(" ~ . + I(", variable, "^2)")
            print(class(model))
            print(.design)
            if (inherits(model, "svyglm"))
                m2 <- update(model, newmod, start = NULL, design = .design)
            else
                m2 <- update(model, newmod, start = NULL)
            c(Test = test <- deviance(model) - deviance(m2),
              Pvalue = 1 - pchisq(test, 1))
        }
    }
}

#' @importFrom effects allEffects
#' @export
allEffects.Regression <- function(model, ...)
{
    assign(".estimation.data", model$estimation.data, envir=.GlobalEnv)
    assign(".formula", model$formula, envir=.GlobalEnv)
    #assign(".weights", model$weights[model$subset], envir=.GlobalEnv)
    assign(".design", model$design, envir=.GlobalEnv)
    attach(.estimation.data)
    effects <- allEffects(model$original, ...)#BreuschPagan(x$original)
    detach(.estimation.data)
    remove(".design", envir=.GlobalEnv)
    #remove(".weights", envir=.GlobalEnv)
    remove(".formula", envir=.GlobalEnv)
    remove(".estimation.data", envir=.GlobalEnv)
    effects
}

#' @export
residualPlotsT <- function (model, terms = ~., layout = NULL, ask, main = "", fitted = TRUE,
          AsIs = TRUE, plot = TRUE, tests = TRUE, groups, ...)
{   # This is a modification of the original function from car. It hacks around problems
    #with svyglm
    require(survey)
    mf <- if (!is.null(terms))
        termsToMf(model, terms)
    else NULL
    groups <- if (!missing(groups)) {
        termsToMf(model, as.formula(paste("~", deparse(substitute(groups)))))$mf.vars[,
                                                                                      2, drop = FALSE]
    }
    else {
        if (is.null(mf$mf.groups))
            NULL
        else mf$mf.groups[, 2, drop = FALSE]
    }
    mf <- mf$mf.vars
    vform <- formula(model)#update(formula(model), attr(mf, "terms"))
    if (any(is.na(match(all.vars(vform), all.vars(formula(model))))))
        stop("Only regressors in the formula can be plotted.")
    terms <- attr(mf, "term.labels")
    vterms <- attr(terms(vform), "term.labels")
    vterms <- setdiff(vterms, terms[attr(mf, "order") > 1])
    good <- NULL
    for (term in vterms) if ((AsIs == TRUE & inherits(model$model[[term]],
                                                      "AsIs")) | inherits(model$model[[term]], "numeric") |
                             inherits(model$model[[term]], "integer") | (inherits(model$model[[term]],
                                                                                  "factor") & is.null(groups)) | inherits(model$model[[term]],
                                                                                                                          "matrix") | inherits(model$model[[term]], "poly"))
        good <- c(good, term)
    nt <- length(good) + fitted
    nr <- 0
    if (nt == 0)
        stop("No plots specified")
    if (nt > 1 & plot == TRUE & (is.null(layout) || is.numeric(layout))) {
        if (is.null(layout)) {
            layout <- switch(min(nt, 9), c(1, 1), c(1, 2), c(2,
                                                             2), c(2, 2), c(3, 2), c(3, 2), c(3, 3), c(3,
                                                                                                       3), c(3, 3))
        }
        ask <- if (missing(ask) || is.null(ask))
            prod(layout) < nt
        else ask
        op <- par(mfrow = layout, ask = ask, no.readonly = TRUE,
                  oma = c(0, 0, 1.5, 0), mar = c(5, 4, 1, 2) + 0.1)
        on.exit(par(op))
    }
    ans <- NULL
    if (!is.null(good)) {
        for (term in good) {
            nr <- nr + 1
            qtest <- if (is.null(groups))
                residualPlot(model, term, plot = plot, ...)
            else residualPlot(model, term, plot = plot, groups = groups,
                              ...)
            if (!is.null(qtest)) {
                ans <- rbind(ans, qtest)
                row.names(ans)[nr] <- term
            }
        }
    }
    if (fitted == TRUE) {
        tuk <- if (is.null(groups))
            residualPlot(model, "fitted", plot = plot, ...)
        else residualPlot(model, "fitted", plot = plot, groups = groups,
                          ...)
        if (!is.null(tuk) & class(model)[1] == "lm") {
            ans <- rbind(ans, tuk)
            row.names(ans)[nr + 1] <- "Tukey test"
            ans[nr + 1, 2] <- 2 * pnorm(abs(ans[nr + 1, 1]),
                                        lower.tail = FALSE)
        }
    }
    if (plot == TRUE)
        mtext(side = 3, outer = TRUE, main, cex = 1.2)
    if (!is.null(ans)) {
        dimnames(ans)[[2]] <- c("Test stat", "Pr(>|t|)")
        return(if (tests == FALSE | !is.null(groups)) invisible(ans) else if (all(is.na(ans))) warning("No possible lack-of-fit tests") else round(ans,
                                                                                                                                                   3))
    }
    else invisible(NULL)
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
  ConfusionMatrixFromVariables(observed, predicted)
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

  if (is(obj,"Regression") & obj$type == "Linear")
    return(ConfusionMatrixFromVariablesLinear(observed, predicted, subset, weights))
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

