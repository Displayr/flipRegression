estimateImportance <- function(formula, data = NULL, weights, type, signs, r.square, variable.names,
                               robust.se = FALSE, outlier.prop.to.remove, show.warnings = TRUE, correction,
                               importance, ...)
{
    if (!is.null(weights))
        robust.se <- FALSE

    if (importance == "Relative Importance Analysis")
        estimateRelativeImportance(formula, data, weights, type, signs, r.square, variable.names,
                                   robust.se, outlier.prop.to.remove, show.warnings, correction, ...)
    else if (importance == "Shapley Regression")
        computeShapleyImportance(formula, data, weights, signs, variable.names, robust.se, outlier.prop.to.remove,
                                 show.warnings, correction, ...)
    else if (importance == "Jaccard Coefficient")
        computeJaccardImportance(formula, data, weights, variable.names, correction, ...)
    else if (importance == "Correlation")
        computeCorrelationImportance(formula, data, weights, variable.names, correction, ...)
    else
        stop("Importance type not handled: ", importance)
}

#' @importFrom stats cov.wt as.formula
#' @importFrom flipData DataFormula
#' @importFrom flipTransformations AsNumeric
#' @importFrom flipU OutcomeName AllVariablesNames
#' @noRd
estimateRelativeImportance <- function(formula, data = NULL, weights, type, signs, r.square, variable.names,
                                       robust.se = FALSE, outlier.prop.to.remove = NULL, show.warnings = TRUE,
                                       correction, ...)
{
    # Johnson, J.W. (2000). "A Heuristic Method for Estimating the Relative Weight
    # of Predictor Variables in Multiple Regression"

    if (type == "Multinomial Logit")
        stop("Relative importance analysis is not available for ", type)

    processed.data <- subsetDataWeightsAndFormula(formula, data, weights)
    data <- processed.data$data
    weights <- processed.data$weights
    formula <- processed.data$formula

    info <- extractRegressionInfo(formula, data, weights, type, signs,
                                  r.square, variable.names, robust.se,
                                  outlier.prop.to.remove, ...)
    signs <- info$signs
    r.square <- info$r.square
    variable.names <- info$variable.names

    signsWarning(signs, show.warnings, type)

    num.X <- extractNumericX(formula, data, show.warnings)

    input.weights <- weights
    if (is.null(weights))
        weights <- rep(1, nrow(data))

    result <- list()

    x.zscore <- sapply(num.X, function(x) weightedZScores(x, weights))

    outcome.name <- OutcomeName(formula, data)
    y <- if (type == "Linear")
    {
        num.y <- AsNumeric(data[[outcome.name]], binary = FALSE)
        weightedZScores(num.y, weights)
    }
    else
        data[[outcome.name]]
    # Protect against ordered factor with empty levels
    if (type == "Ordered Logit")
        y <- Ordered(y)

    corr.x <- cov.wt(num.X, wt = weights, cor = TRUE)$cor
    diag(corr.x) <- 1    # may not be exactly 1 from cov.wt
    eigen.corr.x <- eigen(corr.x)
    delta <- diag(sqrt(eigen.corr.x$values))
    delta_inverse <- diag(1 / sqrt(eigen.corr.x$values))
    lambda <- eigen.corr.x$vectors %*% delta %*% t(eigen.corr.x$vectors) # Lambda = V * Delta * V^T
    lambda_inverse <- eigen.corr.x$vectors %*% delta_inverse %*% t(eigen.corr.x$vectors) # Lambda^-1 = V * Delta^-1 * V^T

    z <- x.zscore %*% lambda_inverse # orthogonal regressors

    reg.data <- cbind(data.frame(y = y), as.data.frame(z))
    data.formula <- as.formula(paste0("y ~ ", paste(paste0("V", 1:ncol(z)), collapse = "+")))

    fit <- if (type == "Linear")
        lm(y ~ 0 + z, weights = weights)
    else
        FitRegression(data.formula, reg.data, input.weights,
                      type, FALSE, outlier.prop.to.remove = 0, seed = 12321, ...)$original
    beta <- extractVariableCoefficients(fit, type, FALSE)
    beta.se <- extractVariableStandardErrors(fit, type, robust.se, FALSE)

    raw.importance <- as.vector(lambda ^ 2 %*% beta ^ 2)

    names(raw.importance) <- variable.names
    if (r.square == 0)
    {
        scaling.factor <- 1
        warning("The R-squared is 0. As a result, the raw scores have not ",
                "been scaled to sum to the R-squared.")
    }
    else
        # R-squared is not always the sum of raw.importance, e.g. with binary logit
        scaling.factor <- r.square / sum(raw.importance)
    raw.importance <- raw.importance * scaling.factor

    se  <- sqrt(rowSums(lambda ^ 4 * beta.se ^ 4) * (2 + 4 * (beta / beta.se) ^ 2)) * scaling.factor
    names(se) <- variable.names

    appendStatistics(result, raw.importance, se, signs, fit, correction)
}

extractNumericX <- function(formula, data, show.warnings)
{
    formula.names <- AllVariablesNames(formula, data)
    outcome.name <- OutcomeName(formula, data)
    X <- data[setdiff(formula.names, outcome.name)]

    ## We remove the "ordered" class so that ordered-categorical variables are
    ## treated in the same way as they are in regression, i.e., dummy variables
    ## are created from the categories.
    for (j in 1:ncol(X))
        if (all(c("factor", "ordered") %in% class(X[, j])))
            class(X[, j]) <- "factor"

    if (show.warnings && any(factors <- sapply(X, function(x) class(x) == "factor")))
        warning(paste0("The following variables have been treated as categorical: ",
                       paste0(names(X)[factors], collapse = ","),
                       ". This may over-inflate their effects."))

    AsNumeric(X, remove.first = TRUE)
}

signsWarning <- function(signs, show.warnings, type)
{
    if (show.warnings && any(signs < 0))
        warning("Negative signs in Relative Importance scores were applied from coefficient signs in ",
                regressionType(type),
                ". To disable this feature, check the Absolute importance scores option.")
}

#' @importFrom stats weighted.mean
#' @importFrom flipStatistics Correlation
weightedZScores <- function(x, weights)
{
    m <- weighted.mean(x, weights)
    std <- sqrt(Correlation(x, x, weights = weights, correlation = FALSE))
    (x - m) / std
}

# Only extract coefficients for variables, not intercepts
extractVariableCoefficients <- function(model, type, linear.regression.intercept = TRUE)
{
    if (type == "Linear" && !linear.regression.intercept)
        model$coefficients
    else if (type %in% c("Linear", "Binary Logit", "Poisson", "Quasi-Poisson", "NBD"))
        model$coefficients[-1]
    else if (type %in% c("Ordered Logit"))
        model$coefficients
    else
        stop(paste("Type not handled: ", type))
}

extractVariableStandardErrors <- function(model, type, robust.se, linear.regression.intercept = TRUE)
{
    standard.errors <- if (robust.se != FALSE)
        coeftest(model, vcov. = vcov2(model, robust.se))[, 2]
    else
        summary(model)$coefficients[, 2]

    if (type == "Linear" && !linear.regression.intercept)
        standard.errors
    else if (type %in% c("Linear", "Binary Logit", "Poisson", "Quasi-Poisson", "NBD"))
        standard.errors[-1]
    else if (type %in% c("Ordered Logit"))
        standard.errors[-length(standard.errors):-(length(model$coefficients) + 1)]
    else
        stop(paste("Type not handled: ", type))
}

extractVariableCoefficientNames <- function(obj)
{
    coef.names <- rownames(obj$summary$coefficients)
    coef.names <- coef.names[!grepDummyVars(coef.names)]
    if (obj$type %in% c("Linear", "Binary Logit", "Poisson", "Quasi-Poisson", "NBD"))
        coef.names[-1]
    else if (obj$type %in% c("Ordered Logit"))
    {
        orig.coef <- obj$original$coefficients
        orig.coef <- orig.coef[!grepDummyVars(names(orig.coef))]
        coef.names[-length(coef.names):-(length(orig.coef) + 1)]
    } else
        stop(paste("Type not handled: ", obj$type))
}

isTStatisticUsed <- function(model)
{
    grepl("^t", colnames(summary(model)$coefficients)[3])
}

appendStatistics <- function(obj, raw.importance, standard.errors, signs, fit, correction)
{
    result <- obj
    result$raw.importance <- raw.importance
    result$standard.errors <- standard.errors
    result$importance <- unname(signs) * 100 * prop.table(raw.importance)
    result$statistics <- unname(signs) * raw.importance / standard.errors
    is.t.statistic.used <- isTStatisticUsed(fit)
    result$statistic.name <- if (is.t.statistic.used) "t" else "z"
    raw.p.values <- if (is.t.statistic.used)
        2 * pt(abs(result$statistics), fit$df.residual, lower.tail = FALSE)
    else
        2 * pnorm(abs(result$statistics), lower.tail = FALSE)
    result$p.values <- pvalAdjust(raw.p.values, correction)
    result
}

extractRegressionInfo <- function(formula, data, weights, type, signs,
                                  r.square, variable.names, robust.se = FALSE,
                                  outlier.prop.to.remove, ...)
{
    if (is.null(signs) || any(is.na(signs)) || is.null(r.square) || is.na(r.square))
    {
        formula2 <- DataFormula(formula, data)
        fit <- FitRegression(formula2, data, weights, type, robust.se, outlier.prop.to.remove, ...)
        if (is.null(signs) || any(is.na(signs)))
        {
            signs <- sign(round(extractVariableCoefficients(fit$original, type), 13))
            signs[signs == 0] <- 1 # set sign 0 to 1
        }
        if (is.null(r.square) || is.na(r.square))
            r.square <- GoodnessOfFit(fit$original)$value

        if (all(is.na(variable.names)))
        {
            tmp.names <- CleanBackticks(names(fit$original$coefficients))
            variable.names <- if (type == "Ordered Logit") tmp.names else tmp.names[-1]
        }
    }
    list(signs = signs, r.square = r.square, variable.names = variable.names)
}

#' Function to find the correct subset omitting outliers, returns
#' the appropriate subsetted data, updated formula (incase dot is used)
#' @param formula The formula object to construct the regression
#' @param data The estimation data used in the regression construction
#' @param weights The optional numeric vector of non-negative weights
#' @param outlier.prop.to.remove Single numeric value denoting the proportion of observations removed in
#'  the automated outlier removal.
#' @importFrom stats terms.formula update.formula
#' @importFrom flipU OutcomeName
#' @importFrom flipU CopyAttributes
#' @noRd
subsetDataWeightsAndFormula <- function(formula, data, weights)
{
    # If necessary, filter the data to the outlier adjusted subset
    if ("non.outlier.data_GQ9KqD7YOf" %in% names(data))
    {
        data.indices <- data[["non.outlier.data_GQ9KqD7YOf"]]
        data <- CopyAttributes(data[data.indices, -which(names(data) == "non.outlier.data_GQ9KqD7YOf")], data)
        if (!is.null(weights))
            weights <- weights[data.indices]
    }

    # Protect against dot in formula with the non.outlier indicator variable
    formula.terms <- terms.formula(formula, data = data)
    predictor.names <- attr(formula.terms, "term.labels")
    non.outlier.in.data.frame <- predictor.names == "non.outlier.data_GQ9KqD7YOf"
    if (any(non.outlier.in.data.frame))
    {
        formula <- update.formula(formula, drop.terms(formula.terms,
                                                      which(non.outlier.in.data.frame),
                                                      keep.response = TRUE))
        predictor.names <- predictor.names[!non.outlier.in.data.frame]
    }
    # subset data (removes interaction term from data and only extracts terms from formula)
    outcome.name <- OutcomeName(formula, data)
    data <- data[, c(outcome.name, predictor.names)]
    return(list(formula = formula,
                data = data,
                weights = weights))
}

#' Computes the Jaccard Coefficients for the outcome variable against all the predictors
#' used in the regression and returns those coefficients along with their relative importance
#' and the outcome variable itself along with the data.frame of predictors
#' @param formula The formula used in the Regression model
#' @param data The data to compute the calculation on
#' @param weights A numeric vector of weights
#' @param variable.names Vector of names of the coefficients in the regression model. Defaults to NA for
#'   comptability with crosstab interaction code that can predictor data could be excluded when filtered.
#' @importFrom flipU OutcomeName
#' @importFrom stats terms.formula
#' @noRd
computeJaccardCoefficients <- function(formula, data = NULL, weights, variable.names = NA)
{
    processed.data <- subsetDataWeightsAndFormula(formula, data, weights)
    relevant.data <- processed.data$data
    weights <- processed.data$weights
    formula <- processed.data$formula

    outcome.name <- OutcomeName(formula, relevant.data)
    outcome.variable <- relevant.data[[outcome.name]]
    predictor.names <- attr(terms.formula(formula, data = relevant.data), "term.labels")
    predictor.variables <- relevant.data[, predictor.names, drop = FALSE]
    if (!any(is.na(variable.names)))
        names(predictor.variables) <- variable.names
    else
        variable.names <- names(predictor.variables)
    jaccard.coefficients <- vapply(predictor.variables, singleJaccardCoefficient,
                                   numeric(1), y = outcome.variable, weights = weights)
    names(jaccard.coefficients) <- variable.names
    list(outcome.variable = outcome.variable,
         coefficients = jaccard.coefficients,
         predictor.variables = predictor.variables,
         weights = weights)
}

#' Compute the single Jaccard coefficient between x and y
#' @param x The binary input variable x
#' @param y The binary input variable y
#' @param centered A logical to determine if the centered coefficient is returned.
#'   If \code{TRUE}, then the estimate of the mean is subtracted off the jaccard coefficient.
#' @noRd
singleJaccardCoefficient <- function(x, y, centered = FALSE, weights = NULL) {
    if (is.null(weights))
        j <- sum(y & x, na.rm = TRUE)/sum(x | y, na.rm = TRUE)
    else
        j <- sum(weights[y & x], na.rm = TRUE)/sum(weights[x | y], na.rm = TRUE)
    if (centered)
        j <- j - singleJaccardExpectation(x, y)
    j
}

# Computes the expected value of the Jaccard coefficient between binary variables x and y
singleJaccardExpectation <- function(x, y)
{
    px <- mean(x, na.rm = TRUE)
    py <- mean(y, na.rm = TRUE)
    px * py/(px + py - px * py)
}

#' @param formula The formula used in the Regression model
#' @param data The data to compute the calculation on
#' @param weights A numeric vector of weights
#' @param variable.names Vector of names of the coefficients in the regression model. Defaults to NA for
#'   comptability with crosstab interaction code that can predictor data could be excluded when filtered.
#' @param correction A character specifying the multiple comparisons correction to be applied.
#' @importFrom stats terms.formula
#' @noRd
computeJaccardImportance <- function(formula, data = NULL, weights, variable.names = NA, correction)
{
    jaccard.coef.output <- computeJaccardCoefficients(formula, data, weights, variable.names)
    # Extract the outcome binary variable, the data.frame of predictor binary variables
    # the computed jaccard coefficients and the relative importance and possible weights
    y <- jaccard.coef.output$outcome.variable
    X <- jaccard.coef.output$predictor.variables
    jaccard.coefs <- jaccard.coef.output$coefficients
    weights <- if(is.null(jaccard.coef.output$weights)) rep(1, length(y)) else CalibrateWeight(weights)
    test.output <- lapply(X, jaccardTest, y = y, weights = weights)
    test.statistics <- vapply(test.output, "[[", numeric(1), "t")
    relative.importance <- 100 * prop.table(abs(test.statistics))
    pvalues <- vapply(test.output, "[[", numeric(1), "p.value")
    pvalues <- pvalAdjust(pvalues, correction)
    standard.errors <- vapply(test.output, "[[", numeric(1), "standard.error")
    names(pvalues) <- names(standard.errors) <- variable.names
    sample.size <- vapply(X, function(x) sum(!is.na(x) & !is.na(y)), numeric(1))
    names(sample.size) <- variable.names
    list(importance = relative.importance,
         raw.importance = jaccard.coefs,
         standard.errors = standard.errors,
         statistics = test.statistics,
         sample.size = sample.size,
         p.values = pvalues)
}

#' Compute the single p-value using reasmpled binary variables x and y
#' @param x A numeric vector of the binary predictor variable
#' @param y A numeric vector of the binary outcome variable
#' @param weights A numeric vector of non-negative weight values
#' @importFrom survey svydesign svyratio degf
#' @noRd
jaccardTest <- function(x, y, weights)
{
    design <- svydesign(ids = ~ 1,
                        data = data.frame(numerator = as.integer(y & x),
                                          denominator = as.integer(y | x)),
                        weights = ~ weights)
    ratio.estimation <- svyratio(~ numerator, ~ denominator, design, na.rm = TRUE)
    jaccard <- as.numeric(ratio.estimation$ratio)
    df <- degf(design)
    p <- mean(x, na.rm = TRUE)
    q <- mean(y, na.rm = TRUE)
    expected.jaccard <- p * q / (p + q - p * q)
    # Catch case when estimated Jaccard coef == 1 (vectors identical)
    if (jaccard != 0 && jaccard != 1)
        standard.error <- sqrt(as.numeric(ratio.estimation$var))
    else
    { # Compute the second order Taylor expansion estimate of the variance
        weight.factor <- sum(weights^2)/sum(weights)^2
        multiplier <- (p + q - 2 * p * q)/(p * q * (p + q - p * q))
        standard.error <- expected.jaccard* sqrt(weight.factor * multiplier)
    }
    t <- (jaccard - expected.jaccard) / standard.error
    p <- 2 * pt(-abs(t), df)
    list(jaccard = jaccard,
         expected = expected.jaccard,
         df = df,
         standard.error = standard.error,
         t = t,
         p.value = p)
}

#' @param x The formula used in the Regression model
#' @param data The data to compute the calculation on
#' @param weights A numeric vector of weights
#' @param variable.names Vector of names of the coefficients in the regression model.
#' @param correction A character specifying the multiple comparisons correction to be applied.
#' @importFrom flipU OutcomeName
#' @importFrom stats terms.formula
#' @importFrom flipStatistics CorrelationsWithSignificance
#' @importFrom flipTransformations AsDataFrame
#' @noRd
computeCorrelationImportance <- function(formula, data = NULL, weights, variable.names, correction)
{
    processed.data <- subsetDataWeightsAndFormula(formula, data, weights)
    relevant.data <- AsDataFrame(processed.data$data, use.names = TRUE, categorical.as.binary = FALSE)
    names(relevant.data) <- names(processed.data$data)
    weights <- processed.data$weights
    formula <- processed.data$formula

    outcome.name <- OutcomeName(formula, relevant.data)
    outcome.variable <- relevant.data[[outcome.name]]
    predictor.names <- attr(terms.formula(formula, data = relevant.data), "term.labels")
    predictor.variables <- relevant.data[, predictor.names, drop = FALSE]
    if (!any(is.na(variable.names)))
        colnames(relevant.data) <- c(outcome.name, variable.names)
    else
        variable.names <- colnames(predictor.variables)

    weights <- if (is.null(weights)) rep(1, nrow(relevant.data)) else weights
    correlation.output <- CorrelationsWithSignificance(relevant.data, weights)
    indices <- match(variable.names, colnames(correlation.output$cor), nomatch = 0)

    correlation.coefs <- extractFirstRowMatrixToNumeric(correlation.output$cor, indices)
    relative.importance <- 100 * prop.table(abs(correlation.coefs))
    statistics <- extractFirstRowMatrixToNumeric(correlation.output$t, indices)
    std.errs <- extractFirstRowMatrixToNumeric(correlation.output$standard.errors, indices)
    pvalues <- extractFirstRowMatrixToNumeric(correlation.output$p, indices)
    pvalues <- pvalAdjust(pvalues, correction)
    sample.size <- vapply(relevant.data[indices], function(x, y) sum(!is.na(x) & !is.na(y)), numeric(1), y = outcome.variable)
    list(importance = relative.importance,
         raw.importance = correlation.coefs,
         statistics = statistics,
         standard.errors = std.errs,
         sample.size = sample.size,
         p.values = pvalues)
}

# Extracts the first row of a numeric matrix, the columns selected are
# governed by the indices argument.
extractFirstRowMatrixToNumeric <- function(mat, indices)
{
    x <- mat[1, indices, drop = FALSE]
    names.x <- colnames(x)
    x <- as.numeric(x)
    names(x) <- names.x
    x
}
