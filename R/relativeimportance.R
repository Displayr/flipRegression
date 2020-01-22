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
                      type, FALSE, outlier.prop.to.remove,...)$original
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

    if (show.warnings && any(sapply(X, function(x) class(x) == "factor")))
        warning(paste0("The following variables have been treated as categorical: ",
                       paste0(names(X), collapse = ","),
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
    if (obj$type %in% c("Linear", "Binary Logit", "Poisson", "Quasi-Poisson", "NBD"))
        coef.names[-1]
    else if (obj$type %in% c("Ordered Logit"))
        coef.names[-length(coef.names):-(length(obj$original$coefficients) + 1)]
    else
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
