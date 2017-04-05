#' @importFrom stats cov.wt as.formula
#' @importFrom flipData DataFormula
#' @importFrom flipTransformations AsNumeric
#' @importFrom flipU OutcomeName AllVariablesNames
estimateRelativeImportance <- function(formula, data, weights, type, signs, r.square, variable.names,
                                       robust.se = FALSE, show.sign.warning = TRUE, ...)
{
    # Johnson, J.W. (2000). "A Heuristic Method for Estimating the Relative Weight
    # of Predictor Variables in Multiple Regression"

    if (type == "Multinomial Logit")
        stop(paste("Relative importance analysis is not available for", type))

    if (is.null(signs) || any(is.na(signs)) || is.null(r.square) || is.na(r.square))
    {
        fit <- FitRegression(formula, data, NULL, NULL, type, robust.se, ...)
        if (is.null(signs) || any(is.na(signs)))
            signs <- sign(extractVariableCoefficients(fit$original, type))
        if (is.null(r.square) || is.na(r.square))
            r.square <- GoodnessOfFit(fit$original)$value
    }

    if (show.sign.warning && any(signs < 0))
        warning(paste0("Negative signs in Relative Importance scores were applied from coefficient signs in ",
                      regressionType(type), ". To disable this feature, check the Absolute importance scores option."))

    formula.names <- AllVariablesNames(formula)
    outcome.name <- OutcomeName(formula)
    X <- data[setdiff(formula.names, outcome.name)]

    num.X <- AsNumeric(X, remove.first = TRUE)

    input.weights <- weights
    if (is.null(weights))
        weights <- rep(1, nrow(data))
    else
        robust.se <- FALSE

    result <- list()

    x.zscore <- sapply(num.X, function(x) weightedZScores(x, weights))

    y <- if (type == "Linear")
    {
        num.y <- AsNumeric(data[[outcome.name]], binary = FALSE)
        weightedZScores(num.y, weights)
    }
    else
        data[[outcome.name]]

    corr.x <- cov.wt(num.X, wt = weights, cor = TRUE)$cor
    eigen.corr.x <- eigen(corr.x)
    delta <- diag(sqrt(eigen.corr.x$values))
    delta_inverse <- diag(1 / sqrt(eigen.corr.x$values))
    lambda <- eigen.corr.x$vectors %*% delta %*% t(eigen.corr.x$vectors) # Lambda = V * Delta * V^T
    lambda_inverse <- eigen.corr.x$vectors %*% delta_inverse %*% t(eigen.corr.x$vectors) # Lambda^-1 = V * Delta^-1 * V^T

    z <- x.zscore %*% lambda_inverse # orthogonal regressors

    reg.data <- cbind(data.frame(y = y), as.data.frame(z))
    data.formula <- as.formula(paste0("y ~ ", paste(paste0("V", 1:ncol(z)), collapse = "+")))

    fit <- if (type == "Linear")
        lm.z <- lm(y ~ 0 + z, weights = weights)
    else
        FitRegression(data.formula, reg.data, NULL, input.weights, type, FALSE, ...)$original
    beta <- extractVariableCoefficients(fit, type, FALSE)
    beta.se <- extractVariableStandardErrors(fit, type, robust.se, FALSE)

    raw.importance <- as.vector(lambda ^ 2 %*% beta ^ 2)
    names(raw.importance) <- variable.names
    scaling.factor <- r.square / sum(raw.importance)
    result$raw.importance <- raw.importance * scaling.factor
    se  <- sqrt(rowSums(lambda ^ 4 * beta.se ^ 4) * (2 + 4 * (beta / beta.se) ^ 2)) * scaling.factor
    names(se) <- variable.names
    result$standard.errors <- se
    result$importance <- unname(signs) * 100 * prop.table(raw.importance)

    result$statistics <- unname(signs) * result$raw.importance / result$standard.errors
    is.t.statistic.used <- isTStatisticUsed(fit)
    result$statistic.name <- if (is.t.statistic.used) "t" else "z"
    result$p.values <- if (is.t.statistic.used)
        2 * pt(abs(result$statistics), fit$df.residual, lower.tail = FALSE)
    else
        2 * pnorm(abs(result$statistics), lower.tail = FALSE)
    result
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
