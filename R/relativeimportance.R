#' @importFrom flipTransformations AsNumeric
#' @importFrom flipU OutcomeName AllVariablesNames
estimateRelativeImportance <- function(formula, data, weights, type)
{
    formula.names <- AllVariablesNames(formula)
    outcome.name <- OutcomeName(formula)
    y <- data[[outcome.name]]
    X <- data[setdiff(formula.names, outcome.name)]

    if (type == "Linear")
    {
        num.y <- AsNumeric(y, binary = FALSE)
        num.X <- AsNumeric(X, remove.first = TRUE)
        ria <- relativeImportanceLinear(num.y, num.X, weights)
    }
    else
        stop(paste("Relative importance analysis is not available for", type))
}

#' @importFrom stats cov.wt
#' @importFrom flipData CalibrateWeight
relativeImportanceLinear <- function(y, X, weights = NULL)
{
    # Johnson, J.W. (2000). "A Heuristic Method for Estimating the Relative Weight
    # of Predictor Variables in Multiple Regression"

    if (is.null(weights))
        weights <- rep(1, length(y))

    result <- list()

    x.zscore <- sapply(X, function(x) weightedZScores(x, weights))
    y.zscore <- weightedZScores(y, weights)

    corr.x <- cov.wt(X, wt = weights, cor = TRUE)$cor
    eigen.corr.x <- eigen(corr.x)
    delta <- diag(sqrt(eigen.corr.x$values))
    delta_inverse <- diag(1 / sqrt(eigen.corr.x$values))
    lambda <- eigen.corr.x$vectors %*% delta %*% t(eigen.corr.x$vectors) # Lambda = V * Delta * V^T
    lambda_inverse <- eigen.corr.x$vectors %*% delta_inverse %*% t(eigen.corr.x$vectors) # Lambda^-1 = V * Delta^-1 * V^T

    z <- x.zscore %*% lambda_inverse # orthogonal regressors
    lm.z <- lm(y.zscore ~ z, weights = weights)
    beta <- unname(lm.z$coefficients[-1])
    beta.se <- sqrt(diag(vcov(lm.z)))[1] # all identical, just take the first one.
    result$raw.importance <- as.vector(lambda ^ 2 %*% beta ^ 2)
    result$standard.errors <- sqrt(rowSums(lambda ^ 4 * beta.se ^ 4) * (2 + 4 * (beta / beta.se) ^ 2))

    lm.x <- lm(y ~ as.matrix(X), weights = weights)
    signs <- sign(unname(lm.x$coefficients[-1]))

    result$importance <- signs * 100 * prop.table(result$raw.importance)
    result$t.statistics <- signs * result$raw.importance / result$standard.errors
    result$p.values <- 2 * pt(abs(result$t.statistic), lm.x$df.residual, lower.tail = FALSE)
    result$df <- lm.x$df.residual
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
