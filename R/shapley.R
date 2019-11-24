# Calculates Shapley importance for each independent variable in the regression.
# Shapley, L.S. (1953). "A value for n-person games"
#' @importFrom utils combn
computeShapleyImportance <- function(formula, data = NULL, weights, signs,
                                     variable.names, show.warnings = TRUE,
                                     correction)
{
    signsWarning(signs, show.warnings, "Linear")

    num.X <- extractNumericX(formula, data, show.warnings)
    num.y <- AsNumeric(data[[outcome.name]], binary = FALSE)

    n.predictors <- ncol(num.X)
    if (n.predictors > 27)
        stop("Shapley can run with a maximum of 27 predictors. Set the ",
             "output to Relative Importance Analysis instead.")

    raw.importance <- rep(NA, n.predictors)

    corr.mat <- cov.wt(cbind(num.X, num.y), wt = weights, cor = TRUE)$cor
    corr.regressors <- corr.mat[1:n.predictors, 1:n.predictors]
    corr.xy <- corr.mat[1:n.predictors, n.predictors + 1]

    combinations <- lapply(seq_len(n.predictors - 1), function(x) {
        combn(seq_len(n.predictors - 1), x)
    })

    raw.importance <- shapleyImportance(corr.regressors, corr.xy, combinations)
    names(raw.importance) <- variable.names

    fit <- lm(num.y ~ num.X, weights = weights)

    # obtain standard errors
    standard.errors <-  estimateRelativeImportance(formula, data, weights,
                                                   "Linear", signs,
                                                   sum(raw.importance),
                                                   variable.names, FALSE,
                                                   show.warnings,
                                                   correction)$standard.errors

    result <- list()
    appendStatistics(result, raw.importance, standard.errors, signs, fit, correction)
}
