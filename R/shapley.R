# Calculates Shapley importance for each independent variable in the regression.
# Shapley, L.S. (1953). "A value for n-person games"
#' @importFrom utils combn
#' @importFrom verbs Sum
#' @importFrom flipU StopForUserError
computeShapleyImportance <- function(formula, data = NULL, weights, signs,
                                     variable.names, robust.se = FALSE, outlier.prop.to.remove = NULL,
                                     show.warnings = TRUE, correction, ...)
{
    signsWarning(signs, show.warnings, "Linear")

    # If necessary, filter the data to the outlier adjusted subset
    outliers.removed <- !is.null(outlier.prop.to.remove) && outlier.prop.to.remove > 0
    if (outliers.removed && "non.outlier.data_GQ9KqD7YOf" %in% names(data))
    {
        data.indices <- data[["non.outlier.data_GQ9KqD7YOf"]]
        data <- data[data.indices, ]
        if (!is.null(weights))
            weights <- weights[data.indices]
    }

    info <- extractRegressionInfo(formula, data, weights, "Linear", signs, NA,
                                  variable.names, robust.se, outlier.prop.to.remove, ...)

    signs <- info$signs
    variable.names <- info$variable.names

    num.X <- extractNumericX(formula, data, show.warnings)
    num.y <- AsNumeric(data[[OutcomeName(formula, data)]], binary = FALSE)

    if (is.null(weights))
        weights <- rep(1, length(num.y))

    n.predictors <- ncol(num.X)
    if (n.predictors > 27)
        StopForUserError("Shapley can run with a maximum of 27 predictors. Set the ",
                         "output to Relative Importance Analysis instead.")

    raw.importance <- rep(NA, n.predictors)

    corr.mat <- cov.wt(cbind(num.X, num.y), wt = weights, cor = TRUE)$cor
    corr.regressors <- corr.mat[1:n.predictors, 1:n.predictors, drop = FALSE]
    corr.xy <- corr.mat[1:n.predictors, n.predictors + 1, drop = FALSE]

    combinations <- lapply(seq_len(n.predictors - 1), function(x) {
        combn(seq_len(n.predictors - 1), x)
    })

    raw.importance <- shapleyImportance(corr.regressors, corr.xy, combinations)
    names(raw.importance) <- variable.names

    fit <- lm(num.y ~ as.matrix(num.X), weights = weights)

    # obtain standard errors from relative importance analysis and scaling by the importance
    relative.importance <- estimateRelativeImportance(formula, data, weights,
                                                      "Linear", signs,
                                                      Sum(raw.importance, remove.missing = FALSE),
                                                      variable.names, robust.se,
                                                      outlier.prop.to.remove = 0,
                                                      show.warnings,
                                                      correction, ...)
    standard.errors <- raw.importance * relative.importance$standard.errors /
                       relative.importance$raw.importance

    result <- if (outliers.removed) list(non.outlier.n = info[["non.outlier.n"]]) else list()
    appendStatistics(result, raw.importance, standard.errors, signs, fit, correction)
}
