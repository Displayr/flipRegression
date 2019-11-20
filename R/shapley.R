#' @importFrom utils combn
computeShapleyImportance <- function(formula, data = NULL, weights, signs, variable.names)
{
    formula.names <- AllVariablesNames(formula, data)
    outcome.name <- OutcomeName(formula, data)
    num.X <- AsNumeric(data[setdiff(formula.names, outcome.name)],
                       remove.first = TRUE)
    num.y <- AsNumeric(data[[outcome.name]], binary = FALSE)

    n.indep <- ncol(num.X)
    if (n.indep > 27)
        stop("Shapley can run with a maximum of 27 predictors. Set the ",
             "output to Relative Importance Analysis instead.")

    raw.importance <- rep(NA, n.indep)

    corr.mat <- cov.wt(cbind(num.X, num.y), wt = weights, cor = TRUE)$cor
    corr.regressors <- corr.mat[1:n.indep, 1:n.indep]
    corr.xy <- corr.mat[1:n.indep, n.indep + 1]

    combinations <- lapply(seq_len(n.indep - 1), function(x) {
        combn(seq_len(n.indep - 1), x)
    })

    repeats.factor <- sapply(seq_len(n.indep), function(x) {
        factorial(x - 1) * factorial(n.indep - x)
    }) / factorial(n.indep)

    raw.importance <- shapleyImportance(corr.regressors, corr.xy, combinations,
                                        repeats.factor)

    result <- list()
    result$raw.importance <- raw.importance
    result
}
