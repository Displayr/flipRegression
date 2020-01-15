#' @importFrom flipU OutcomeName
#' @importFrom stats formula
outcomeVariableFromModel <- function(Regression.object)
{
    form <- formula(Regression.object)
    if (!is.null(Regression.object$model)) # multiple imputation
        Regression.object$model[, OutcomeName(form)]
    else
        Regression.object$original$model[, OutcomeName(form)]
}
