#' @importFrom flipU OutcomeName
#' @importFrom stats formula
outcomeVariableFromModel <- function(Regression.object)
{
    form <- formula(Regression.object)
    Regression.object$model[, OutcomeName(form)]
}
