outcomeVariableFromModel <- function(Regression.object)
{
    form <- formula(Regression.object)
    Regression.object$model[, flipU::OutcomeName(form)]
}
