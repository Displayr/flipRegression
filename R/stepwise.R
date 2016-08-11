#' \code{Stepwise}
#' @description Stepwise selection of predictor variables by AIC.
#' @param object a \code{Regression} object.
#' @param output one of \code{final}, \code{detailed} or \code{all}, which determines how much information to display.
#' @param always.include a vector of names of variables to always include in the model.
#' @param steps the maximum number of steps to be considered.
#' @importFrom MASS stepAIC
#' @export
Stepwise <- function(object, output = "final", always.include = NULL, steps = 1000)
{
    var.names <- all.vars(object$formula)
    outcome.name <- var.names[1]

    # Check that variables in always.include are in the regression formula
    if (!is.null(always.include) && length(always.include) > 0)
    {
        lower <- formula(paste(outcome.name, "~", paste(always.include, sep = "+")))
        scope <- list(lower = lower, upper = object$formula)
    }
    else
        scope <- NULL

    params <- c(list(formula = object$formula, data = object$estimation.data, weights = object$weights[object$subset],
                     type = object$type, robust.se = object$robust.se), object$ellipsis)
    # Use do.call so that we can pass the ellipsis parameters
    reg.without.missing <- do.call("Regression", params)
    selected.model <- stepAIC(reg.without.missing, scope = scope, trace = 0, steps = steps)

    # Use the call from the original object but with an updated formula
    call.formula <- selected.model$call[attr(selected.model$call, "name") == "formula"]
    selected.model$summary$call <- object$call
    selected.model$summary$call[attr(object$call, "name") == "formula"] <- call.formula

    result <- list(model = selected.model, output = output)
    class(result) <- "Stepwise"

    if (output == "all")
    {
        # We assign the output to v so that only the steps are captured
        captured <- capture.output(v <- stepAIC(reg.without.missing, scope = scope, steps = steps))
        result$steps.output <- paste(captured, collapse = "\n")
    }

    result
}
