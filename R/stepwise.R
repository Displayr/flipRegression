#' \code{Stepwise}
#' @description Stepwise selection of predictor variables by AIC.
#' @param object a \code{Regression} object.
#' @param output one of \code{Final}, \code{Detailed} or \code{All}, which determines how much information to display.
#' @param always.include a vector of names of variables to always include in the model.
#' @param steps the maximum number of steps to be considered.
#' @importFrom MASS stepAIC
#' @export
Stepwise <- function(object, output = "Final", always.include = NULL, steps = 1000)
{
    if (class(object) != "Regression")
        stop("Invalid regression model object supplied.")

    if (object$missing == "Use partial data (pairwise correlations)")
        stop("Stepwise regression is incompatible with regression models which use partial data (pairwise correlations).")

    var.names <- all.vars(object$formula)
    outcome.name <- var.names[1]

    vars.outside.model <- setdiff(always.include, var.names)
    if (length(vars.outside.model) == 1)
        warning(paste("The following variable was not included as it is not in the original model:",
                      vars.outside.model[1]))
    else if (length(vars.outside.model) > 1)
        warning(paste("The following variables were not included as they are not in the original model:",
                      paste(vars.outside.model, collapse = ", ")))
    always.include <- intersect(var.names, always.include)
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

    if (output == "All")
    {
        # We assign the output to v so that only the steps are captured
        captured <- capture.output(v <- stepAIC(reg.without.missing, scope = scope, steps = steps))
        result$steps.output <- paste(captured, collapse = "\n")
    }

    result
}
