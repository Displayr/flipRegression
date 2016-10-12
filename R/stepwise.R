#' \code{Stepwise}
#' @description Stepwise selection of predictor variables by AIC.
#' @param object a \code{Regression} object.
#' @param output one of \code{Final}, \code{Detailed} or \code{All}, which determines how much information to display.
#' @param direction one of \code{Backward} (backward elimination of variables) or \code{Forward} (forward selection of variables).
#' @param always.include a vector of names of variables to always include in the model.
#' @param steps the maximum number of steps to be considered.
#' @importFrom MASS stepAIC
#' @importFrom flipU OutcomeName AllVariablesNames CopyAttributes
#' @importFrom flipFormat Labels
#' @export
Stepwise <- function(object, output = "Final", direction = "Backward", always.include = NULL, steps = 1000)
{
    if (class(object) != "Regression")
        stop("Invalid regression model object supplied.")

    if (object$type == "Quasi-Poisson" && is.null(object$weight))
        stop("Stepwise regression is currently incompatible with unweighted Quasi-Poisson regression models. Consider using a Poisson or NBD model instead.")

    if (object$missing == "Use partial data (pairwise correlations)")
        stop("Stepwise regression is incompatible with regression models which use partial data (pairwise correlations). Please modify the 'Missing data' setting in the original model.")

    if (object$missing == "Imputation (replace missing values with estimates)" || object$missing == "Multiple imputation")
        stop("Stepwise regression is incompatible with regression models which use imputation. Please modify the 'Missing data' setting in the original model.")

    var.names <- AllVariablesNames(object$formula)
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
        lower <- formula(paste(outcome.name, "~", paste(always.include, collapse = "+")))
        scope <- list(lower = lower, upper = object$formula)
    }
    else
        scope <- object$formula

    reg.formula <- if (direction == "Backward")
        object$formula
    else if (!is.null(scope$lower))
        scope$lower
    else
        formula(paste(outcome.name, "~ 1"))

    # Copy attributes from model data so that labels are included
    d <- object$estimation.data
    for (nm in names(d))
        d[[nm]] <- CopyAttributes(d[[nm]], object$model[[nm]])

    params <- c(list(formula = reg.formula, data = d,
                     weights = object$weights[object$subset], type = object$type,
                     robust.se = object$robust.se, show.labels = object$show.labels), object$ellipsis)
    # Use do.call so that we can pass the ellipsis parameters
    reg.without.missing <- do.call("Regression", params)
    selected.model <- stepAIC(reg.without.missing, scope = scope, direction = tolower(direction),
                              trace = 0, steps = steps)

    # Use the call from the original object but with an updated formula
    call.formula <- selected.model$call[attr(selected.model$call, "name") == "formula"]
    selected.model$summary$call <- object$call
    selected.model$summary$call[attr(object$call, "name") == "formula"] <- call.formula
    selected.model$sample.description <- object$sample.description

    # Replace variable names in steps with labels
    if (object$show.labels)
    {
        nms <- names(d)
        steps <- levels(selected.model$anova$Step)
        levels(selected.model$anova$Step) <- sapply(steps, function (x) {
            nm <- substr(x, 3, nchar(x))
            if (nm %in% nms)
            {
                lbl <- Labels(d[[nm]])
                if (!is.null(lbl))
                    gsub(nm, lbl, x)
                else
                    x
            }
            else
                x
        })
    }

    result <- list(model = selected.model, output = output, direction = direction)
    class(result) <- "Stepwise"

    if (output == "All")
    {
        # We assign the output to v so that only the steps are captured
        captured <- capture.output(v <- stepAIC(reg.without.missing, scope = scope,
                                                direction = tolower(direction), steps = steps))
        result$steps.output <- paste(captured, collapse = "\n")
    }
    result
}
