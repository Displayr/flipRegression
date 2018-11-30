#' Plots the observed, predict and an independent variable from a
#' regression model.
#'
#' @param Regression.object The model.
#' @param predictor.number The index of the predictor to be used in the plot,
#' where 1 means the first predictor.
#' @details With numeric predicts, other variables are assumed to have an
#' average effect. With factors, the first level is from the intercept, and
#' the other levels are the intercept plus the parameter (thus, if you modify
#' the contrats, this plot will not display the predicted values correctly).
#' @importFrom flipFormat Equation
#' @importFrom graphics abline segments
#' @export
PredictionPlot <- function(Regression.object, predictor.number = NULL)
{
    if (!is(Regression.object, "Regression"))
        stop("'PredictionPlot' requires a regression object.")
    # Working out which variable to use.
    coefs <- coef(Regression.object)
    coefs.n <- length(coefs)
    if (is.null(predictor.number))
    {
        if (coefs.n > 2)
            warning("Only the first predictor variable is shown on this plot")
        predictor.number <- 1
    }
    else if (!predictor.number %in% coefs.n)
        warningPredictorVariableDoesNotExist()
    # Identifying the variable to use
    variable.number <- predictor.number + 1
    y.name = names(Regression.object$model)[1]
    x.name <- names(Regression.object$model)[variable.number]
    x <- Regression.object$model[[variable.number]]#model.matrix(Regression.object)[, 2]
    y <- Regression.object$model[[1]]
    # Creating the plot.
    equation <- Equation(Regression.object)
    plt <- plot(x,  y, xlab = x.name, ylab = y.name, main = equation)
    # Plotting the line of best fit.
    if(is.factor(x))
    {
        # Identifying the parameter to use
        n.parameters <- unlist(lapply(x, nlevels))
        factors <- n.parameters != 0
        n.parameters[!factors] <- 1
        n.parameters[factors] <- n.parameters[factors] - 1
        cum.parameters <- cumsum(n.parameters)
        par.indices <- c(1, (cum.parameters[predictor.number] + 1):cum.parameters[predictor.number + 1])
        pars <- coefs[par.indices]
        pars[-1] <- pars[-1] + pars[1]
        n.pars <- n.parameters[variable.number] + 1
        for (i in 1:n.pars)
            segments(i - 0.5, pars[i], i + 0.5, pars[i], col = "red", lwd = "2", lty = 2)
    }
    else
    {
        b <- coefs[[x.name]]
        x.mean <- mean(x)
        y.mean <- mean(y)
        a <- y.mean - b * x.mean
        plt <- abline(a, b, col = "red", lwd = 2, lty = 2)
    }
    plt
}

#' Produce an effects plot of a regression model
#'
#' @param model A model of class \code{Regression}
#' @param max.factor.label.length The maximum length of any factor label, as plotted on the
#'   x-axis for categorical variables. Longer labels are truncated, possibly with an integer
#'   suffix to ensure uniquueness.
#' @param y.axis.title The title of the y-axis for each plot. Defaults to the outcome label.
#' @export
EffectsPlot <- function(model,
                        max.factor.label.length = NULL,
                        y.axis.title = NULL)
{
    effects <- allEffects(model)

    fixEffectsLabels <- function(ef, max.len){

        if (ef$variables[[1]]$is.factor)
        {
            old.levs <- ef$variables[[1]]$levels
            new.levs <- sapply(old.levs, function(x) substr(x, 1, min(max.len, nchar(x))))
            new.levs <- make.unique(new.levs, sep = "")

            indices <- match(levels(ef$x[[1]]), old.levs)
            levels(ef$x[[1]]) <- new.levs[indices]
            ef$variables[[1]]$levels <- new.levs
            return(ef)

        }
        else
            return(ef)
    }

    ylab <- y.axis.title
    if (is.null(ylab) || nchar(ylab) == 0)
    {
        ylab <- model$outcome.label
        if (model$type == "Binary Logit")
            ylab <- paste0("probability(", ylab, ")")

    }

    if (!is.null(max.factor.label.length))
    {
        effects <- lapply(effects, fixEffectsLabels, max.factor.label.length)
        class(effects) <- "efflist"
    }

    type <- if (model$type %in% c("Ordered Logit", "Multinomial Logit"))
        "probability"
    else
        "response"

    plot(effects,
         ylab = ylab,
         type = type)
}
