#' @importFrom flipFormat ExtractChartData
#' @export
flipFormat::ExtractChartData

#' @importFrom flipTables TidyTabularData
#' @export
ExtractChartData.ConfusionMatrix <- function(x)
{
    mat <- TidyTabularData(x)
    if (attr(x, "type") == "numeric")
    {
        breakpoints <- sub("[^,]*,([^]]*)\\]", "\\1", rownames(mat))
        rownames(mat) <- breakpoints
        colnames(mat) <- breakpoints
    }
    attr(mat, "title") <- paste0("Prediction-Accuracy Table: ", attr(x, "outcome.label"))
    attr(mat, "footer") <- attr(mat, "description")
    return(mat)
}

#' @export
ExtractChartData.Regression <- function(x)
{
    if (x$test.interaction)
        return(x$interaction$coefficients)
    else if (!is.null(x$importance))
        return(x$importance$importance)
    else if (x$type == "Multinomial Logit")
        return(tidyRegressionCoefficients(x$type,
                                          coefficients = t(x$summary$coefficients)))
    else if (x$type %in% c("Binary Logit", "Linear", "NBD", "Ordered Logit", "Poisson", "Quasi-Poisson"))
        return(tidyRegressionCoefficients(x$type,
                                          coefficients = x$summary$coefficients[, 1],
                                          zeta = x$summary$zeta))
    else
        stop("Unexpected regression type: ", x$type)
}

#' @importFrom flipFormat TidyLabels
tidyRegressionCoefficients <- function(type, coefficients, zeta)
{
    if(type %in% c("Binary Logit", "Linear", "NBD", "Poisson", "Quasi-Poisson"))
        names(coefficients)[-1] <- TidyLabels(names(coefficients[-1]))
    else if (type == "Ordered Logit")
    {
        coefficient.index <- !names(coefficients) %in% names(zeta)
        names(coefficients)[coefficient.index] <- TidyLabels(names(coefficients[coefficient.index]))
    } else if (type == "Multinomial Logit")
        rownames(coefficients)[-1] <- TidyLabels(rownames(coefficients)[-1])
    else
        stop("Unexpected regression type: ", type)
    return(coefficients)
}
