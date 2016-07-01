
#' #' Converts a factor into an indicator matrix.
#' #'
#' #' @param variable A variable in a \code{DataSet} or \code{data.frame}.
#' #' @param variable.name The name of the variable.
#' #' @export
#' FactorToIndicators <- function(variable, variable.name = deparse(substitute(variable)))
#' {
#'     indicators <- model.matrix( ~ variable - 1)
#'     if (nrow(indicators) == length(variable)) {
#'         colnames(indicators) = paste0(variable.name, ".", levels(variable))
#'         return(indicators)
#'     }
#'     new.indicators <- matrix(NA, length(variable), ncol(indicators))
#'     row.names <- as.numeric(dimnames(indicators)[[1]])
#'     colnames(new.indicators) <- colnames(indicators)
#'     new.indicators[row.names, ] <- indicators
#'     return(new.indicators)
#' }
#'
#' Converts an ordered factor into a numeric variable
#' #'
#' #' @param ordered.factor A variable in a \code{DataSet} or \code{data.frame}.
#' #' @export
#' OrderedToNumeric <- function(ordered.factor)
#' {
#'     return(unclass(ordered.factor))
#' }

#' #' Converts a an ordered factor into a numeric variable
#' #'
#' #' @param variable A variable in a \code{DataSet} or \code{data.frame}.
#' #' @param variable.name The name of the variable.
#' #' @export
#' FactorToNumeric <- function(variable, variable.name = deparse(substitute(variable)))
#' {
#'     if (is.ordered(variable))
#'         return(OrderedToNumeric(variable))
#'     return(FactorToIndicators(variable, variable.name))
#' }


#' #' Converts a data frame into a vector if it only contains a single variable.
#' @param data.frame.object A  \code{data.frame}.
dataFrameToVariableIfAppropriate <- function(data.frame.object)
{
    if(is.data.frame(data.frame.object))
        return(data.frame.object[, 1])
    data.frame.object
}