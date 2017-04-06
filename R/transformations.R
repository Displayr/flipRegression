
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

#' Adjust P-values for multiple comparisons using FDR
#' @description This method gives different values from those outputted by \code{p.adjust} with \code{method = "fdr"}.
#'    This difference is that our method uses a single scaling factor to multiply the p-values. However, conclusions
#'    (i.e. significant or not) does not change.
#' @param p Vector of unadjusted p-values.
#' @param alpha Overall significance level.
#' @export
PValueAdjustFDR <- function(p, alpha = 0.05)
{
    p <- as.numeric(p)
    p.adj <- rep(NA, length(p))
    nna <- which(!is.na(p))

    p <- p[nna]
    ord <- order(p, decreasing = F)
    n <- length(p)

    ii <- which(p[ord] * n/(1:n) < alpha)
    i <- if (length(ii) == 0) 1
         else max(ii)
    p.adj[nna] <- pmin(1, n/i * p)
    return(p.adj)
}
