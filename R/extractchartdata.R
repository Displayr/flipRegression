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
    return(mat)
}

#' @export
ExtractChartData.Regression <- function(x)
{
    ExtractChartData(ConfusionMatrix(x))
}
