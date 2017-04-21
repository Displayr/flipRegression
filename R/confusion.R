#' \code{ConfusionMatrix}
#'
#' @description Produces a confusion matrix for a trained model or data.frame showing the proportion
#' of observed values that take the same values as the predicted values. Where the outcome variable
#' in the model is not a factor and not a count, observed and predicted values are assigned to buckets.
#' @param obj A model with an outcome variable or a data.frame where the first column is the outcome and
#' the second column is the prediction.
#' @param subset An optional vector specifying a subset of observations to be used or the name of a
#' column if \code{obj} is a data.frame.
#' @param weights An optional vector of sampling weights or the name of a column if \code{obj} is a data.frame.
#' @export
ConfusionMatrix <- function(obj, subset = obj$subset, weights = obj$weights)
{
    UseMethod("ConfusionMatrix")
}

# Default method for fitted objects with predict method
#' @importFrom stats predict
#' @importFrom methods is
#' @importFrom flipData Observed EstimationData
#' @export
ConfusionMatrix.default <- function(obj, subset = obj$subset, weights = obj$weights)
{
    if (is(obj, "Stepwise"))
    {
        obj <- obj$model
        subset <- obj$subset
        weights <- obj$weights
    }
    observed <- Observed(obj)
    predicted <- predict(obj)
    confusion <- confusionMatrixHelper(observed, predicted, subset, weights)

    attr(confusion, "outcome.label") <- obj$outcome.label
    accuracy.pct <- FormatAsPercent(attr(confusion, "accuracy"), 4)
    description <- paste0("Fitted model : ", obj$sample.description, "  ", sum(confusion), " observed/predicted pairs with ",
                          accuracy.pct, " accuracy;")
    attr(confusion, "description") <- description
    return(confusion)
}

# Alternative method taking a data.frame as input
#' @importFrom methods is
#' @export
ConfusionMatrix.data.frame <- function(obj, subset = obj$subset, weights = obj$weights)
{
    confusion <- confusionMatrixHelper(obj[, 1], obj[, 2], subset, weights)

    attr(confusion, "outcome.label") <- colnames(obj)[2]
    accuracy.pct <- FormatAsPercent(attr(confusion, "accuracy"), 4)
    description <- paste0(sum(confusion), " observed/predicted pairs with ", accuracy.pct, " accuracy;")
    attr(confusion, "description") <- description
    return(confusion)
}


confusionMatrixHelper <- function(observed, predicted, subset, weights)
{
    if (is.factor(observed))
    {
        confusion <- ConfusionMatrixFromVariables(observed, predicted, subset, weights)
        attr(confusion, "type") <- "factor"
    }
    else if (IsCount(observed))
    {
        confusion <- ConfusionMatrixFromVariablesNumeric(observed, predicted, subset, weights)
        attr(confusion, "type") <- "count"
    }
    else
    {
        # numeric variable and not a count - bucket predicted and observed based on range of values
        min.value <- min(predicted[subset == TRUE], observed[subset == TRUE], na.rm = TRUE)
        max.value <- max(predicted[subset == TRUE], observed[subset == TRUE], na.rm = TRUE)
        range <- max.value - min.value
        # between 3 and 30 buckets depending on the number of values
        buckets <- max(min(floor(sqrt(length(predicted[subset == TRUE]) / 3)), 30), 3)
        breakpoints <- seq(min.value, max.value, range / buckets)
        confusion <- ConfusionMatrixFromVariables(cut(observed, breakpoints, include.lowest = TRUE),
                                                  cut(predicted, breakpoints, include.lowest = TRUE), subset, weights)
        attr(confusion, "type") <- "numeric"
    }

    class(confusion) <- "ConfusionMatrix"
    accuracy <- sum(diag(confusion)) / sum(confusion)
    attr(confusion, "accuracy") <- accuracy
    return(confusion)
}


#' \code{ConfusionMatrixFromVariables}
#'
#' @param observed A \code{factor}.
#' @param predicted A \code{factor}.
#' @param subset An optional vector specifying a subset of observations to be
#' used in the fitting process or the name of a variable in \code{data}. It
#' may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights or the
#' name of a variable in \code{data}. It may not be an expression.
#' @details A contingency table showing the observed versus predicted values
#' where both inputs are factor variables.
#' @importFrom flipU IsCount
#' @importFrom stats xtabs
#' @export
ConfusionMatrixFromVariables <- function(observed, predicted, subset = NULL, weights = NULL)
{
  if(IsCount(observed))
    predicted <- floor(predicted)
  if (is.null(weights))
  {
    if (is.null(subset))
      cm <- (xtabs(~ observed + predicted))
    else
      cm <- (xtabs(~ observed + predicted, subset = subset))
  }
  else
  {
    if (is.null(subset))
      cm <- (xtabs(weights ~ observed + predicted))
    else
      cm <- (xtabs(weights ~ observed + predicted, subset = subset))
  }
  return(makeConfusionMatrixSymmetrical(cm))
}

#' \code{ConfusionMatrixFromVariablesNumeric}
#'
#' @param observed A \code{numeric}.
#' @param predicted A \code{numeric}.
#' @param subset An optional vector specifying a subset of observations to be
#' used in the fitting process, or the name of a variable in \code{data}. It
#' may not be an expression.
#' @param weights An optional vector of sampling weights or the name
#' name of a variable in \code{data}. It may not be an expression.
#' @details A contingency table showing the observed versus predicted values
#' where predicted values are assigned their closest observed value.
#' @export
ConfusionMatrixFromVariablesNumeric <- function(observed, predicted, subset = NULL, weights = NULL)
{
  if (is.factor(observed))
    observed <- as.numeric(observed)
  if (is.factor(predicted))
    predicted <- as.numeric(predicted)
  predicted.na <- is.na(predicted)
  unique.observed <- unique(observed[!predicted.na])
  unique.observed <- unique.observed[!is.na(unique.observed)]
  if(any(predicted.na))
    predicted[predicted.na] <- -Inf
  predicted <- sapply(predicted, function(x) unique.observed[which.min(abs(unique.observed - x))])
  predicted[predicted.na] <- NA
  ConfusionMatrixFromVariables(observed, predicted, subset, weights)
}

makeConfusionMatrixSymmetrical <- function(cm)
{
    row.names <- rownames(cm)
    col.names <- colnames(cm)
    all.names <- unique(c(row.names, col.names)) #As the rows are the observed values
    # Sorting if numeric
    re.numericed <- suppressWarnings(as.character(as.numeric(as.character(all.names))))
    if (!any(is.na(re.numericed)))
        if (all(re.numericed == all.names))
            all.names <- as.character(sort(as.numeric(all.names)))
    k <- length(all.names)
    new.cm <- matrix(0, nrow = k, ncol = k, dimnames = list(Observed = all.names, Predicted = all.names))
    new.cm[match(row.names, all.names), match(col.names, all.names)] <- cm
    new.cm
}


#' \code{print.ConfusionMatrix}
#'
#' @param x An object of class \code{\link{ConfusionMatrix}}.
#' @param ... Further arguments, currently unusued.
#' @details Displays a confusion matrix as a heatmap.
#' @importFrom flipU IsCount
#' @importFrom utils read.table
#' @importFrom flipData GetTidyTwoDimensionalArray
#' @export
print.ConfusionMatrix <- function(x, ...) {

    mat <- GetTidyTwoDimensionalArray(x)
    color <- "Reds"
    n.row <- nrow(mat)
    show.cellnote.in.cell <- (n.row <= 10)
    if (attr(x, "type") == "numeric")
    {
        breakpoints <- sub("[^,]*,([^]]*)\\]", "\\1", rownames(mat))
        rownames(mat) <- breakpoints
        colnames(mat) <- breakpoints
    }

    # create tooltip matrices of percentages
    cell.pct <- 100 * mat / sum(mat)
    cell.pct <- matrix(sprintf("%s%% of all cases",
                               format(round(cell.pct, 2), nsmall = 2)),
                       nrow = n.row, ncol = n.row)

    column.sums <- t(data.frame(colSums(mat)))
    column.sums <- column.sums[rep(row.names(column.sums), n.row), ]
    column.pct <- 100 * mat / column.sums
    column.pct <- matrix(sprintf("%s%% of Predicted class",
                                 format(round(column.pct, 2), nsmall = 2)),
                         nrow = n.row, ncol = n.row)
    column.pct[mat == 0] <- "-"

    row.sums <- t(data.frame(rowSums(mat)))
    row.sums <- row.sums[rep(row.names(row.sums), n.row), ]
    row.pct <- 100 * mat / t(row.sums)
    row.pct <- matrix(sprintf("%s%% of Observed class",
                              format(round(row.pct, 2), nsmall = 2)),
                      nrow = n.row, ncol = n.row)
    row.pct[mat == 0] <- "-"

    heatmap <- rhtmlHeatmap::Heatmap(mat, Rowv = FALSE, Colv = FALSE,
                                     scale = "none", dendrogram = "none",
                                     xaxis_location = "top", yaxis_location = "left",
                                     colors = color, color_range = NULL, cexRow = 0.79,
                                     cellnote = mat, show_cellnote_in_cell = show.cellnote.in.cell,
                                     xaxis_title = "Predicted", yaxis_title = "Observed",
                                     title = paste0("Prediction-Accuracy Table: ", attr(x, "outcome.label")),
                                     footer = attr(x, "description"),
                                     extra_tooltip_info = list("% cases" = cell.pct,
                                                               "% Predicted" = column.pct,
                                                               "% Observed" = row.pct))
    print(heatmap)
}


