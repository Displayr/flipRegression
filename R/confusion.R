#' \code{ConfusionMatrix}
#'
#' @param obj A model with an outcome variable.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression.
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @details The proportion of observed values that take the same values as the predicted values.
#' Where the outcome
#' variable in the model is not a factor and not a count, predicted values are assigned to the closest observed
#' value.
#' @export
ConfusionMatrix <- function(obj, subset = NULL, weights = NULL)
{
    UseMethod("ConfusionMatrix")
}


#' @importFrom stats predict
#' @importFrom methods is
#' @importFrom flipData Observed
#' @describeIn ConfusionMatrix  Default confusion matrix method.
#' @export
ConfusionMatrix.default <- function(obj, subset = NULL, weights = NULL)
{
  observed <- Observed(obj)
  predicted <- predict(obj)

  if (is(obj,"Regression"))
  {
      if(obj$type == "Linear")
        return(ConfusionMatrixFromVariablesLinear(observed, predicted, subset, weights))
  }
  return(ConfusionMatrixFromVariables(observed, predicted, subset, weights))
}


#' \code{ConfusionMatrixFromVariables}
#'
#' @param observed A \code{factor}.
#' @param predicted A \code{factor}.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @details A contingency table showing the observed versus predicted values from a model.
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


#' \code{ConfusionMatrixFromVariablesLinear}
#'
#' @param observed A \code{factor}.
#' @param predicted A \code{factor}.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#'
#' @details A contingency table showing the observed versus predicted values from a model, for linear models.
#' Prediced values are assigned the value of the closest observed value.
#' @export
ConfusionMatrixFromVariablesLinear <- function(observed, predicted, subset = NULL, weights = NULL)
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
  #levels(observed) <- paste("Observed", levels(observed))
  #levels(predicted) <- paste("Predicted", levels(predicted))
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

