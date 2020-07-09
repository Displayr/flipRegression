#' @importFrom flipU OutcomeName
#' @importFrom stats formula
outcomeVariableFromModel <- function(Regression.object)
{
    form <- formula(Regression.object)
    Regression.object$model[, OutcomeName(form)]
}

#' Processes the input data to be suitable to create the Jaccard coefficient output. Numeric variables
#' are inspected to be binary and categorical variables are coerced to be binary by creating sub-variables
#' for each level of the categorical variable
#' If numeric variables that are not binary are identified or a variable with no variation is identified
#' then an error is thrown. Otherwise a list is returned which gives the new estimation data, along with
#' a new formula and labels. These are required to be passed into the estimateImportance function.
#' The interaction formula is required for possible crosstab interaction.
#' @param data The data.frame containing the data to be used in the Regression
#' @param formula The formula to determine which variables to inspect and process
#' @param interaction.name String containing the name of the variable to be used in interaction.
#' @param show.labels The logical to determine if output elements or error messages here state the
#' names or labels of variables used in the analysis.
#' @return A list which contains elements suitable for a Jaccard coefficient analysis. This includes
#' \itemize{
#' \item data A data.frame of representing the equivalent information of .estimation.data after any
#'  categorical variables have been converted to binary variables
#' \item formula A formula that contains the outcome variable and the predictor names, after possible
#' categorical variables are split into many binary variables.
#' \item formula.with.interaction Similar formula but with the interaction term added.
#' \item labels String vector containing either the variable names or labels.
#' }
#' @importFrom flipU OutcomeName OutcomeVariable
#' @importFrom flipTransformations AsDataFrame
#' @importFrom flipFormat ExtractCommonPrefix
#' @noRd
processDataSuitableForJaccard <- function(data, formula, interaction.name = "NULL", show.labels = FALSE)
{
    outcome.name <- OutcomeName(formula)
    is.categorical <- vapply(data, is.factor, logical(1))
    # exclude the outcome and interaction term, if it exists
    outcome.or.interaction <- names(data) %in% c(outcome.name, interaction.name)
    categorical.predictors <- is.categorical & !outcome.or.interaction
    if (any(categorical.predictors))
    {
        # Append binary variables to replace categorical predictors
        data <- cbind(data, AsDataFrame(data[categorical.predictors],
                                        use.names = TRUE,
                                        categorical.as.binary = TRUE))
        # Remove the old categorical predictors
        data[names(which(categorical.predictors))] <- NULL
    }

    outcome.variable <- data[[outcome.name]]
    outcome.check <- checkVariableIsBinary(outcome.variable)
    outcome.is.binary <- outcome.check$is.binary
    # Check predictors are binary too
    predictor.variables <- !names(data) %in% c(outcome.name, interaction.name, "non.outlier.data_GQ9KqD7YOf")
    predictor.names <- names(data)[predictor.variables]
    predictor.variables <- data[predictor.variables]
    predictor.checks <- lapply(predictor.variables, checkVariableIsBinary)
    predictors.are.binary <- vapply(predictor.checks, "[[", FALSE, "is.binary")
    if (!outcome.is.binary || any(!predictors.are.binary))
    { # Throw error and explain why
        if (!outcome.is.binary)
            outcome.string <- variablesNotBinaryMessage(data, outcome.name, show.labels, "outcome")
        else
            outcome.string <- NULL
        if (any(!predictors.are.binary))
            predictor.string <- variablesNotBinaryMessage(data, predictor.names[!predictors.are.binary],
                                                          show.labels, "predictor")
        else
            predictor.string <- NULL
        # Check if both predictors and outcome are not binary
        both.not <- ifelse(!outcome.is.binary && any(!predictors.are.binary), " and ", "")
        # General statement
        non.binary.variable.msg <- ngettext(sum(c(!outcome.is.binary, !predictors.are.binary)),
                                            "variable to a binary variable ", "variables to binary variables ")
        general.statement <- paste0(", please change the ", non.binary.variable.msg,
                                    "if you wish to create output with the Jaccard coefficients.")
        stop("Both the outcome and predictor variable", ngettext(ncol(predictor.variables), " ", "s "),
             "need to be binary variables (only take the values zero or one). The ", outcome.string,
             both.not, predictor.string, general.statement)
    }
    # From here variables appear binary (non-missing values are 0 or 1)
    # Check if any variables have no variation (all zeros or all ones) and throw error
    outcome.no.variation <- checkBinaryVariableNoVariation(outcome.check)
    predictors.no.variation <- vapply(predictor.checks, checkBinaryVariableNoVariation, FALSE)
    if (outcome.no.variation || predictors.no.variation)
    {
        if (outcome.no.variation)
            outcome.string <- variablesNotBinaryMessage(data, outcome.name, show.labels, "outcome")
        else
            outcome.string <- NULL
        if (any(predictors.no.variation))
            predictor.string <- variablesNotBinaryMessage(data, predictor.names[predictors.no.variation],
                                                          show.labels, "predictor")
        else
            predictor.string <- NULL
        # Check if both predictors and outcome have no variation
        both.not <- ifelse(outcome.no.variation && any(predictors.no.variation), " and ", "")
        # General statement
        n.variables <- sum(c(outcome.no.variation, predictors.no.variation))
        no.variation.msg <- ngettext(n.variables,
                                     ", it is constant ", ", the variables are constant ")
        general.statement <- paste0(no.variation.msg, "with no variation (all values in the variable are zero ",
                                    "or all values in the variable are one). Consider if ",
                                    ngettext(n.variables, "this variable is " , "these variables are "),
                                    "appropriate and remove if necessary.")
        stop("Both the outcome and predictor variable", ngettext(ncol(predictor.variables), " ", "s "),
             "should be binary variables. However, the ", outcome.string,
             both.not, predictor.string, general.statement)
    }
    formulae <- createSuitableFormulaForJaccard(outcome.name = outcome.name,
                                                predictor.names = predictor.names,
                                                interaction.name = interaction.name)
    if (interaction.name != "NULL")
        predictor.names <- predictor.names[predictor.names != interaction.name]
    if (show.labels)
    {
        labels <- Labels(data, names.to.lookup = predictor.names)
        extracted.labels <- ExtractCommonPrefix(labels)
        if (!is.na(extracted.labels$common.prefix))
            labels <- extracted.labels$shortened.labels
    } else
        labels <- predictor.names
    return(list(data = data,
                formula = formulae$formula,
                formula.with.interaction = formulae$formula.with.interaction,
                labels = labels))
}

#' Helper function to extract the names in a consistent manner and paste together predictor names nicely.
#' @param dat The data.frame containing the variables and metadata if available
#' @param variable.names The string of names to check the names or labels
#' @param show.labels The logical to distinguish between names and labels
#' @param variable.type A character of either "outcome" or "predictor"
#' @importFrom flipFormat Labels
#' @noRd
variablesNotBinaryMessage <- function(dat, variable.names, show.labels, variable.type)
{
    labels <- sQuote(Labels(dat, names.to.lookup = variable.names, show.name = !show.labels))
    n.variables <- length(variable.names)
    binary.msg <- ngettext(n.variables, " is not a binary variable", " are not binary variables")
    variable.msg <- ngettext(n.variables, " variable ", " variables ")
    if (n.variables == 2)
        labels <- paste0(labels, collapse = " and ")
    else if (n.variables > 2)
        labels <- paste0(c(paste0(labels[1:(n.variables - 1)], collapse = ", "),
                           labels[n.variables]), collapse = " and ")
    paste0(variable.type, variable.msg, labels, binary.msg)
}

# Returns TRUE or FALSE is input variable is binary or not binary respectively
checkVariableIsBinary <- function(x)
{
    # Remove any missing, in Regression this function should only be called
    # after variable has been inspected for entirely missing data (avoids edge case here)
    if (any(is.na(x)))
        x <- x[!is.na(x)]
    unique.values <- unique(x)
    is.binary <- all(unique.values %in% c(0, 1))
    list(is.binary = is.binary, unique.values = unique.values)
}

#' Checks if input variable x has variation and is not a constant 0 or 1
#' @param x Output list generated from \code{checkVariableIsBinary} above.
#' @return Logical TRUE if constant with no variation seen. FALSE if binary.
#' @noRd
checkBinaryVariableNoVariation <- function(x)
{
    # Inspect the unique values
    x <- x$unique.values
    missing.x <- is.na(x)
    if (any(missing.x))
        x <- x[!missing.x]
    length(x) == 1
}

#' @importFrom stats terms.formula
createSuitableFormulaForJaccard <- function(outcome.name, predictor.names, interaction.name = "NULL")
{
    basic.formula <- formula(paste0(outcome.name, " ~ ", paste0(predictor.names, collapse = " + ")))
    formula.with.interaction <- basic.formula
    if (interaction.name != "NULL")
        formula.with.interaction <- update(terms(basic.formula),
                                           sprintf(".~.*%s", interaction.name))
    return(list(formula = basic.formula, formula.with.interaction = formula.with.interaction))
}
