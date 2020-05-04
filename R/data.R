#' @importFrom flipU OutcomeName
#' @importFrom stats formula
outcomeVariableFromModel <- function(Regression.object)
{
    form <- formula(Regression.object)
    Regression.object$model[, OutcomeName(form)]
}

#' Checks if the input data is suitable to create the Jaccard coefficient output
#' by checking that it is a binary variable with only two values, 0 and 1.
#' @param data The data.frame containing the data to be used in the Regression
#' @param formula The formula to extract the appropriate variables
#' @param show.labels The logical to determine if the error message states names or labels
#' @importFrom flipU OutcomeName OutcomeVariable
#' @noRd
checkDataSuitableForJaccard <- function(data, formula, show.labels)
{
    outcome.name <- OutcomeName(formula)
    outcome.variable <- data[[outcome.name]]
    outcome.is.binary <- checkVariableIsBinary(outcome.variable)
    # Check predictors are binary too
    predictor.names <- attr(terms.formula(formula, data = data), "term.labels")
    predictor.variables <- data[which(names(data) %in% predictor.names)]
    predictors.are.binary <- vapply(predictor.variables, checkVariableIsBinary, FALSE)
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
    # Check if any variables have no variation (all zeros or all ones) and throw warning
    outcome.no.variation <- checkBinaryVariableNoVariation(outcome.variable)
    predictors.no.variation <- vapply(predictor.variables, checkBinaryVariableNoVariation, FALSE)
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
        warning("Both the outcome and predictor variable", ngettext(ncol(predictor.variables), " ", "s "),
                "should be binary variables. However, the ", outcome.string,
                both.not, predictor.string, general.statement)
    }
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
    all(unique.values %in% c(0, 1))
}

#' Checks if input variable x has variation and is not a constant 0 or 1
#' @param x A binary variable, a vector with values either, 0, 1 or NA.
#' @return Logical TRUE if constant with no variation seen. FALSE if binary.
#' @noRd
checkBinaryVariableNoVariation <- function(x)
{
    missing.x <- is.na(x)
    if (any(missing.x))
        x <- x[!missing.x]
    length(unique(x)) == 1
}
