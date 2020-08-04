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
        # Change ordered factors to factors, otherwise it won't be split into binary numeric,
        # only numeric with ordered values for each level instead of separate binary values
        # for each level
        data[categorical.predictors] <- lapply(data[categorical.predictors],
                                               function(x) {
                                                   class(x) <- "factor"
                                                   x
                                               })
        # Append (insert) binary variables to replace categorical predictors
        for(cat.var in names(which(categorical.predictors)))
            data <- append(data,
                           AsDataFrame(data[names(data) == cat.var], use.names = TRUE, categorical.as.binary = TRUE),
                           after = which(names(data) == cat.var))
        # Restore the data.frame
        data <- data.frame(data, check.names = FALSE)
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
    formula <- createSuitableFormulaForJaccard(outcome.name = outcome.name,
                                                predictor.names = predictor.names)
    if (interaction.name != "NULL")
        predictor.names <- predictor.names[predictor.names != interaction.name]
    if (show.labels)
        labels <- Labels(data, names.to.lookup = predictor.names)
    else
        labels <- predictor.names
    return(list(data = data,
                formula = formula,
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
createSuitableFormulaForJaccard <- function(outcome.name, predictor.names)
    formula(paste0(outcome.name, " ~ ", paste0(predictor.names, collapse = " + ")))


#' Function determines the predictor names of both numeric and factors that are aliased
#' and the same information at the factor level. E.g. suppose there are two aliased variables
#' X1 and X2 where X1 is a numeric var and X2 is a factor with three levels A, B and C where only
#' level C is the aliased level. Then the predictor names returned by this function would be "X1" and
#' "X2" while the level representation would be "X1" and "X2C". For later use the mapping for the
#' factor levels to their factor variable are determined via the formula terms and model matrix.
#' The aliased variables are determined using the \code{\link{alias}} function which requires either
#' an \code{\link{lm}}, \code{\link{aov}} input or a \code{\link{formula}} input. The latter is used
#' here.
#' @param input.formula The formula object for the regression
#' @param data The data associated with the formula
#' @param outcome.name The name of the outcome variable, it could be determined via the formula but for
#'  convenience the name is used here to simplify the input data.
#' @return A list that contains the following four elements \itemize{
#' \item \code{predictors.to.remove} A character vector of variables names that are identified as
#' aliased via the \code{\link{alias}} function and should be removed.
#' \item \code{aliased.variables} The character vector giving the same aliased variables but containing
#' the specific aliased levels for factor variables.
#' \item \code{predictor.classes} Character vector giving the class of predictor (factor or numeric)
#' \item \code{variable.classes} Character vector giving the class of predictor, at the granularity of
#' levels of each factor if possible.
#' }
#' @importFrom stats alias model.matrix
#' @noRd
determineAliased <- function(input.formula, data, outcome.name)
{
    # input formula converted to aov object in alias call, will complain
    # of factor outcome in computation of residuals in that call
    if (is(data[[outcome.name]], "factor"))
        data[[outcome.name]] <- unclass(data[[outcome.name]])
    # Filter the data using the outlier subset if it exists
    if ("non.outlier.data_GQ9KqD7YOf" %in% names(data))
    {
        data <- data[data$non.outlier.data_GQ9KqD7YOf, ]
        data <- data[names(data) != "non.outlier.data_GQ9KqD7YOf"]
    }
    # Determine which variables are aliased in the model and return NULL early if there are none.
    aliased.variables <- alias(input.formula, data = data)$Complete
    if (is.null(aliased.variables))
        return(NULL)
    # Determine which predictors are aliased with each other, coercing to list if necessary
    aliased.variables <- apply(aliased.variables, 1, function(x) names(which(x != 0)))
    if (!is(aliased.variables, "list"))
        aliased.variables <- as.list(aliased.variables)
    # Add aliased variables to their identified counterparts
    aliased.variables <- mapply(function(x, x.names) c(x, x.names), aliased.variables, names(aliased.variables),
                                SIMPLIFY = FALSE, USE.NAMES = FALSE)
    # Determine the variables in the formula
    formula.term.labels <- attr(terms(input.formula, data = data), "term.labels")
    # Determine all variables in contrast form as required
    relevant.model.matrix <- model.matrix(input.formula, data = data)
    model.mapping <- attr(relevant.model.matrix, "assign")
    names(model.mapping) <- colnames(relevant.model.matrix)
    # Reconcile the aliased predictors/contrasts with the predictors in the formula
    aliased.variables <- lapply(aliased.variables, function(x) unique(formula.term.labels[model.mapping[x]]))
    # Remove duplicates
    duplicated.entries <- duplicated(unname(aliased.variables))
    if (any(duplicated.entries))
        aliased.variables <- unname(aliased.variables[!duplicated.entries])
    # Determine the class of the predictors and the levels
    grouped.predictors <- lapply(aliased.variables,
                                 function(x) { # Use is instead of class to catch ordered factors as well.
                                     vapply(data[x], function(y) if (is(y, "factor")) "factor" else "numeric",
                                            character(1))
                                })
    grouped.predictors
}

#' Helper function to throw an informative error when a user attempts to conduct a Relative Importance
#' Analysis or Shapley Regression when aliased predictors are in their dataset.
#' @param alaised.grouped List containing named character vectors. Each character vector describes the
#' group of aliased predictors where each character element gives the class of the predictor
#' and the name gives the variable name of each predictor.
#' @param labels Named character vector with elements being the variable labels and names being the variable
#' names
#' @noRd
throwAliasedErrorImportanceAnalysis <- function(aliased.grouped, labels, output)
{
    # Determine if there are any factors that are aliased
    factors.aliased <- vapply(aliased.grouped, function(x) any(x == "factor"), logical(1))
    if (any(factors.aliased))
    {
        factor.variables <- lapply(aliased.grouped[factors.aliased], function(x) labels[names(which(x == "factor"))])
        factor.msg <- paste0(" The linearly dependent relationship with the categorical variables ",
                             " occurs in the dummy coding of at least one of the contrast levels for each categorical variable.")
    } else
        factor.msg <- NULL
    base.error.message <- paste0("Some predictors are linearly dependent on other predictors and cannot ",
                                 "be estimated if they are included in the model together. ")
    groups <- vapply(aliased.grouped, function(x) paste0(sQuote(labels[names(x)], q = FALSE),
                                                         collapse = ", "), character(1))
    if (length(groups) == 1)
        group.msg <- paste0("The following group of predictors: (", groups, ") have a linearly dependent ",
                            "relationship and at least one of them needs to be removed to conduct a ",
                            output, ".")
    else
        group.msg <- paste0("In each of the following groups of predictors there is a linearly dependent relationship ",
                            "and at least one predictor needs to be removed to conduct a ", output, ". ",
                            paste0("Group ", 1:length(groups), ": (", groups, ")", collapse = ", "), ".")
    stop(base.error.message, group.msg, factor.msg)
}
