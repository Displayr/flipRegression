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
#' @importFrom verbs Sum
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
        non.binary.variable.msg <- ngettext(Sum(c(!outcome.is.binary, !predictors.are.binary), remove.missing = FALSE),
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
        n.variables <- Sum(c(outcome.no.variation, predictors.no.variation), remove.missing = FALSE)
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
    aliased.variables <- data.frame(t(aliased.variables), check.names = FALSE)
    aliased.variable.names <- names(aliased.variables)
    var.deps <- row.names(aliased.variables)
    aliased.variables <- lapply(aliased.variables, function(x) var.deps[which(x != 0)])
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
#' @param group.name Character string or \code{NULL}. If character, then the error message is prepended
#' with a string about the \code{group.name}, useful for crosstab interaction analysis output.
#' If \code{MULL}, then it is assumed that the analysis is for the entire dataset and an error is thrown without
#' any group description and solely describes the aliased predictors.
#' @noRd
throwAliasedExceptionImportanceAnalysis <- function(aliased.grouped, labels, output, group.name = NULL)
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
    base.message <- paste0("Some predictors are linearly dependent on other predictors and cannot ",
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
    if (is.null(group.name))
        stop(base.message, group.msg, factor.msg)
    else
    {
        base.message <- paste0("Within the ", group.name, " category, ", sub("^S", "s", base.message))
        stop(base.message, group.msg, factor.msg)
    }
}

#' Throw an informative error or construct string
#' @param input.formula The formula for the desired model
#' @param estimation.data The estimation.data including outlier identifier logical column
#' @param outcome.name Character string of the outcome variable name (not label) to look up in the input data.
#' @param show.labels Logical whether to show labels or names in the output message/error.
#' @param output The type of output desired, either \code{"Shapley Regression"} or \code{"Relative Importance Analysis"}
#' @param group.name Either a character string or \code{NULL} to pass to \code{throwAliasedExceptionImportanceAnalysis}
#' If \code{NULL}, a error is thrown and is intended to be used when analysis of entire dataset is conducted. When a character
#' string is used here, it is expected to refer to a specific level in a crosstab interaction that might have aliasing problems
#' and a warning is given to the user.
#' @noRd
validateDataForRIA <- function(input.formula, estimation.data, outcome.name, show.labels, output, group.name = NULL)
{
    # Check to see if there are columns with no variation
    validateVariablesHaveVariation(input.formula, estimation.data, outcome.name, output, show.labels, group.name)
    # Check to see if there are linearly dependent predictors
    aliased.processed <- determineAliased(input.formula, estimation.data, outcome.name)
    if (is(aliased.processed, "list"))
    { # Create mapping of predictor names to their labels if necessary for error message
        formula.names <- AllVariablesNames(input.formula, estimation.data)
        formula.labels <- if (show.labels) Labels(estimation.data, formula.names) else formula.names
        names(formula.labels) <- formula.names
        throwAliasedExceptionImportanceAnalysis(aliased.processed, formula.labels, output, group.name)
    }
}

syntactic.name.patt <- "^((([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*)|[.])$"
dataset.reference.patt <- "^[[:print:]]*[$](Variables|Questions)[$]"

removeDataSetReferences <- function(vars.to.relabel)
{
    new.var.names <- names(vars.to.relabel)
    new.var.names <- sub(dataset.reference.patt, "", new.var.names)
    names(new.var.names) <- names(vars.to.relabel)
    new.var.names
}

#' Function determines whether the outcome variable of predictors dont have any variation. Used to validate
#' the data before use in Shapley Regression or RIA.
#' @param input.formula The formula object for the regression. Used to determine predictor names
#' @param data The estimation data, used to apply outlier filter if necessary
#' @param outcome.name The name of the outcome variable, it could be determined via the formula but for
#'  convenience the name is used here to simplify the input data.
#' @param output String describing the output type. Typically Shapley Regression or Relative Importance Analysis
#' @param show.labels Logical to determine whether to use names or labels in error messages.
#' @param group.name String of the category label when used in crosstab interaction.
#' @importFrom flipU AllVariablesNames
#' @noRd
validateVariablesHaveVariation <- function(input.formula, data, outcome.name, output, show.labels, group.name = NULL)
{
    # Filter the data using the outlier subset if it exists
    if ("non.outlier.data_GQ9KqD7YOf" %in% names(data))
    {
        data <- data[data$non.outlier.data_GQ9KqD7YOf, ]
        data <- data[names(data) != "non.outlier.data_GQ9KqD7YOf"]
    }
    outcome <- data[[outcome.name]]
    outcome.label <- if (show.labels) Labels(data, outcome.name) else outcome.name
    outcome.label <- sQuote(outcome.label, q = FALSE)
    outcome.msg <- paste0("The outcome variable, ", outcome.label, ", is constant and has no variation. The outcome needs ",
                          "to have at least two unique values to conduct a ", output)
    # Check predictor has no variation if numeric or factor.
    .hasNoVariation <- function(x) if (is(x, "factor")) all(duplicated(x)[-1L]) else var(x, na.rm = TRUE) == 0
    outcome.no.variation <- .hasNoVariation(outcome)
    if (outcome.no.variation)
        throwRIAException(outcome.msg, group.name)
    # Outcome variable should be fine from here. Inspect the predictors
    predictors <- data[AllVariablesNames(input.formula, data)]
    predictors <- predictors[names(predictors) != outcome.name]
    no.variation.vars <- vapply(predictors, .hasNoVariation, logical(1))
    if (any(no.variation.vars))
    {
        no.variation.vars <- names(which(no.variation.vars))
        pred.labels <- if (show.labels) Labels(data, no.variation.vars) else no.variation.vars
        pred.labels <- sQuote(pred.labels, q = FALSE)
        if (length(pred.labels) == 1)
            no.variation.msg <- paste0("The predictor ", pred.labels, " is constant and has no variation.")
        else
            no.variation.msg <- paste0("The following predictors are constant and have no variation: ",
                                       paste0(pred.labels, collapse = ", "), ".")
        no.variation.msg <- paste0("Each predictor needs to have at least two unique values to ",
                                   "conduct a ", output, ". ", no.variation.msg)
        throwRIAException(no.variation.msg, group.name)
    }
}

#' Throw the error or prepend the error message with a relevant category name reference for the cross
#' tab interaction
#' @param x The string to pass into the error
#' @param group.name The string of the category label/group name within the cross tab interaction,
#' @noRd
throwRIAException <- function(x, group.name = NULL)
{
    if (is.null(group.name))
        stop(x)
    else
    { # Change first char to lowercase and prepend the group/category name
        first.char <- substr(x, 1, 1)
        x <- sub(paste0("^", first.char), tolower(first.char), x)
        stop("Within the ", group.name, " category, ", x)
    }
}

#' Checks the provided character vecvtor of variable names use dataset references. e.g. \code{TRUE} if \code{`My data`$Variables$`Some Y`} and
#' \code{FALSE} if a variable doesn't have dataset references.
#' @param all.variable.names Variable names to be checked
#' @return A named logical vector, with length (p + 1) where p is the number of predictors (1 extra for the
#' outcome name). The values are \code{TRUE} if dataset reference found, \code{FALSE} otherwise.
#' @noRd
checkVariablesForDataSetNames <- function(all.variable.names)
{
    dataset.refs.found <- grepl(pattern = dataset.reference.patt, all.variable.names)
    names(dataset.refs.found) <- all.variable.names
    dataset.refs.found
}

#' Updates the variables names for a formula and its associated data.
#' @param new.var.names Relevant named character vector. The elements of the vector contain
#'  the new desired labels and the names of the vector contain the old labels, hence providing
#'  the appropriate mapping.
#' @param formula Relevant \code{data.frame} used in the regression
#' @param data Relevant \code{data.frame} used in the regression
#' @return list that has an updated formula, data and outcome name without any dataset references
#' @noRd
relabelFormulaAndData <- function(new.var.names, formula, data)
{
    new.var.names <- gsub(" ", "_", new.var.names)
    data <- updateAttribute(data, attr.to.update = "name", updated.values = new.var.names)
    outcome.name <- new.outcome.name <- new.var.names[1]
    new.predictor.names <- new.var.names[-1]
    formula <- update(formula, as.formula(paste0(new.outcome.name, " ~ ",
                                                 paste0(new.predictor.names, collapse = " + "),
                                                 collapse = "")))
    # Match data names to the appropriate new names
    cols.to.update <- match(names(new.var.names), colnames(data), nomatch = 0)
    names(data)[cols.to.update] <- new.var.names

    list(formula = formula, data = data, outcome.name = unname(outcome.name))
}

#' Checks the provided character vector contains syntactic names by comparing the output of make.names.
#' If the names are syntactic, the return output is NULL. Otherwise a mapped character vector with syntactic
#' names is provided (syntactic names are the vector elements, old names are the vector element names)
#' @noRd
checkForNonSyntacticNames <- function(variable.names)
{
    syntactic.names <- make.names(variable.names, unique = TRUE)
    if (non.syntactic.names <- !identical(variable.names, syntactic.names))
    {
        names(syntactic.names) <- variable.names
        return(syntactic.names)
    }
    return(NULL)
}

#' Look up name of the provided data set or derive it from the provided variable names if required.
#' @noRd
lookupDataSetNames <- function(variable.names, data)
{
    dataset.names <- vapply(variable.names,
                            function(x) if (!is.null(dataset.found <- attr(data[[x]], "dataset")))
                                            dataset.found
                                        else NA_character_,
                            character(1L))
    if (anyNA(dataset.names))
    {
        missing.dataset.names <- is.na(dataset.names)
        dataset.names[missing.dataset.names] <- sub("[$](Variables|Questions)[$].*", "", variable.names[missing.dataset.names])
        dataset.names[missing.dataset.names] <- gsub("`", "", dataset.names[missing.dataset.names])
    }
    dataset.names
}

#' @param data data.frame of data to be updated
#' @param attr.to.update The name of the attribute to update (attribute declared at the variable level not data.frame level).
#' @param updated.vales A named vector of values to update. The elements contain the new attribute values. The names contain the
#'   variables to lookup inside the data.frame.
#' @noRd
updateAttribute <- function(data, attr.to.update, updated.values)
{
    for (var in names(updated.values))
        if (!is.null(attr(data[[var]], attr.to.update)))
            attr(data[[var]], attr.to.update) <- unname(updated.values[var])
    data
}
