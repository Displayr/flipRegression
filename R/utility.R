# Returns a logical whether the model has been weighted or not
isWeightedModel <- function(model) {
    # Examine the actual model if a Regression output
    if (inherits(model, "Regression"))
        return(Recall(model[["original"]]))
    # If model created using survey, then it is weighted
    if (issvyglm(model) || inherits(model, "svyolr"))
        return(TRUE)
    # Check if the (weights) variable exists if created using glm.nb
    if (inherits(model, "negbin") && !is.null(model[["model"]][["(weights)"]]))
        return(TRUE)
    # Check if there are any non-unit weights in multinom output
    inherits(model, "multinom") && any(model[["weights"]] != 1L)
}

# Takes a regression model and returns the type of model
#' @importFrom stats family
getModelType <- function(model) {
    # Examine the actual model if a Regression output
    if (inherits(model, "Regression"))
        return(Recall(model[["original"]]))
    # Examine distribution family if model is a GLM
    if (inherits(model, "glm")) {
        model.family <- family(model)[["family"]]
        if (startsWith(model.family, "Negative")) return("NBD")
        return(switch(model.family,
                      quasibinomial = ,
                      binomial = "Binary Logit",
                      gaussian = "Linear",
                      poisson = "Poisson",
                      quasipoisson = "Quasi-Poisson"))
    }
    # Needs to occur after
    if (inherits(model, "lm"))
        return("Linear")
    if (inherits(model, "multinom"))
        return("Multinomial Logit")
    # Must be polr from here
    "Ordered Logit"
}

# Convert the regression call to a list of arguments, then evaluate the arguments
# in the grandparent environment. This allows the arguments to be evaluated since this
# function should be called within Regression itself. Then add the default arguments
# to the list. Some arguments are not evaluated such as the subset, weights and
# interaction arguments.
#' @importFrom stats setNames
validateRegressionArguments <- function(regression.call) {
    regression.call[[1]] <- quote(list)
    # Subset, weights or interaction might be emebedded in the data
    # and not able to evaluated at the start
    if (!is.null(regression.call[["weights"]]))
        regression.call[["weights"]] <- NULL
    if (!is.null(regression.call[["interaction"]]))
        regression.call[["interaction"]] <- NULL
    if (!is.null(regression.call[["subset"]]))
        regression.call[["subset"]] <- NULL
    # Evaluate the regression call in the parent environment to validate parameters
    regression.args <- eval.parent(regression.call, n = 2L)
    # Add the default arguments
    all.args <- formals(Regression)
    default.args <- setdiff(names(all.args), c(names(regression.args), "..."))
    if (length(default.args) > 0)
        regression.args <- c(regression.args, setNames(eval(all.args[default.args]), default.args))
    # Validate the full list of arguments
    validateFormulaArgument(regression.args)
    validateRegressionTypeArgument(regression.args)
    validateMissingValueArgument(regression.args)
    validateOutlierRemovalArgument(regression.args)
    validateStatisticalAssumptionsArgument(regression.args)
}

isFormula <- function(formula) {
    is.call(formula) && length(formula) == 3L && formula[[1]] == quote(`~`)
}

#' @importFrom flipU StopForUserError
validateFormulaArgument <- function(regression.args) {
    # If stacked data is provided, then the formula is not required
    stacked.data.check <- regression.args[["stacked.data.check"]]
    if (isTRUE(stacked.data.check)) return()
    # Check formula
    formula <- regression.args[["formula"]]
    if (!isFormula(formula))
        StopForUserError(dQuote("formula"), " argument is not a formula and is required unless stackable data is provided via the ",
                         dQuote("stacked.data.check"), " and ", dQuote("unstacked.data"), " arguments. ",
                         "Please provide a formula or stackable data and re-run the Regression.")
}

# Regression argument checking
#' @importFrom flipU StopForUserError
validateOutlierRemovalArgument <- function(regression.args) {
    outlier.prop.to.remove <- regression.args[["outlier.prop.to.remove"]]
    if (is.null(outlier.prop.to.remove)) return()
    if (!is.numeric(outlier.prop.to.remove) || length(outlier.prop.to.remove) != 1L)
        StopForUserError(dQuote("outlier.prop.to.remove"), " should be a single numeric value.")
    if (outlier.prop.to.remove < 0 || outlier.prop.to.remove > 1)
        StopForUserError(dQuote("outlier.prop.to.remove"), " should be between 0 and 1.")
}

#' @importFrom flipU StopForUserError
validateStatisticalAssumptionsArgument <- function(regression.args) {
    if (!identical(regression.args[["statistical.assumptions"]], alist(, )[[1]]))
        StopForUserError("'statistical.assumptions' objects are not yet supported.")
}

#' @importFrom flipU StopForUserError
validateRobustStandardErrorsArgument <- function(regression.args) {
    robust.standard.errors <- regression.args[["robust.standard.errors"]]
    if (is.null(robust.standard.errors)) return()
    if (!is.logical(robust.standard.errors) || length(robust.standard.errors) != 1L)
        StopForUserError(dQuote("robust.standard.errors"), " should be a single logical value.")
}

#' @importFrom flipU StopForUserError
validateRegressionArg <- function(regression.args, arg.name, valid.values) {
    arg <- regression.args[[arg.name]]
    if (!is.character(arg) || length(arg) != 1L)
        StopForUserError(dQuote(arg.name), " should be a single character value.")
    if (!arg %in% valid.values)
        throwErrorInvalidArgument(arg.name)
}

valid.arguments <- list(
    type = c("Linear", "Binary Logit", "Poisson", "Quasi-Poisson", "Ordered Logit", "Multinomial Logit", "NBD"),
    missing = c("Error if missing data", "Exclude cases with missing data", "Dummy variable adjustment",
                "Use partial data (pairwise correlations)", "Imputation (replace missing values with estimates)",
                "Multiple imputation")
)

#' @importFrom flipU StopForUserError
validateRegressionTypeArgument <- function(regression.args) {
    validateRegressionArg(regression.args, "type", valid.arguments[["type"]])
    type <- regression.args[["type"]]
    robust.se <- regression.args[["robust.se"]]
    not.linear <- type != "Linear"
    if (not.linear && isTRUE(robust.se))
        StopForUserError("Robust standard errors are only supported for Linear regression.")
    output <- regression.args[["output"]]
    if (not.linear && startsWith(output, "Shapley"))
        StopForUserError("Shapley requires Regression type to be Linear. Set the output to ",
                         "Relative Importance Analysis instead.")
}

#' @importFrom flipU StopForUserError
throwErrorInvalidArgument <- function(arg.name) {
    valid.parameters <- valid.arguments[[arg.name]]
    StopForUserError(sQuote(arg.name), " should be one of ",
                     paste0(dQuote(valid.parameters), collapse = ", "), ".")
}

#' @importFrom flipU StopForUserError
validateMissingValueArgument <- function(regression.args) {
    validateRegressionArg(regression.args, "missing", valid.arguments[["missing"]])
    # Check combination of missing value handling and outlier removal
    outlier.prop.to.remove <- regression.args[["outlier.prop.to.remove"]]
    missing <- regression.args[["missing"]]
    if (missing == "Multiple imputation" && !is.null(outlier.prop.to.remove) && outlier.prop.to.remove > 0)
        StopForUserError("Multiple imputation is not supported with automated outlier removal. ",
                         "Either change the missing value handling option or set the Automated outlier ",
                         "removal percentage to zero")
    internal <- isTRUE(regression.args[["internal"]])
    partial <- missing == "Use partial data (pairwise correlations)"
    if (internal && partial)
        StopForUserError("'internal' may not be selected with regressions based on correlation matrices.")
    not.linear <- regression.args[["type"]] != "Linear"
    if (not.linear && partial)
        StopForUserError("Use partial data (pairwise correlations) is only supported for Linear regression.")
    if (isTRUE(regression.args[["robust.se"]]) && (partial || missing == "Multiple imputation"))
        StopForUserError("Robust standard errors cannot be computed with 'missing' set to ", missing, ".")
}
