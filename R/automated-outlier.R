# Helper function to check user has input a valid value.
#' @importFrom flipU StopForUserError
checkAutomaterOutlierRemovalSetting <- function(outlier.prop.to.remove, estimation.data)
{
    remove.outliers <- !is.null(outlier.prop.to.remove) && outlier.prop.to.remove > 0
    if (remove.outliers && outlier.prop.to.remove >= 0.5)
        StopForUserError("At most, 50% of the data can be removed as part of the Automated Outlier Removal process. ",
                         FormatAsPercent(outlier.prop.to.remove), " of outliers were asked to be removed, please set this ",
                         " to a lower setting and re-run the analysis.")
    n <- nrow(estimation.data)
    p <- ncol(estimation.data)
    outlier.prop.to.remove <- if (is.null(outlier.prop.to.remove)) 0 else outlier.prop.to.remove
    if (floor(n * (1 - outlier.prop.to.remove)) < p + 1 && outlier.prop.to.remove > 0)
        StopForUserError(warningSampleSizeTooSmall(), " If ", outlier.prop.to.remove * 100, "% of the outlying data is ",
                         "removed there will be less data than parameters to predict in the model which is not possible. ",
                         " Consider a simpler model with less parameters or change the automated outlier removal setting ",
                         " to a smaller value.")
    remove.outliers
}

# Identifies the outliers and refits the model
#' @importFrom flipU InterceptExceptions StopForUserError
refitModelWithoutOutliers <- function(model, formula, .estimation.data, .weights,
                                      type, robust.se, outlier.prop.to.remove,
                                      dummy.processed.data, seed, ...)
{
    non.outlier.data <- findNonOutlierObservations(model, outlier.prop.to.remove, seed)
    if (!is.null(dummy.processed.data))
    {
        basic.formula <- dummy.processed.data[["formulae"]][["formula"]]
        formula.with.interaction <- dummy.processed.data[["formulae"]][["formula.with.interaction"]]
        relevant.subset <- dummy.processed.data[["post.missing.data.estimation.sample"]]
        data.with.missing <- dummy.processed.data[["data"]][relevant.subset, ][non.outlier.data, ]
        processed.data <- EstimationData(basic.formula, data.with.missing, missing = "Dummy variable adjustment")
        aliased.or.non.outlier <- setdiff(names(.estimation.data), names(processed.data$estimation.data))
        aliased <- setdiff(aliased.or.non.outlier, "non.outlier.data_GQ9KqD7YOf")
        new.estimation.data <- .estimation.data
        for (var in setdiff(names(.estimation.data), c(aliased.or.non.outlier, OutcomeName(formula))))
            new.estimation.data[non.outlier.data, var] <- processed.data[["estimation.data"]][[var]]
        if (length(aliased) > 0) {
            new.estimation.data[aliased] <- NULL
        }
        interaction.requested <- !is.null(dummy.processed.data[["interaction.name"]])
        if (interaction.requested)
            new.estimation.data[[dummy.processed.data[["interaction.name"]]]] <- dummy.processed.data[["interaction"]][relevant.subset]
        dummy.vars.left <- names(new.estimation.data)[isDummyVariable(names(new.estimation.data))]
        if (length(dummy.vars.left) > 0)
        {
            # Copy attributes for mapping
            for (dummy.var in dummy.vars.left)
                attr(new.estimation.data[[dummy.var]], "predictors.matching.dummy") <-
                    attr(processed.data[["estimation.data"]][[dummy.var]], "predictors.matching.dummy")
            # Update the formulae
            dummy.vars.left <- paste0(dummy.vars.left, collapse = " + ")
            formula <- update(terms(basic.formula, data = new.estimation.data),
                              as.formula(paste(". ~ . + ", dummy.vars.left, collapse = "")))
        }
        .estimation.data <- new.estimation.data
    }
    .estimation.data$non.outlier.data_GQ9KqD7YOf <- non.outlier.data
    # svyolr is fragile and errors if ordered response values have empty levels since the
    # Hessian is singular. Removing data with automated outlier removal can trigger this.
    if (type == "Ordered Logit" && !is.null(.weights))
    {
        fitted.model <- InterceptExceptions(
            fitModel(formula, .estimation.data, .weights = .weights,
                     type, robust.se, subset = non.outlier.data, ...),
            error.handler = function(e) {
                if (grepl("Outcome variable has level", e$message))
                {
                    missing.levels <- regmatches(e$message,
                                                 regexpr("(?<=level\\(s\\): )(.*?)(?= that)", e$message, perl = TRUE))
                    StopForUserError("Removing outliers has removed all the observations in the outcome variable with level(s): ",
                                     missing.levels,  ". If possible, this issue could be solved by merging the categories of the",
                                     " outcome variable or reducing the Automated Outlier removal setting.")
                }
                else
                    stop(e$message)
            }
        )
    } else
        fitted.model <- fitModel(formula, .estimation.data, .weights = .weights,
                                 type, robust.se, subset = non.outlier.data, ...)
    return(list(model = fitted.model$model, .estimation.data = .estimation.data,
                design = fitted.model$design, non.outlier.data = non.outlier.data))
}

# Returns a logical vector of observations that are not deemed outliers
findNonOutlierObservations <- function(model, outlier.prop.to.remove, seed = 12321)
{
    model.residuals <- computeAutomatedResiduals(model, seed)
    n.model <- nrow(model[["model"]])
    bound <- ceiling(n.model * (1 - outlier.prop.to.remove))
    valid.data.indices <- unname(rank(abs(model.residuals), ties.method = "random") <= bound)
    return(valid.data.indices)
}

#' @importFrom stats rstudent
computeAutomatedResiduals <- function(model, seed = 12321) {
    type <- getModelType(model)
    weighted <- isWeightedModel(model)
    if (type == "Ordered Logit" && weighted) {
        assign(".design", model[["design"]], envir = .GlobalEnv)
        assign(".formula", model[["formula"]], envir = .GlobalEnv)
        on.exit({
            remove(".design", envir = .GlobalEnv)
            remove(".formula", envir = .GlobalEnv)
        })
    }
    # use standardised deviance residuals in unweighted cases.
    # otherwise use the Pearson sampling re-weighted residuals.
    # These are computed in residuals.svyglm except in the NBD case.
    # NBD kept as Pearson for consistency.
    switch(type,
           Linear = ,
           Poisson = ,
           `Quasi-Poisson` = ,
           NBD = if (weighted) residuals(model, type = "pearson") else rstudent(model, type = "deviance"),
           # In the Ordered Logit and Binary Logit cases use the Surrogate residuals
           # for both the weighted and non-weighted models
           `Binary Logit` = computeBinaryLogitResiduals(model, seed),
           `Ordered Logit` = computeOrderedLogitResiduals(model, seed),
           stop("Unexpected or unsupported regression for automated outlier removal type: ", type))
}

#' @importFrom sure resids
computeBinaryLogitResiduals <- function(model, seed) {
    set.seed(seed)
    resids(model, method = "jitter", type = "response")
}

#' @importFrom sure resids
computeOrderedLogitResiduals <- function(model, seed) {
    set.seed(seed)
    resids(model, method = "latent")
}
