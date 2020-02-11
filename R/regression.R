#' \code{Regression}
#' @description Generalized Regression. Computes output for seven different regression types.
#' Those being linear, binary logistic, ordered logistic, binomial, poisson, quasi-poisson and
#' multinomial. Output includes general coefficient estimates and importance analysis estimates
#' with possibilities for handling missing data and interaction terms.
#' @param formula An object of class \code{\link{formula}} (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#'   The details of type specification are given under \sQuote{Details}.
#' @param data A \code{\link{data.frame}}.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param missing How missing data is to be treated in the regression. Options:
#'   \code{"Error if missing data"},
#'   \code{"Exclude cases with missing data"},
#'   \code{"Use partial data (pairwise correlations)"},
#'   \code{"Imputation (replace missing values with estimates)"}, and
#'   \code{"Multiple imputation"}.
#' @param type Defaults to \code{"Linear"}. Other types are: \code{"Poisson"},
#'   \code{"Quasi-Poisson"}, \code{"Binary Logit"}, \code{"NBD"},
#'   \code{"Ordered Logit"}, and \code{"Multinomial Logit"}
#' @param robust.se If \code{TRUE}, computes standard errors that are robust to violations of
#'   the assumption of constant variance for linear models, using the HC3 modification of White's (1980) estimator
#'   (Long and Ervin, 2000). This parameter is ignored if weights are applied (as weights already
#'   employ a sandwich estimator). Other options are \code{FALSE} and \code{"FALSE"No}, which do the same
#'   thing, and \code{"hc0"}, \code{"hc1"}, \code{"hc2"}, \code{"hc4"}.
#' @param output \code{"Coefficients"} returns a table of coefficients and various
#'   summary and model statistics. It is the default.
#'   \code{"ANOVA"} returns an ANOVA table.
#'   \code{"Detail"} returns a more traditional R output.
#'   \code{"Relative Importance Analysis"} returns a table with Relative Importance scores.
#'   \code{"Shapley Regression"} returns a table with Shapley Importance scores.
#'   \code{"Effects Plot"} returns the effects plot per predictor.
#' @param detail This is a deprecated function. If \code{TRUE}, \code{output} is set to \code{R}.
#' @param method The method to be used; for fitting. This will only do something if
#'   method = "model.frame", which returns the model frame.
#' @param m The number of imputed samples, if using multiple imputation.
#' @param seed The random number seed used in imputation.
#' @param statistical.assumptions A Statistical Assumptions object.
#' @param auxiliary.data A \code{\link{data.frame}} containing additional variables
#'   to be used in imputation (if required). While adding more variables will improve
#'   the quality of the imputation, it will dramatically slow down the time to estimate.
#'   Factors and Character variables with a large number of categories should not be included,
#'   as they will both slow down the data and are unlikely to be useful
#' @param show.labels Shows the variable labels, as opposed to the names, in the outputs, where a
#'   variables label is an attribute (e.g., attr(foo, "label")).
#' @param internal If \code{TRUE}, skips most of the tidying at the end. Only for use when it is
#'   desired to call a relatively light version of Regression for other purposes (e.g., in ANOVA).
#'   This leads to creation of an object of class \code{FitRegression}.)
#' @param contrasts A vector of the contrasts to be used for \code{\link{factor}} and
#'   \code{\link{ordered}} variables. Defaults to \code{c("contr.treatment", "contr.treatment"))}.
#'   Set to \code{c("contr.treatment", "contr.poly"))} to use orthogonal polynomials for \code{\link{factor}}
#'   See \code{\link{contrasts}} for more information.
#' @param interaction Optional variable to test for interaction with other variables in the model. Output will be a crosstab showing coefficients from both both models.
#' @param relative.importance Deprecated. To run Relative Importance Analysis, use the output variable.
#' @param importance.absolute Whether the absolute value of the relative importance should be shown.
#' @param correction Method to correct for multiple comparisons. Can be one of \code{"None"},
#'   \code{"False Discovery Rate", "Benjamini & Yekutieli", "Bonferroni", "Hochberg", "Holm"} or \code{"Hommel"}.
#' @param interaction.formula Used internally for multiple imputation.
#' @param recursive.call Used internally to indicate if call is a result of recursion (e.g., multiple imputation).
#' @param effects.format A list of items \code{max.label} (the maximum length of a factor label on the x-axis)
#'   and \code{y.title} (the title of the y-axis, defaults to outcome label).
#' @param ... Additional argments to be passed to  \code{\link{lm}} or, if the
#'  data is weighted,  \code{\link[survey]{svyglm}} or \code{\link[survey]{svyolr}}.
#' @details In the case of Ordered Logistic regression, this function computes a proporional odds model using
#'  the cumulative link (logistic). In the case of no weights, the \code{\link[MASS]{polr}} function is used.
#'  In the case of a weighted regresion, the \code{\link[survey]{svyolr}} function is used.
#'
#'  "Imputation (replace missing values with estimates)". All selected
#'  outcome and predictor variables are included in the imputation, along with
#'  all \code{auxiliary.data}, excluding cases that are excluded via subset or
#'  have invalid weights, but including cases with missing values of the outcome variable.
#'  Then, cases with missing values in the outcome variable are excluded from
#'  the analysis (von Hippel 2007). See \code{\link[flipImputation]{Imputation}}.
#' @references von Hippel, Paul T. 2007. "Regression With Missing Y's: An
#'   Improved Strategy for Analyzing Multiply Imputed Data." Sociological
#'   Methodology 37:83-117.
#'
#'   White, H. (1980), A heteroskedastic-consistent  covariance matrix estimator
#'   and a direct test of heteroskedasticity. Econometrica, 48, 817-838.
#'
#'   Long, J. S. and Ervin, L. H. (2000). Using heteroscedasticity consistent
#'   standard errors in the linear regression  model. The American Statistician, 54(3): 217-224.
#'
#'   Lumley, T. (2004) Analysis of complex survey samples. Journal of Statistical Software 9(1): 1-19
#'
#' @importFrom stats pnorm anova update terms
#' @importFrom flipData GetData CleanSubset CleanWeights DataFormula
#' EstimationData CleanBackticks RemoveBackticks ErrorIfInfinity
#' @importFrom flipFormat Labels OriginalName
#' @importFrom flipU OutcomeName IsCount
#' @importFrom flipTransformations AsNumeric
#' CreatingBinaryDependentVariableIfNecessary Factor Ordered
#' @importFrom lmtest coeftest
#' @importFrom utils tail
#' @importFrom stats drop.terms terms.formula
#' @export
Regression <- function(formula,
                       data = NULL,
                       subset = NULL,
                       weights = NULL,
                       missing = "Exclude cases with missing data",
                       type = "Linear",
                       robust.se = FALSE,
                       method = "default",
                       output = "Coefficients",
                       detail = FALSE,
                       m = 10,
                       seed = 12321,
                       statistical.assumptions,
                       auxiliary.data = NULL,
                       show.labels = FALSE,
                       internal = FALSE,
                       contrasts = c("contr.treatment", "contr.treatment"),
                       relative.importance = FALSE,
                       importance.absolute = FALSE,
                       interaction = NULL,
                       correction = "None",
                       interaction.formula = NULL,     # only non-NULL in multiple imputation inner loop
                       recursive.call = FALSE,
                       effects.format = list(max.label = 10),
                       ...)
{
    old.contrasts <- options("contrasts")
    options(contrasts = contrasts)
    if (detail || output == "Detail")
        output <- "R"
    if (robust.se == "No")
        robust.se <- FALSE
    cl <- match.call()
    if(!missing(statistical.assumptions))
        stop("'statistical.assumptions' objects are not yet supported.")

    importance <- if (output == "Relative Importance Analysis" || relative.importance)
        "Relative Importance Analysis"
    else if (output == "Shapley regression" || output == "Shapley Regression")
    {
        output <- "Shapley Regression"
        if (type == "Linear")
            "Shapley Regression"
        else
            stop("Shapley requires Regression type to be Linear. Set the output to ",
                 "Relative Importance Analysis instead.")
    }
    else
        NULL

    if (!is.null(importance) && is.null(importance.absolute))
        importance.absolute <- FALSE

    input.formula <- formula # Hack to work past scoping issues in car package: https://cran.r-project.org/web/packages/car/vignettes/embedding.pdf.
    subset.description <- try(deparse(substitute(subset)), silent = TRUE) #We don't know whether subset is a variable in the environment or in data.
    subset <- eval(substitute(subset), data, parent.frame())
    if (!is.null(subset))
    {
        if (is.null(subset.description) | (class(subset.description) == "try-error") | !is.null(attr(subset, "name")))
            subset.description <- Labels(subset)
        if (is.null(attr(subset, "label")))
            attr(subset, "label") <- subset.description
    }
    if (!is.null(list(...)$weights))
        weights <- list(...)$weights
    weight.name <- deparse(substitute(weights))
    weights <- eval(substitute(weights), data, parent.frame())

    interaction.name <- deparse(substitute(interaction))
    interaction <- eval(substitute(interaction), data, parent.frame())
    if (!is.null(interaction))
    {
        if (!is.null(attr(interaction, "name")))
            interaction.name <- attr(interaction, "name")
        interaction.label <- if (show.labels && is.character(Labels(interaction))) Labels(interaction)
        else interaction.name
    }

    if (!is.null(interaction.formula))
    {
        # Inside internal loop, data is already set up properly
        formula.with.interaction <- interaction.formula
        data <- GetData(interaction.formula, data, auxiliary.data)
        interaction.name <- tail(colnames(data), 1)
    }
    else
    {
        ## Includes interaction in formula if there is one
        ## stats::update.formula, does not accept a data arg,
        ## so update fails if dot on RHS of formula.
        ## Calling stats::terms first expands the dot so update works
        formula.with.interaction <- if (is.null(interaction)) input.formula
        else update(terms(input.formula, data = data),
                    sprintf(".~.*%s",interaction.name))
        data <- GetData(input.formula, data, auxiliary.data)
        if (!is.null(interaction))
        {
            interaction.label <- if (show.labels && is.character(Labels(interaction))) Labels(interaction)
            else interaction.name
            if (length(unique(Factor(interaction))) < 2)
                stop("Crosstab interaction variable must contain more than one unique value.")
            if (type == "Multinomial Logit")
                stop("Crosstab interaction is incompatible with Multinomial Logit regression.")

            if (interaction.name %in% colnames(data))
                stop("The 'Crosstab interaction' variable has been selected as a 'Predictor'")
            data <- cbind(data, Factor(interaction))
            colnames(data)[ncol(data)] <- interaction.name

            # No imputation occurs on interaction variable
            subset.description <- Labels(subset)
            tmp.sub <- !is.na(interaction)
            if (is.null(subset) || length(subset) <= 1)
            {
                subset <- tmp.sub
                attr(subset, "label") <- ""
            } else
            {
                subset <- subset & tmp.sub
                attr(subset, "label") <- subset.description
            }
        }
    }
    if (show.labels)
    {
        labels <- Labels(data)
        dup.labels <- unique(labels[duplicated(labels)])
        ind.dup <- which(labels %in% dup.labels)
        for (i in ind.dup)
            attr(data[[i]], "label") <- paste0(labels[i], " (", names(data)[i], ")")
    }

    if (method == "model.frame")
        return(data)
    if (output == "Effects Plot") # allEffects fails if names have backticks and are not syntactic
    {
        colnames(data) <- make.names(colnames(data))
        clean.formula <- make.names(input.formula)
        clean.formula <- paste(clean.formula[2], "~", gsub("...", "+", clean.formula[3], fixed = TRUE))
        formula.with.interaction <- input.formula <- as.formula(clean.formula)
    }
    outcome.name <- OutcomeName(input.formula, data)
    outcome.variable <- data[[outcome.name]]
    if(sum(outcome.name == names(data)) > 1)
        stop("The 'Outcome' variable has been selected as a 'Predictor'. It must be one or the other, but may not be both.")
    if (!is.null(weights) & length(weights) != nrow(data))
        stop("'weights' and 'data' are required to have the same number of observations. They do not.")
    if (!is.null(subset) & length(subset) > 1 & length(subset) != nrow(data))
        stop("'subset' and 'data' are required to have the same number of observations. They do not.")
    # Check if there are any entirely missing variables in the data and adjust accordingly.
    missing.variables <- apply(data, 2, function(x) all(is.na(x)))
    if(any(missing.variables))
    {
        missing.variable.adjustment <- removeMissingVariables(data, formula, formula.with.interaction,
                                                              missing.variables, outcome.name, input.formula)
        data <- missing.variable.adjustment$data
        formula <- missing.variable.adjustment$formula
        formula.with.interaction <- missing.variable.adjustment$formula.with.interaction
        input.formula <- missing.variable.adjustment$input.formula
    }
    if (type == "Binary Logit")
    {
        data <- CreatingBinaryDependentVariableIfNecessary(input.formula, data)
        outcome.variable <- data[[outcome.name]]
    }
    else if (type == "Ordered Logit")
        data[, outcome.name] <- Ordered(outcome.variable)
    else if (type == "Multinomial Logit")
        data[, outcome.name] <- Factor(outcome.variable)
    else if (IsCount(type) & !IsCount(outcome.variable))
        stopNotCount()
    else if (is.factor(outcome.variable))
    {
        WarningFactorToNumeric()
        data[, outcome.name] <- outcome.variable <- AsNumeric(outcome.variable, binary = FALSE)
    }
    row.names <- rownames(data)
    partial <- missing == "Use partial data (pairwise correlations)"
    if (robust.se != FALSE & (partial | missing == "Multiple imputation"))
        stop(paste0("Robust standard errors cannot be computed with 'missing' set to ", missing, "."))
    if (robust.se != FALSE & type != "Linear")
        stop("Robust standard errors may only be computed using Linear regressions.")
    if (partial)
    {
        if (internal)
            stop("'internal' may not be selected with regressions based on correlation matrices.")
        if (!is.null(importance))
            stop("Relative importance analysis and Shapley Regression are not ",
                 "available when using pairwise correlations on missing data.")
        subset <- CleanSubset(subset, nrow(data))
        unfiltered.weights <- weights <- CleanWeights(weights)
        if (type != "Linear")
            stop(paste0("'Use partial data (pairwise)' can only be used with 'type' of 'Linear'."))
        result <- list(original = LinearRegressionFromCorrelations(input.formula, data, subset,
                                                                   weights, outcome.name, ...),
                       call = cl)
        result$sample.description <- result$original$sample.description
    }
    else
    {
        processed.data <- EstimationData(formula.with.interaction, data, subset,
                                         weights, missing, m = m, seed = seed)

        data.for.levels <- if (missing == "Multiple imputation") processed.data$estimation.data[[1]] else processed.data$estimation.data
        ErrorIfInfinity(data.for.levels)
        data.for.levels <- data.for.levels[, !(names(data.for.levels) == outcome.name), drop = FALSE]
        if (ncol(data.for.levels) > 1)
        {
            variable.count <- sum(sapply(data.for.levels, function(x) abs(length(levels(x)) - 1)))
            n.data <- sum(processed.data$post.missing.data.estimation.sample)
            if (n.data < variable.count)
                stop(gettextf("There are fewer observations (%d)%s(%d)", n.data,
                              " than there are variables after converting categorical to dummy variables ", variable.count),
                     ". Either merge levels, remove variables, or convert categorical variables to numeric.")
        }

        if (missing == "Multiple imputation")
        {
            models <- lapply(processed.data$estimation.data,
                             FUN = function(x) Regression(formula,
                                                          data = x,                               # contains interaction factor; filters already applied
                                                          missing = "Error if missing data",
                                                          weights = processed.data$weights,
                                                          type = type,
                                                          robust.se = FALSE,
                                                          detail = detail,
                                                          show.labels = show.labels,
                                                          interaction = interaction,
                                                          interaction.formula = formula.with.interaction,
                                                          output = output,
                                                          correction = "None",
                                                          recursive.call = TRUE
                             ))

            models[[1]]$correction <- correction
            final.model <- models[[1]]
            final.model$outcome.label <- RemoveBackticks(outcome.name)
            if (show.labels)
            {
                label <- Labels(outcome.variable)
                if(!is.null(label))
                    final.model$outcome.label <- label
            }
            coefs <- MultipleImputationCoefficientTable(models)
            if (show.labels && type == "Multinomial Logit")
            {
                coef.labels <- colnames(final.model$summary$coefficients)
                kc <- length(coef.labels)
                alt.labels <- rownames(final.model$summary$coefficients)
                kr <- length(alt.labels)
                rownames(coefs) <- paste(alt.labels, coef.labels[rep(1:kc, rep(kr, kc))])
            }
            aliasedPredictorWarning(final.model$summary$aliased,
                                    if (show.labels) Labels(data, names(final.model$summary$aliased)) else NULL)

            final.model$coefficient.table <- coefs
            final.model$summary$coefficients  <- coefs[, -4]
            final.model$coef <- final.model$original$coef <- coefs[, 1]
            final.model$missing = "Multiple imputation"
            final.model$sample.description <- processed.data$description
            if (!is.null(interaction))
            {
                final.model$interaction <- multipleImputationCrosstabInteraction(models, importance)
                final.model$interaction$label <- interaction.label
            }
            final.model$footer <- regressionFooter(final.model)
            if (!is.null(importance) && is.null(interaction))
            {
                final.model$importance <- multipleImputationImportance(models,
                                                                       importance.absolute)
                final.model$importance.footer <- importanceFooter(final.model)
            }
            final.model <- setChartData(final.model, output)
            return(final.model)
        }

        unfiltered.weights <- processed.data$unfiltered.weights
        .estimation.data <- processed.data$estimation.data
        n <- nrow(.estimation.data)
        if (n < ncol(.estimation.data) + 1)
            stop(warningSampleSizeTooSmall())
        post.missing.data.estimation.sample <- processed.data$post.missing.data.estimation.sample
        .weights <- processed.data$weights
        subset <-  processed.data$subset
        .formula <- DataFormula(input.formula, data)
        fit <- FitRegression(.formula, .estimation.data, subset, .weights, type, robust.se, ...)
        if (internal)
        {
            fit$subset <- row.names %in% rownames(.estimation.data)
            fit$sample.description <- processed.data$description
            return(fit)
        }
        original <- fit$original
        .design <- fit$design

        result <- list(original = original, call = cl)

        if (!is.null(.design))
            result$design <- .design
        requireNamespace("car")
        if (missing == "Imputation (replace missing values with estimates)")
            data <- processed.data$data
        result$subset <- row.names %in% rownames(.estimation.data)
        result$sample.description <- processed.data$description
        result$n.predictors <- sum(!(names(result$original$coefficients) %in% "(Intercept)"))
        result$n.observations <- n
        result$estimation.data <- .estimation.data
    }
    class(result) <- "Regression"
    result$correction <- correction
    result$formula <- input.formula
    # Inserting the coefficients from the partial data.
    result$model <- data
    result$robust.se <- robust.se
    result$type <- type
    result$weights <- unfiltered.weights
    result$output <- output
    result$show.labels <- show.labels
    result$missing <- missing
    result$test.interaction <- !is.null(interaction)
    result$effects.format <- effects.format

    # remove environment attribute to reduce size
    attr(result$original$terms, ".Environment") <- NULL
    attr(attr(result$original$model, "terms"), ".Environment") <- NULL

    suppressWarnings(tmpSummary <- summary(result$original))
    result$summary <- tidySummary(tmpSummary, result$original, result)
    result$summary$call <- cl

    # Replacing the variables with their labels
    result$outcome.name <- outcome.name
    result$outcome.label <- RemoveBackticks(outcome.name)
    if (show.labels)
    {
        if (type == "Multinomial Logit")
        {
            nms <- colnames(result$summary$coefficients)
            colnames(result$summary$coefficients) <- colnames(result$summary$standard.errors) <- Labels(data, nms)
        }
        else
        {
            nms <- rownames(result$summary$coefficients)
            rownames(result$summary$coefficients) <- Labels(data, nms)
        }
        label <- Labels(outcome.variable)
        if (!is.null(label))
            result$outcome.label <- label
    }
    if (!recursive.call)
        aliasedPredictorWarning(result$summary$aliased,
                                if (show.labels) Labels(data, names(result$summary$aliased)) else NULL)

    result$terms <- result$original$terms
    result$coef <- coef(result$original)
    if (!result$test.interaction)
        result$r.squared <- GoodnessOfFit(result)$value
    if (type == "Ordered Logit" & !inherits(result$original, "svyolr"))
        result$coef <- c(result$coef, result$original$zeta)
    if (type == "Multinomial Logit")
    {
        result$z.statistics <- result$summary$coefficients / result$summary$standard.errors
        raw.pvalues <- 2 * (1 - pnorm(abs(result$z.statistics)))
        dnn <- dimnames(raw.pvalues)
        adj.pvalues <- pvalAdjust(raw.pvalues, correction)
        result$p.values <- matrix(adj.pvalues, ncol=length(dnn[[2]]), dimnames=dnn)
    }
    else
    {
        result$summary$coefficients[,4] <- pvalAdjust(result$summary$coefficients[,4], correction)
    }

    if (!is.null(importance))
    {
        labels <- rownames(result$summary$coefficients)
        labels <- if (result$type == "Ordered Logit") labels[1:result$n.predictors] else labels[-1]
        signs <- if (importance.absolute) 1 else sign(extractVariableCoefficients(result$original, type))
        result$importance <- estimateImportance(input.formula, .estimation.data, .weights,
                                                type, signs, result$r.squared,
                                                labels, robust.se, !recursive.call,
                                                correction, importance, ...)
        result$importance.type <- importance
        if (importance == "Relative Importance Analysis")
            result$relative.importance <- result$importance
    }
    if (result$test.interaction)
        result$interaction <- computeInteractionCrosstab(result, interaction.name, interaction.label,
                                                         formula.with.interaction, importance,
                                                         importance.absolute,
                                                         internal.loop = !is.null(interaction.formula), ...)

    # Creating the subtitle/footer
    if (!partial)
    {
        result$sample.description <- processed.data$description
        if (output == "ANOVA" || output == "Effects Plot")
        {
            anova.out <- Anova(result, robust.se)
            if (!recursive.call)
            {
                p.name <- grep("Pr", names(anova.out), value = TRUE)
                anova.out[[p.name]] <- pvalAdjust(anova.out[[p.name]], result$correction)
                #    anova.out[[4]] <- pvalAdjust(anova.out[[4]], result$correction)
            }
            result$anova <- anova.out
        }
    }
    result$footer <- regressionFooter(result)
    if (!is.null(result$importance))
        result$importance.footer <- importanceFooter(result)
    options(contrasts = old.contrasts[[1]])

    result <- setChartData(result, output)

    return(result)
}

# Tidies up inconsistencies in summary output
# And applies robust standard errors
tidySummary <- function(rsummary, fit.reg, result)
{
    if (!is.matrix(rsummary$coefficients)) # Tidying up MNL outputs with only one two categories.
    {
        rsummary$coefficients <- t(as.matrix(rsummary$coefficients))
        rsummary$standard.errors <- t(as.matrix(rsummary$standard.errors))
        outcome.variable <- result$model[,result$outcome.name]
        rownames(rsummary$standard.errors) <- rownames(rsummary$coefficients) <- levels(outcome.variable)[[2]]
    }
    if (result$robust.se != FALSE)
    {
        if(is.null(result$weights))
        {
            #robust.coef <-  coeftest(result, vcov. = vcov(result, result$robust.se))
            robust.coef <-  coeftest(fit.reg, vcov. = vcov2(fit.reg, result$robust.se))
            colnames(robust.coef)[2] <- "Robust SE"
            class(robust.coef) <- "matrix" # Fixing weird bug where robust.se changes class of matrix.
            rsummary$coefficients <- robust.coef
        }
        else
            robust.se = FALSE
    }
    else if (result$type == "Ordered Logit" & result$missing != "Multiple imputation")
    {   #Tidying up Ordered Logit coefficients table to be consistent with the rest of R.
        coefs <-  rsummary$coefficients
        ps <- 2 * pt(-abs(coefs[, 3]), df = rsummary$df.residual)
        colnames(coefs)[1] <- "Estimate"
        rsummary$coefficients <- cbind(coefs, p = ps)
    }
    # Removing extra backticks introduced by DataFormula, and unescaping original backticks
    if(result$type == "Multinomial Logit")
    {
        nms <- CleanBackticks(colnames(rsummary$coefficients))
        colnames(rsummary$coefficients) <- colnames(rsummary$standard.errors) <- nms
    }
    else
    {
        nms <- CleanBackticks(rownames(rsummary$coefficients))
        rownames(rsummary$coefficients) <- nms
    }
    return(rsummary)
}



regressionFooter <- function(x)
{
    # Creating a nicely formatted text description of the model.
    partial <- x$missing == "Use partial data (pairwise correlations)"
    aic <- if(partial) NA else AIC(x)
    rho.2 <- if(partial | x$type == "Linear") NA else McFaddensRhoSquared(x)
    footer <- x$sample.description

    if (x$test.interaction)
    {
        if (x$missing == "Multiple imputation" && !is.null(x$interaction$anova.test))
            footer <- paste0(footer, " ", x$interaction$anova.test, " uses statistic averaged over multiple imputations;")

        if (is.null(x$interaction$original.r2) || is.null(x$interaction$full.r2) ||
            is.na(x$interaction$original.r2) || is.na(x$interaction$full.r2))
        {
            footer <- sprintf("%s multiple comparisons correction: %s", footer, x$correction)
            return(footer)
        }

        r.desc <- ifelse(x$type == "Linear" & is.null(x$weights), "R-squared", "McFaddens's rho-squared")
        footer <- sprintf("%s %s of pooled model: %.4f; %s of interaction model %.4f;",
                          footer, r.desc, x$interaction$original.r2, r.desc, x$interaction$full.r2)

        if (x$missing == "Multiple imputation")
            footer <- sprintf("%s %s averaged over multiple imputations;", footer, r.desc)
    }
    else
    {
        footer <- paste0(footer," R-squared: ", FormatAsReal(x$r.squared, 4), "; ")
        if (!partial)
            footer <- paste0(footer,
                             "Correct predictions: ", FormatAsPercent(Accuracy(x, x$subset, x$weights), 4),
                             if (is.null(rho.2) | is.na(rho.2)) "" else paste0("; McFadden's rho-squared: ", round(rho.2, 4)),
                             if (is.na(aic)) "" else paste0("; AIC: ", FormatAsReal(aic, 5)), "; ")
    }
    footer <- sprintf("%s multiple comparisons correction: %s", footer, x$correction)
    footer

}

importanceFooter <- function(x)
{
    footer <- x$sample.description
    if (!x$test.interaction)
        footer <- paste0(footer, " R-squared: ", FormatAsReal(x$r.squared, 4), ";")
    footer <- paste0(footer, " multiple comparisons correction: ", x$correction, ";")
    footer
}

#' \code{FitRegression}
#'
#' Fits a regression model.
#' @param .formula An object of class \code{\link{formula}} (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#'   The details of type specification are given under \sQuote{Details}.
#' @param .estimation.data A \code{\link{data.frame}}.
#' @param subset Not used.
#' @param .weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param type See \link{Regression}.
#' @param robust.se Computes standard errors that are robust to violations of
#'   the assumption of constant variance, using the HC1 (degrees of freedom)
#'   modification of White's (1980) estimator (Long and Ervin, 2000). This parameter is ignored
#'   if weights are applied (as weights already employ a sandwich estimator).
#' @param ... Arguments to the wrapped functions.
#' @importFrom flipData CalibrateWeight WeightedSurveyDesign
#' @importFrom flipU InterceptExceptions
#' @importFrom MASS polr glm.nb
#' @importFrom nnet multinom
#' @importFrom stats glm lm poisson quasipoisson binomial pt quasibinomial
#' @importFrom survey svyglm svyolr
#' @export
FitRegression <- function(.formula, .estimation.data, subset, .weights, type, robust.se, ...)
{
    weights <- .weights #Does nothing, except remove notes from package check.
    .design <- NULL
    if (is.null(.weights))
    {
        if (type == "Linear")
        {
            model <- lm(.formula, .estimation.data, model = TRUE)
            model$aic <- AIC(model)
        }
        else if (type == "Poisson" | type == "Quasi-Poisson" | type == "Binary Logit")
            model <- glm(.formula, .estimation.data, family = switch(type,
                                                                     "Poisson" = poisson,
                                                                     "Quasi-Poisson" = quasipoisson,
                                                                     "Binary Logit" = binomial(link = "logit")))
        else if (type == "Ordered Logit")
        {
            model <- fitOrderedLogit(.formula, .estimation.data, NULL, ...)
            model$aic <- AIC(model)
        }
        else if (type == "Multinomial Logit")
        {
            model <- multinom(.formula, .estimation.data, Hess = TRUE, trace = FALSE, MaxNWts = 1e9, maxit = 10000, ...)
            model$aic <- AIC(model)
        }
        else if (type == "NBD")
        {
            withWarnings <- function(expr) {
                myWarnings <- NULL
                wHandler <- function(w) {
                    myWarnings <<- c(myWarnings, list(w))
                    invokeRestart("muffleWarning")
                }
                val <- withCallingHandlers(expr, warning = wHandler)
                list(value = val, warnings = myWarnings)
            }

            result <- withWarnings(glm.nb(.formula, .estimation.data))
            model <- result$value
            if (!is.null(result$warnings))
                if (result$warnings[[1]]$message == "iteration limit reached")
                    warning("Model may not have converged. If the dispersion parameter from the Detail",
                            " output is large, a Poisson model may be appropriate.")
            else
                warning(result$warnings)
        }
        else
            stop("Unknown regression 'type'.")
    }
    else
    {
        if (robust.se)
        {
            warning(warningRobustInappropriate())
            robust.se <- FALSE
        }
        if (type == "Linear")
        {
            .design <- WeightedSurveyDesign(.estimation.data, .weights)
            model <- svyglm(.formula, .design)
            if (all(model$residuals == 0)) # perfect fit
                model$df <- NA
            else
            {
                assign(".design", .design, envir=.GlobalEnv)
                aic <- try(extractAIC(model), silent = TRUE)
                if (any("try-error" %in% class(aic)))
                {
                    warning("Error occurred when computing AIC. The most likely ",
                            "explanation for this is this is a small sample size in ",
                            "some aspect of the analysis. ")
                    aic <- rep(NA, 2)
                }
                remove(".design", envir=.GlobalEnv)
                model$df <- aic[1]
                model$aic <- aic[2]
            }
        }
        else if (type == "Ordered Logit")
            model <- fitOrderedLogit(.formula, .estimation.data, weights, ...)
        else if (type == "Multinomial Logit")
        {
            .estimation.data$weights <- CalibrateWeight(.weights)
            model <- multinom(.formula, .estimation.data, weights = weights,
                              Hess = TRUE, trace = FALSE, maxit = 10000, MaxNWts = 1e9, ...)
            model$aic <- AIC(model)
        }
        else if (type == "NBD")
        {
            .estimation.data$weights <- CalibrateWeight(.weights)
            model <- glm.nb(.formula, .estimation.data, weights = weights, ...)
        }
        else
        {
            .design <- WeightedSurveyDesign(.estimation.data, .weights)
            model <- switch(type,
                            "Binary Logit" = svyglm(.formula, .design, family = quasibinomial()),
                            "Poisson" = svyglm(.formula, .design, family = poisson()),
                            "Quasi-Poisson" = svyglm(.formula, .design, family = quasipoisson()))
            assign(".design", .design, envir=.GlobalEnv)
            aic <- extractAIC(model)
            remove(".design", envir=.GlobalEnv)
            model$df <- aic[1]
            model$aic <- aic[2]
        }
    }
    result <- list(original = model, formula = .formula, design = .design, weights = .weights, robust.se = robust.se)
    class(result) <- "FitRegression"
    result
}


#' notValidForPartial
#'
#' @param object A Regression object
#' @param method The regression method.
notValidForPartial <- function(object, method)
{
    ms <- "Use partial data (pairwise correlations)"
    if (object$missing == ms)
        stop(paste0("'", method, "' not available when 'missing' = ",ms, "'." ))
}


#' notValidForCrosstabInteraction
#'
#' @param object A Regression object
#' @param method The regression method.
notValidForCrosstabInteraction <- function(object, method)
{
    if (object$test.interaction)
        stop(paste0("'", method, "' not available when Crosstab interaction variable is supplied." ))
}


#' @importFrom stats coef
#' @export
coef.Regression <- function(object, ...)
{
    coef(object$original, ...)
}


#' @importFrom stats nobs
#' @export
nobs.Regression <- function(object, ...)
{
    object$n.observations
}

#' vcov.Regression
#'
#' @param object A \code{Regression} model.
#' @param robust.se If \code{TRUE}, computes standard errors that are robust
#' to violations of the assumption of constant variance for linear and Poisson
#' models, using the HC3 modification of White's (1980) estimator (Long and Ervin,
#' 2000). This parameter is ignored if weights are applied (as weights already
#' employ a sandwich estimator). Other options are \code{FALSE}, \code{"hc0"},
#' \code{"hc1"}, \code{"hc2"}, \code{"hc4"}.
#' @param ... Additional arguments.
#' @importFrom car hccm
#' @importFrom stats vcov
#' @export
vcov.Regression <- function(object, robust.se = FALSE, ...)
{
    if (robust.se == FALSE)
    {
        v <- vcov(object$original)
        if(!issvyglm(object))
            return(v)
    }
    else
    {
        if (robust.se == TRUE)
            robust.se <- "hc3"
        v <- hccm(object$original, type = robust.se)
    }
    FixVarianceCovarianceMatrix(v)
}

vcov2 <- function(fit.reg, robust.se = FALSE, ...)
{
    if (robust.se == FALSE)
    {
        v <- vcov(fit.reg)
        if(!inherits(fit.reg, "svyglm"))
            return(v)
    }
    else
    {
        if (robust.se == TRUE)
            robust.se <- "hc3"
        v <- hccm(fit.reg, type = robust.se)
    }
    FixVarianceCovarianceMatrix(v)
}


#' vcov.Regression
#'
#' @param object A \code{Regression} model.
#' @param robust.se If \code{TRUE}, computes standard errors that are robust
#' to violations of the assumption of constant variance for linear and Poisson
#' models, using the HC3 modification of White's (1980) estimator (Long and Ervin,
#' 2000). This parameter is ignored if weights are applied (as weights already
#' employ a sandwich estimator). Other options are \code{FALSE}, \code{"hc0"},
#' \code{"hc1"}, \code{"hc2"}, \code{"hc4"}.
#' @param ... Additional arguments.
#' @importFrom car hccm
#' @importFrom stats vcov
#' @export
vcov.FitRegression <- function(object, robust.se = FALSE, ...)
{
    vcov.Regression(object, robust.se)
}

#' FixVarianceCovarianceMatrix
#'
#' Makes some adjustments to permit a covariance-marix to be inverted, if required.
#' @param x A variance-covariance matrix of parameter estimates.
#' @param min.eigenvalue Minimm acceptable eigenvalue.
#' @details Sandwich and sandwich-like standard errors can result uninvertable
#' covariance matrices (e.g., if a parameter represents a sub-group, and the sub-group has no
#' residual variance). This function checks to see if there are any eigenvalues less than \code{min.eigenvalue},
#' which defaults to 1e-12. If there are, an attempt is made to guess the  offending variances, and they are multiplied by 1.01.
#' @export
FixVarianceCovarianceMatrix <- function(x, min.eigenvalue = 1e-12)
{
    wng <- "There is a technical problem with the parameter variance-covariance matrix. This is most likely due to either a problem or the appropriateness of the statistical model (e.g., using weights or robust standard errors where a sub-group in the analysis has no variation in its residuals, or lack of variation in one or more predictors."
    v <- x
    v <- try(
        {
            if (min(eigen(v)$values) >= min.eigenvalue)
                return(v)
            v.diag <- diag(v)
            n.similar.to.diag <- abs(sweep(v, 1, v.diag, "/"))
            high.r <- apply(n.similar.to.diag > 0.99, 1, sum) > 1
            diag(v)[high.r] <- v.diag[high.r] * 1.01
            v
        }, silent = TRUE
    )
    if (tryError(v))
        stop(wng)
    else
        warning(wng)
    v
}

tryError <- function(x)
{
    if (any("try-error" %in% class(x)))
        return(TRUE)
    FALSE
}

# Warn for colinear variables, which have NA coeffient and are removed from summary table
aliasedPredictorWarning <- function(aliased, aliased.labels) {
    if (any(aliased))
    {
        alias.vars <- if (!is.null(aliased.labels)) aliased.labels[aliased] else names(aliased)[aliased]
        warning("The following variable(s) are colinear with other variables and no",
                " coefficients have been estimated: ", paste(alias.vars, collapse = ", "))
    }
}

#' @importFrom stats model.frame model.response model.matrix
fitOrderedLogit <- function(.formula, .estimation.data, weights, ...)
{
    model <- InterceptExceptions(
        if (is.null(weights))
            polr(.formula, .estimation.data, Hess = TRUE, ...)
        else
        {
            .design <- WeightedSurveyDesign(.estimation.data, weights)
            out <- svyolr(.formula, .design, ...)
            out$df <- out$edf
            # Compute the dAIC
            wt <- weights(.design)
            out$deltabar <- mean(diag(out$Hessian %*% out$var))/mean(wt)
            out$aic <- out$deviance + 2 * out$deltabar * out$df
            # Need this model element to handle the code in the predict.Regression method
            out$model <- model.frame(.formula, model.frame(.design), na.action = na.pass)
            # Need linear predictor element to compute the surrogate residuals.
            Terms <- attr(out$model, "terms")
            x <- model.matrix(Terms, out$model)
            # Remove intercept if if exists
            if  (length(xint <- match("(Intercept)", colnames(x), nomatch = 0)) > 0)
                x <- x[, -xint, drop = FALSE]
            # Remove any colinear variables that were dropped by polr/svyolr
            if (any(keep <- colnames(x) %in% names(out$coefficients)))
                x <- x[, keep, drop = FALSE]
            out$lp <- drop(x %*% out$coefficients)
            # Give it the polr class to use the predict.polr method while using summary.svyolr method
            # Also allows the sure resids method to compute the mean via the polr method to get the linear
            # predictor.
            class(out) <- c("svyolr", "polr")
            out
        },
        warning.handler = function(w) {
            if (w$message == "design appears to be rank-deficient, so dropping some coefs")
                warning("Some variable(s) are colinear with other variables ",
                        "and they have been removed from the estimation.")
            else
                warning(w$message)
        },
        error.handler = function(e) {
            m <- model.frame(.formula, model.frame(.design), na.action = na.pass)
            y <- model.response(m)
            unobserved.levels <- paste0(setdiff(levels(y), y), collapse = ", ")
            if (unobserved.levels != "")
                stop("Outcome variable has level(s): ", unobserved.levels, " that are not observed in the data. ",
                     "If possible, this issue could be solved by merging the categories of the outcome variable ",
                     "such that all categories appear in all sub-groups.")
            else
                stop("An error occurred during model fitting. ",
                     "Please check your input data for unusual values: ", e$message)
        })
}

setChartData <- function(result, output)
{
    chart.data <- if (output == "ANOVA")
                      data.frame(result$anova)
                  else if (result$test.interaction)
                  {
                      dt <- rbind(result$interaction$coefficients,
                                  result$interaction$split.size)
                      rownames(dt)[nrow(dt)] <- "n"
                      dt
                  }
                  else if (output %in% c("Relative Importance Analysis",
                                         "Shapley Regression"))
                  {
                      importance <- result$importance
                      df <- data.frame(importance$importance,
                                       importance$raw.importance,
                                       importance$standard.errors,
                                       importance$statistics,
                                       importance$p.values)
                      colnames(df) <- c("Importance", "Raw score", "Standard Error",
                                        "t statistic", "p-value")
                      df
                  }
                  else
                      result$summary$coefficients

    attr(result, "ChartData") <- chart.data
    return(result)
}

removeMissingVariables <- function(data, formula, formula.with.interaction,
                                   missing.variables, outcome.name, input.formula)
{
    missing.variable.names <- colnames(data)[missing.variables]
    if (outcome.name %in% missing.variable.names)
        stop("Response variable is entirely missing (all observed values of the variable are missing).")
    else {
        # Update data and formula
        formula.terms <- terms.formula(input.formula, data = data)
        terms.to.drop <- unique(unlist(sapply(missing.variable.names, grep, x = attr(formula.terms, "term.labels"))))
        input.formula <- update(input.formula, drop.terms(formula.terms, terms.to.drop, keep.response = TRUE))
        interaction.formula.terms <- terms.formula(formula.with.interaction, data = data)
        interaction.terms.to.drop <- unique(unlist(sapply(missing.variable.names, grep,
                                                          x = attr(interaction.formula.terms, "term.labels"))))
        formula.with.interaction <- update(formula.with.interaction, drop.terms(interaction.formula.terms,
                                                                                interaction.terms.to.drop,
                                                                                keep.response = TRUE))
        formula <- input.formula
        data <- data[, !missing.variables]
        missing.variable.names <- paste0(missing.variable.names, collapse = ", ")
        warning("Data has variable(s) that are entirely missing values (all observed values of the variable are missing). ",
                "These variable(s) have been removed from the analysis: ", missing.variable.names, ".")
    }
    return(list(data = data, formula = formula, formula.with.interaction = formula.with.interaction))
}
