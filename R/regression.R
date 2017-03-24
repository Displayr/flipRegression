#' \code{Regression} 
#' @description Generalized Regression.
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
#' @param type Defaults to \code{"linear"}. Other types are: \code{"Poisson"},
#'   \code{"Quasi-Poisson"}, \code{"Binary Logit"}, \code{"NBD"},
#'   \code{"Ordered Logit"}, and \code{"Multinomial Logit"}
#' @param robust.se If \code{TRUE}, computes standard errors that are robust to violations of
#'   the assumption of constant variance for linear models, using the HC3 modification of White's (1980) estimator
#'   (Long and Ervin, 2000). This parameter is ignored if weights are applied (as weights already
#'   employ a sandwich estimator). Other options are \code{FALSE} and \code{"FALSE"No}, which do the same
#'   thing, and \code{"hc0"}, \code{"hc1"}, \code{"hc2"}, \code{"hc4"}.
#' @param output \code{"Coefficients"} returns a table of coefficients and various
#' summary and model statistics. It is the default. \code{"ANOVA"} returns an
#' ANOVA table. \code{"R"} returns a more traditional R output.
#' @param detail This is a deprecated function. If \code{TRUE}, \code{output} is set to \code{R}.
#' @param method The method to be used; for fitting. This will only do something if
#' method = "model.frame", which returns the model frame.
#' @param m The number of imputed samples, if using multiple imputation.
#' @param seed The random number seed used in imputation.
#' @param statistical.assumptions A Statistical Assumptions object.
#' @param auxiliary.data A \code{\link{data.frame}} containing additional variables
#'  to be used in imputation (if required). While adding more variables will improve
#'  the quality of the imputation, it will dramatically slow down the time to estimate.
#'  Factors and Character variables with a large number of categories should not be included,
#'  as they will both slow down the data and are unlikely to be useful
#' @param show.labels Shows the variable labels, as opposed to the names, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param internal If \code{TRUE}, skips most of the tidying at the end. Only for use when it is
#' desired to call a relatively light version of Regression for other purposes (e.g., in ANOVA).
#' This leads to creation of an object of class \code{FitRegression}.)
#' @param contrasts A vector of the contrasts to be used for \code{\link{factor}} and
#' \code{\link{ordered}} variables. Defaults to \code{c("contr.treatment", "contr.treatment"))}.
#' Set to \code{c("contr.treatment", "contr.poly"))} to use orthogonal polynomials for \code{\link{factor}}
#' See \code{\link{contrasts}} for more information.
#' @param relative.importance Whether to compute relative importance.
#' @param interaction Optional variable to test for interaction with other variables in the model.
#' @param ... Additional argments to be past to  \code{\link{lm}} or, if the
#'   data is weighted,  \code{\link[survey]{svyglm}}.
#' @details "Imputation (replace missing values with estimates)". All selected
#'   outcome and predictor variables are included in the imputation, along with
#'   all \code{auxiliary.data}, excluding cases that are excluded via subset or
#'    have invalid weights, but including cases with missing values of the outcome variable.
#'   Then, cases with missing values in the outcome variable are excluded from
#'   the analysis (von Hippel 2007). See \code{\link[flipImputation]{Imputation}}.
#' @references von Hippel, Paul T. 2007. "Regression With Missing Y's: An
#'   Improved Strategy for Analyzing Multiply Imputed Data." Sociological
#'   Methodology 37:83-117. White, H. (1980), A heteroskedastic-consistent
#'   covariance matrix estimator and a direct test of heteroskedasticity.
#'   Econometrica, 48, 817-838. Long, J. S. and Ervin, L. H. (2000). Using
#'   heteroscedasticity consistent standard errors in the linear regression
#'   model. The American Statistician, 54(3): 217-224.
#' @importFrom stats pnorm anova update
#' @importFrom flipData GetData CleanSubset CleanWeights DataFormula EstimationData CleanBackticks
#' @importFrom flipFormat Labels OriginalName
#' @importFrom flipU OutcomeName IsCount
#' @importFrom flipTransformations AsNumeric CreatingBinaryDependentVariableIfNecessary Factor Ordered
#' @importFrom lmtest coeftest
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
                       interaction = NULL,
                       ...)
{
    old.contrasts <- options("contrasts")
    options(contrasts = contrasts)
    if (detail)
        output <- "R"
    if (robust.se == "No")
        robust.se <- FALSE
    cl <- match.call()
    if(!missing(statistical.assumptions))
        stop("'statistical.assumptions' objects are not yet supported.")

    input.formula <- formula # Hack to work past scoping issues in car package: https://cran.r-project.org/web/packages/car/vignettes/embedding.pdf.
    subset.description <- try(deparse(substitute(subset)), silent = TRUE) #We don't know whether subset is a variable in the environment or in data.
    subset <- eval(substitute(subset), data, parent.frame())
    if (!is.null(subset))
    {
        if (is.null(subset.description) | (class(subset.description) == "try-error") | !is.null(attr(subset, "name")))
            subset.description <- Labels(subset)
        if (is.null(attr(subset, "name")))
            attr(subset, "name") <- subset.description
    }
    if (!is.null(list(...)$weights))
        weights <- list(...)$weights
    weight.name <- deparse(substitute(weights))
    weights <- eval(substitute(weights), data, parent.frame())

    interaction.name <- deparse(substitute(interaction))
    interaction <- eval(substitute(interaction), data, parent.frame())
    if (!is.null(interaction) && relative.importance)
        stop("Relative importance is incompatible with Crosstab interaction.")
    if (!is.null(interaction) && type == "Multinomial Logit")
        stop("Crosstab interaction is incompatible with Multinomial logit regression.")

    formula2 <- if (is.null(interaction)) input.formula
                else update(input.formula, sprintf(".~.*%s",interaction.name))
    data <- GetData(input.formula, data, auxiliary.data)
    if (!is.null(interaction))
    {
        data <- cbind(data, Factor(interaction))
        colnames(data)[ncol(data)] <- interaction.name
    }

    if (method == "model.frame")
        return(data)
    outcome.name <- OutcomeName(input.formula)
    outcome.variable <- data[[outcome.name]]
    if(sum(outcome.name == names(data)) > 1)
        stop("The 'Outcome' variable has been selected a 'Predictor'. It must be one or the other, but may not be both.")
    if (!is.null(weights) & length(weights) != nrow(data))
        stop("'weights' and 'data' are required to have the same number of observations. They do not.")
    if (!is.null(subset) & length(subset) > 1 & length(subset) != nrow(data))
        stop("'subset' and 'data' are required to have the same number of observations. They do not.")
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
        if (relative.importance)
            stop("Relative importance analysis is not available when using pairwise correlations on missing data.")
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
        processed.data <- EstimationData(formula2, data, subset, weights, missing, m = m, seed = seed)
        if (missing == "Multiple imputation")
        {
            models <- lapply(processed.data$estimation.data,
                FUN = function(x)
                {
                    int.copy <- if (!is.null(interaction)) x[,interaction.name]
                                else                       NULL
                    Regression(formula,
                    data = x,
                    missing = "Error if missing data",
                    weights = processed.data$weights,
                    type = type,
                    robust.se = FALSE,
                    detail = detail,
                    show.labels = show.labels,
                    relative.importance = relative.importance,
                    interaction = int.copy)
                })
            final.model <- models[[1]]
            final.model$outcome.label <- if(show.labels) Labels(outcome.variable) else outcome.name
            coefs <- MultipleImputationCoefficientTable(models)
            if (show.labels && type == "Multinomial Logit")
            {
                coef.labels <- colnames(final.model$summary$coefficients)
                kc <- length(coef.labels)
                alt.labels <- rownames(final.model$summary$coefficients)
                kr <- length(alt.labels)
                rownames(coefs) <- paste(alt.labels, coef.labels[rep(1:kc, rep(kr, kc))])
            }
            final.model$coefficient.table <- coefs
            final.model$summary$coefficients  <- coefs[, -4]
            final.model$coef <- final.model$original$coef <- coefs[, 1]
            final.model$missing = "Multiple imputation"
            final.model$sample.description <- processed.data$description
            final.model$footer <- regressionFooter(final.model)
            if (relative.importance)
            {
                final.model$relative.importance <- multipleImputationRelativeImportance(models)
                final.model$relative.importance.footer <- relativeImportanceFooter(final.model)
            }
            final.model$interaction.name <- interaction.name
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
        .formula <- DataFormula(input.formula)
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
        result$n.predictors <- ncol(.estimation.data) - 1
        result$n.observations <- n
        result$estimation.data <- .estimation.data


        if (relative.importance)
        {
            signs <- sign(extractVariableCoefficients(original, type))
            result$relative.importance <- estimateRelativeImportance(input.formula, .estimation.data, .weights,
                                                                     type, signs, ...)
        }
    }
    class(result) <- "Regression"
    result$summary$call <- cl
    result$formula <- input.formula
    # Inserting the coefficients from the partial data.
    result$model <- data
    result$robust.se <- robust.se #1
    result$type = type #!
    result$weights <- unfiltered.weights #!
    result$output <- output
    result$show.labels <- show.labels
    result$missing <- missing #!

    suppressWarnings(tmpSummary <- summary(result$original))
    result$summary <- tidySummary(tmpSummary, result$original, result)

    # Replacing the variables with their labels
    result$outcome.label <- result$outcome.name <- outcome.name
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
        result$outcome.label <- Labels(outcome.variable)
    }

    result$terms <- result$original$terms
    result$coef <- coef(result$original)
    result$r.squared <- GoodnessOfFit(result)$value
    if (type == "Ordered Logit")
        result$coef <- c(result$coef, result$original$zeta)
    if (type == "Multinomial Logit")
    {
        result$z.statistics <- result$summary$coefficients / result$summary$standard.errors
        result$p.values <- 2 * (1 - pnorm(abs(result$z.statistics)))
    }

    # Crosstab-interaction
    result$test.interaction <- !is.null(interaction)
    result$interaction.name <- interaction.name
    if (result$test.interaction)
    {
        fit2 <- FitRegression(formula2, .estimation.data, subset, .weights, type, robust.se, ...)

        atest <- if (type %in% c("Linear", "Quasi-Poisson")) "F"
                 else                                        "Chisq"
        atmp <- anova(fit$original, fit2$original, test=atest)
        result$anova.test <- switch(atest, F="F test", Chisq="Chi-square test")
        result$interaction.pvalue <- atmp$Pr[2]
        result$interaction.model <- fit2$original

        # Compute table of coefficients
        rsum2 <- summary(fit2$original)
        cat("line 325\n")
        print(rsum2$coef)
        rsum2 <- tidySummary(rsum2, fit2$original, result)
        cat("line 328\n")
        print(rsum2$coef)
        tmp.coef2 <- rsum2$coef[,1]
        if (any(is.na(tmp.coef2)) && result$robust.se)
        {
            warning("Robust SE not used as some coefficients are undefined\n")
            result$robust.se <- FALSE
        }

        tmp.sd2 <- rsum2$coef[,2]^2
        #print(tmp.sd2)
        tmp.coef <- summary(fit$original)$coef[,1]
        num.var <- length(tmp.coef)
        split.labels <- levels(.estimation.data[,interaction.name])
        split.names <- paste0(interaction.name, split.labels)
        num.split <- length(split.names)
        split.size <- table(.estimation.data[,interaction.name])

        var.names <- names(tmp.coef)
        all.names <- sprintf("%s%s", var.names,
                    rep(c("", paste0(":", split.names[-1])), each=length(var.names)))
        all.names <- gsub("(Intercept):", "", all.names, fixed=T)

        coef.tab <- matrix(tmp.coef2[all.names], ncol=num.split)
        print(matrix(all.names, ncol=num.split))
        sd2.tab <- matrix(tmp.sd2[all.names], ncol=num.split)
        print(sd2.tab)
        diff.coef <- matrix(0, nrow(coef.tab), ncol(coef.tab))

        # Only check differences between coefficients if we accept fit2
        if (result$interaction.pvalue < 0.05)
            diff.coef <- compareCoef(coef.tab, sd2.tab, split.size)

        split.size <- c(split.size, NET=sum(split.size))
        combined.coefs <- cbind(coef.tab, tmp.coef)
        colnames(combined.coefs) <- c(split.labels, "NET")
        rownames(combined.coefs) <- names(tmp.coef)
        result$combined.coefs <- combined.coefs
        result$coef.sign <- diff.coef
        result$split.size <- split.size
    }

    # Creating the subtitle/footer
    if (!partial)
    {
        result$sample.description <- processed.data$description
        if (output == "ANOVA")
            result$anova <- Anova(result, robust.se)
    }
    result$footer <- regressionFooter(result)
    if (!is.null(result$relative.importance))
        result$relative.importance.footer <- relativeImportanceFooter(result)
    options(contrasts = old.contrasts[[1]])

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
        if (x$type == "Linear")
            footer <- paste0(footer, " R-squared of pooled model: ",
                                       FormatAsReal(summary(x$original)$r.square, 4),
                                    "; R-squared of interaction model: ",
                                       FormatAsReal(summary(x$interaction.model)$r.square, 4))
        else
            footer <- paste0(footer, " McFadden's rho-squared of pooled model: ",
                                       round(1 - deviance(x$original)/nullDeviance(x), 4),
                                    "; McFaddens's rho-squared of interaction model: ",
                                       round(1 - deviance(x$interaction.model)/nullDeviance(x), 4))
        return(footer)
    }

    footer <- paste0(footer," R-squared: ", FormatAsReal(x$r.squared, 4), "; ")
    if (!partial)
        footer <- paste0(footer,
               "Correct predictions: ", FormatAsPercent(Accuracy(x, x$subset, x$weights), 4),
               if (is.null(rho.2) | is.na(rho.2)) "" else paste0("; McFadden's rho-squared: ", round(rho.2, 4)),
               if (is.na(aic)) "" else paste0("; AIC: ", FormatAsReal(aic, 5), "; "))
    footer

}

relativeImportanceFooter <- function(x)
{
    paste0(x$sample.description ," R-squared: ", FormatAsReal(x$r.squared, 4), "; ")
}

#' \code{FitRegression}
#'
#' Fits a regression model.
#' @param .formula An object of class \code{\link{formula}} (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#'   The details of type specification are given under \sQuote{Details}.
#' @param .estimation.data A \code{\link{data.frame}}.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param .weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param type See \link{Regression}.
#' @param robust.se Computes standard errors that are robust to violations of
#'   the assumption of constant variance, using the HC1 (degrees of freedom)
#'   modification of White's (1980) estimator (Long and Ervin, 2000). This parameter is ignored
#'   if weights are applied (as weights already employ a sandwich estimator).
#' @param ... Arguments to the wrapped functions.
#' @importFrom flipData CalibrateWeight WeightedSurveyDesign
#' @importFrom MASS polr glm.nb
#' @importFrom nnet multinom
#' @importFrom stats glm lm poisson quasipoisson binomial pt quasibinomial
#' @importFrom survey svyglm
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
            model <- polr(.formula, .estimation.data, Hess = TRUE, ...)
            model$aic <- AIC(model)
        }
        else if (type == "Multinomial Logit")
        {
            model <- multinom(.formula, .estimation.data, Hess = TRUE, trace = FALSE, maxit = 10000, ...)
            model$aic <- AIC(model)
        }
        else if (type == "NBD")
            model <- glm.nb(.formula, .estimation.data)
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
                    warning("Error occurred when computing AIC. The most likely explanation for this is this is a small sample size in some aspect of the analysis. ")
                    aic <- rep(NA, 2)
                }
                remove(".design", envir=.GlobalEnv)
                model$df <- aic[1]
                model$aic <- aic[2]
            }
        }
        else if (type == "Ordered Logit")
        {
            .estimation.data$weights <- CalibrateWeight(.weights)
            model <- polr(.formula, .estimation.data, weights = weights, Hess = TRUE, ...)
            model$aic <- AIC(model)
        }
        else if (type == "Multinomial Logit")
        {
            .estimation.data$weights <- CalibrateWeight(.weights)
            model <- multinom(.formula, .estimation.data, weights = weights, Hess = TRUE, trace = FALSE, maxit = 10000, ...)
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

compareCoef <- function(bb, ss, nn, alpha = 0.05)
{
    if (any(dim(bb) != dim(ss)))
        stop("Dimensions of bb and ss must be the same\n")
    if (length(nn) != ncol(bb))
        stop("Length of nn should match columns in bb\n")
    res <- matrix(0, nrow=nrow(bb), ncol=ncol(ss))

    for (i in 1:nrow(bb))
    {
    for (j in 1:ncol(bb))
    {
        n0 <- sum(nn[-j], na.rm=T)
        b0 <- sum(nn[-j] * bb[i,-j], na.rm=T)/n0
        s0 <- sum((nn[-j]-1) * ss[i,-j], na.rm=T)/sum(nn[-j]-1, na.rm=T)
        v <- (ss[i,j] + s0)^2/(ss[i,j]^2/(nn[j]-1) + s0^2/(n0-1))

        t.stat <- (bb[i,j] - b0)/sqrt(ss[i,j] + s0)
        tc <- qt(1-alpha/2, v)
        if (!is.na(t.stat) && t.stat < -tc)
            res[i,j] <- -1
        if (!is.na(t.stat) && t.stat > tc)
            res[i,j] <- 1
    }
    }
    return (res)
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

