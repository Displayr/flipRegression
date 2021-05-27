#' Generalized Regression Outputs
#'
#' Computes output for seven different regression types.
#' Those being linear, binary logistic, ordered logistic, binomial, poisson, quasi-poisson and
#' multinomial. Output includes general coefficient estimates and importance analysis estimates
#' with possibilities for handling missing data and interaction terms.
#'
#' @param formula An object of class \code{\link{formula}} (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#'   The details of type specification are given under \sQuote{Details}.
#' @param data A \code{\link{data.frame}}.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param missing How missing data is to be treated in the regression. Supplied parameter needs to be one
#'   of the following strings:
#'   \itemize{
#'   \item \code{"Error if missing data"}: Throws an error if any case has a missing value,
#'   \item \code{"Exclude cases with missing data"}: Filters out cases that have any missing values,
#'   \item \code{"Use partial data (pairwise correlations)"}: This method is only valid for \code{type = "Linear"}
#'      and it computes the pairwise correlations using all the available data. Given the pairwise computed
#'      correlation matrix, a linear regression model is computed.
#'   \item \code{"Dummy variable adjustment"}: This method assumes that the missing data is structurally missing and
#'      that a predictor could be impossible for some cases and hence coded as missing. E.g. if a non-married
#'      person is asked to rate the quality of their marriage (or a person with no pets is asked the age of their
#'      pet). A model with a structure to allow this is used whereby the missing predictor is removed and an
#'      intercept adjustment is performed. This is implemented by adding a dummy variable for each predictor that
#'      has at least one missing value. The dummy indicator variables take the value zero if the original predictor
#'      has a non-missing value and the value one if the original predictor is missing. The original missing value
#'      is then recoded to a new value. In particular, missing numeric predictors are recoded to be the mean of the
#'      predictor (excluding the missing data) and factors are recoded to have their missing values recoded as the
#'      reference level of the factor.
#'   \item \code{"Imputation (replace missing values with estimates)"}: Missing values in the data are imputed via the
#'      use of the predictive mean matching method implemented in \code{\link[mice]{mice}}. Missing values in the
#'      outcome variable are excluded from the analysis after the imputation has been performed. If the
#'      \code{\link[mice]{mice}} method fails, hot-decking is used via the \code{\link[hot.deck]{hot.deck}} package.
#'   \item \code{"Multiple imputation"}: Implements the imputation method described above with option
#'       \code{"Imputation (replace missing values with estimates)"}. However, repeated imputation data is computed
#'       \code{m} times (see parameter below). The repeated set of imputed datasets are used to create \code{m} sets
#'       of separate regression coefficients that are then combined to produce a single set of estimated regression
#'       coefficients.
#'   }
#' @param type Defaults to \code{"Linear"}. Other types are: \code{"Poisson"},
#'   \code{"Quasi-Poisson"}, \code{"Binary Logit"}, \code{"NBD"},
#'   \code{"Ordered Logit"}, and \code{"Multinomial Logit"}
#' @param robust.se If \code{TRUE}, computes standard errors that are robust to violations of
#'   the assumption of constant variance for linear models, using the HC3 modification of White's (1980) estimator
#'   (Long and Ervin, 2000). This parameter is ignored if weights are applied (as weights already
#'   employ a sandwich estimator). Other options are \code{FALSE} and \code{"FALSE"No}, which do the same
#'   thing, and \code{"hc0"}, \code{"hc1"}, \code{"hc2"}, \code{"hc4"}. A singularity can occur when a robust
#'   estimator is requested that depends on the influence (\code{"hc2"}, \code{"hc3"} or \code{"hc4"}) and
#'   at least one observation has a hat value of one. In that case, division by zero could occur and to avoid
#'   this the robust estimator is still computed but the observations with a singularity are adjusted
#'   with the degrees of freedom adjustment (\code{"hc1"}).
#' @param output \code{"Coefficients"} returns a table of coefficients and various
#'   summary and model statistics. It is the default.
#'   \code{"ANOVA"} returns an ANOVA table.
#'   \code{"Detail"} returns a more traditional R output.
#'   \code{"Relative Importance Analysis"} returns a table with Relative Importance scores.
#'   \code{"Shapley Regression"} returns a table with Shapley Importance scores.
#'   \code{"Effects Plot"} returns the effects plot per predictor.
#'   \code{"Jaccard Coefficient"} returns the a relative importance table using the computed Jaccard
#'     coefficient of the outcome variable against each predictor. The outcome and predictor variables need
#'     to be binary variables for this output.
#'   \code{"Correlation"} returns a relative importance table using the Pearson correlation
#'     of the outcome variable against each predictor.
#' @param detail This is a deprecated function. If \code{TRUE}, \code{output} is set to \code{R}.
#' @param method The method to be used; for fitting. This will only do something if
#'   method = "model.frame", which returns the model frame.
#' @param m The number of imputed samples, if using multiple imputation.
#' @param seed The random number seed used in imputation and residual computations.
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
#' @param outlier.prop.to.remove A single numeric value that determines the percentage of data points to remove from the
#'   analysis. The data points removed correspond to those in the proportion with the largest residuals.
#'   A value of 0 or NULL would denote no points are removed. A value x, with 0 < x < 0.5 (not inclusive) would
#'   denote that a percentage between none and 50\% of the data points are removed.
#' @param stacked.data.check Logical value to determine if the Regression should be the data and formula based off
#'   the \code{unstacked.data} argument by stacking the input and creating a formula based off attributes and provided
#'   labels in the data. More details are given in the argument details for \code{unstacked.data}
#' @param unstacked.data A list with two elements that provide the outcome and predictor variables respectively for data
#'   that needs to be stacked. See details section for more information.
#' @param ... Additional argments to be passed to  \code{\link{lm}} or, if the
#'  data is weighted,  \code{\link[survey]{svyglm}} or \code{\link[survey]{svyolr}}.
#' @details In the case of Ordered Logistic regression, this function computes a proportional odds model using
#'  the cumulative link (logistic). In the case of no weights, the \code{\link[MASS]{polr}} function is used.
#'  In the case of a weighted regression, the \code{\link[survey]{svyolr}} function is used.
#'
#'  "Imputation (replace missing values with estimates)". All selected
#'  outcome and predictor variables are included in the imputation, along with
#'  all \code{auxiliary.data}, excluding cases that are excluded via subset or
#'  have invalid weights, but including cases with missing values of the outcome variable.
#'  Then, cases with missing values in the outcome variable are excluded from
#'  the analysis (von Hippel 2007). See \code{\link[flipImputation]{Imputation}}.
#'
#'  Outlier removal is performed by computing residuals for the regression model and removing the largest residuals
#'  from the dataset (outlier removal). The model is then refit on the reduced dataset after outliers are removed.
#'  The residuals used in this process depend on the regression type. For a regression with a numeric response
#'  (\code{type} is "Linear", "Poisson", "Quasi-Poisson" or  "NBD") in an unweighted regression, the studentised
#'  deviance residuals are used (see Davison and Snell (1991) and \code{\link[stats]{rstudent}}). In the weighted case
#'  of the numeric response, the Pearson residuals are used (see Davison and Snell (1991) and
#'  \code{\link[stats]{residuals.glm}}). In the case of Binary and Ordinal data for both the unweighted and weighted
#'  regression cases, the Surrogate residuals (SURE) are used (via the implementation in Greenwell, McCarthy and
#'  Boehmke (2017) with their sure R package). This was based on the recent theoretical paper in Liu and Zhang (2018).
#'  Currently "Multinomial Logit" is unsupported for automated outlier removal. Possible surrogate residual to be used
#'  in a future version.
#'
#'  In the case of stacking using the \code{unstacked.data} argument, it is designed to work best with input that is
#'  created with Q or Displayr which contains  \code{data.frame}s with a particular structure. If the Q/Displayr
#'  \code{data.frame}s are not available then simple \code{data.frame}s can be provided. In particular, a list is
#'  required with two elements, \itemize{
#'   \item \code{Y} A \code{data.frame} with \code{m} columns that represent the \code{m} variables to be stacked.
#'   \item \code{X} A \code{data.frame} where each column represents a column of a design matrix relevant to one of the
#'   \code{m} variables given in element \code{Y} above. So if the overall regression model has \code{p} predictors.
#'   Then this \code{data.frame} should contain \code{m * p} columns. In the absence of Q/Displayr metadata, the naming
#'   structure each column is comma separated of the form 'predictor, outcome' where 'predictor' denotes the
#'   predictor name in the regression design matrix and 'outcome' denotes the name of the variable in element \code{Y}.
#'   This format is required to ensure that the columns are appropriately matched and stacked. The function also
#'   accepts column names of the reverse order with 'outcome, predictor', so long as there isn't any ambiguity.
#'   In the absense of Q/Displayr meta data, the identification split is attempted via an assumed single comma separator
#'   }
#'   Also, when using Q/Displayr, some columns in the \code{data.frame} for the  \code{unstacked.data} argument will contain
#'   data reductions or NETs based off the codeframe and assigned codes to each NET. During the stacking process a NET is
#'   removed from analysis unless it is entirely comprised of codes that are not observed elsewhere in the \code{data.frame}
#'
#' @references Davison, A. C. and Snell, E. J. (1991) Residuals and diagnostics. In: Statistical Theory and Modelling.
#'   In Honour of Sir David Cox, FRS, eds. Hinkley, D. V., Reid, N. and Snell, E. J., Chapman & Hall.
#'
#'   Greenwell, B., McCarthy, A. and Boehmke, B. (2017). sure: Surrogate Residuals for Ordinal and General
#'   Regression Models. R package version 0.2.0. https://CRAN.R-project.org/package=sure
#'
#'   Gromping, U. (2007). "Estimators of Relative Importance in Linear
#'   Regression Based on Variance Decomposition", The American Statistician,
#'   61, 139-147.
#'
#'   von Hippel, Paul T. 2007. "Regression With Missing Y's: An
#'   Improved Strategy for Analyzing Multiply Imputed Data." Sociological
#'   Methodology 37:83-117.
#'
#'   Johnson, J.W. (2000). "A Heuristic Method for Estimating the Relative Weight",
#'   Multivariate Behavioral Research, 35:1-19.
#'
#'   Long, J. S. and Ervin, L. H. (2000). Using heteroscedasticity consistent
#'   standard errors in the linear regression  model. The American Statistician, 54(3): 217-224.
#'
#'   Lui, D. and Zhang, H. (2018). Residuals and Diagnostics for Ordinal Regression Models: A Surrogate Approach.
#'   Journal of the American Statistical Association, 113:522, 845-854.
#'
#'   Lumley, T. (2004) Analysis of complex survey samples. Journal of Statistical Software 9(1): 1-19
#'
#'   White, H. (1980), A heteroskedastic-consistent  covariance matrix estimator
#'   and a direct test of heteroskedasticity. Econometrica, 48, 817-838.
#' @return Generally, a list of class \code{Regression}. The exception being when \code{method = 'model.frame'}
#'  is a specified input argument. In that case, a \code{data.frame} is returned which returns only the \code{data}
#'  element from the \code{Regression} return list.
#'  The \code{Regression} return list contains the fitted regression object and other statistical outputs.
#'  These include elements \itemize{
#'  \item \code{robust.se} A logical specifying if robust standard error calculations were performed.
#'  \item \code{type} A character string specifying the Regression type (matches the input argument)
#'  \item \code{weights} A numeric vector of weights applied in the regression
#'  \item \code{output} A character vector specifying the output type specified in the input argument.
#'   Could be the table type or a separate analysis (see input argument for more details).
#'  \item \code{outlier.prop.to.remove} A numeric value specifying the proportion of outliers removed in the analysis.
#'  \item \code{show.labels} A logical value specifying if variable labels were used (\code{TRUE}) or variable names (\code{FALSE})
#'  \item \code{test.interaction} A logical value specifying if an interaction test was assessed in the output.
#'  \item \code{effects.format} A list containing input for the the relevant X and Y values for an effects plot output.
#'  \item \code{original} The initial standard R regression output (possibly refitted with outliers removed)
#'  \item \code{sample.description} A character string describing the regression and its inputs and outputs for
#'  printing in a footer of the output table.
#'  \item \code{summary} A \code{summary} of the \code{original} regression object above tidied up.
#'  \item \code{design} The survey design object accompanying any survey weighted regression (computed using the input \code{weights})
#'  \item \code{subset} The logical vector specifying which observations were filtered into the regression.
#'  \item \code{n.predictors} An integer specifying the number of predictors (not including the intercept) in the regression.
#'  \item \code{n.observations} An integer specifying the number of observations used in the regression after outliers are removed.
#'  \item \code{estimation.data} A \code{data.frame} containing the regression design matrix.
#'  This design matrix takes into account the subset, missing data options.
#'   \item \code{correction} A character string specifying the multiple comparisons correction used (see input arguments)
#'  \item \code{formula} A \code{formula} object for the regression model (before interaction term added)
#'  \item \code{model} A single \code{data.frame} of the input data with both predictors and outcome variable, possibly stacked
#'   and including imputed values or interaction term if applicable.
#'  \item \code{outcome.name} A character string of the outcome variable name as used in the formula.
#'  \item \code{outcome.label} A character string of the outcome variable label or
#'  name with possible back ticks removed if labels are not requested.
#'  \item \code{terms} A \code{terms} object from the \code{original} output element.
#'  \item \code{coef} The computed coefficients from the \code{original} output element.
#'  \item \code{r.squared} The \code{original} output element R squared (or equivalent)
#'  \item \code{z.statistics} Computed z-statistics from the set of coefficients in a Mulitnomial Logit model
#'  \item \code{p.values} Computed p-values for the z-statistics above
#'  \item \code{importance} A list of output relevant when the selected output is either a \code{"Relative Importance Analysis"},
#'  \code{"Shapley Regression"}, \code{"Jaccard Coefficient"} or \code{"Correlation"}. This list has elements
#'  \itemize{
#'      \item \code{raw.importance} The raw importance scores (regression coefficients, jaccard coefficients or correlations)
#'      \item \code{importance} The raw importance scores scaled to 0-100% for interpretative purposes.
#'      \item \code{standard.errors} The computed standard errors for the raw importance scores.
#'      \item \code{statistics} The computed standardised statistics of the raw importance scores
#'      \item \code{statistic.name} Character showing either the t or z statistic being used.
#'      \item \code{p.values} The vector of p-values for the relevant statistics computed above.
#'    }
#'  \item \code{importance.type} Character string specifying the type of Importance analysis requested
#'  \item \code{importance.names} Character vector of the names of the predictors in the importance analysis
#'  \item \code{importance.labels} Character vector of the labels of the predictors in the importance analysis
#'  \item \code{relative.importance} A copy of the \code{importance} output, kept for legacy purposes.
#'  \item \code{interaction} A list containing the regression analysis with an interaction term. The list has elements
#'  \itemize{
#'       \item \code{label} Character string of the variable label of the interaction variable.
#'       \item \code{split.size} Numeric vector of counts of each level of the interaction variable and a total NET count.
#'       \item \code{pvalue} p-value of overall test of significance of the Regression using a call to \code{stats::anova}
#'       \item \code{original.r2} Either the R squared for linear regression or
#'       proportion of deviance in model without interaction
#'       \item \code{full.r2} Either the R squared for linear regression or
#'       proportion of deviance in model with interaction
#'       \item \code{fit} Regression model with interaction
#'       \item \code{net.coef} Vector of regression coefficients or importance.scores
#'       \item \code{importance} The importance list of the Regression without interaction if applicable, \code{NULL} otherwise.
#'       \item \code{anova.output} The anova output for the Regression output without interaction.
#'       \item \code{anova.test} Character string of the Overall test of significance used (F or Chis-square)
#'       \item \code{coef.pvalues} Matrix of pvalues for the coefficients or importance scores used at each interaction level
#'       \item \code{coef.tstat} Matrix of statistics for the coefficients or importance scores used at each interaction level
#'       \item \code{coefficients} Matrix of coefficients or raw.importance scores used at each interaction level
#'    }
#'    \item \code{anova} Essentially is the return output of \code{\link[car]{Anova}} with relevant metadata added.
#'    This element only added when the input argument \code{output = 'ANOVA'} or \code{'Effects plot'}.
#'    \item \code{footer} Character string of the footer to appear in the output table
#'    \item \code{importance.footer} Character string of the footer to appear in the output table of an importance analysis
#'    \item \code{stacked} Logical element to specify if the data was stacked (\code{TRUE}) or not (\code{FALSE})
#'  }
#'  The \code{Regression} list also has a \code{'ChartData'} attribute that is used when exporting to XLS files.
#'  The contents of this attribute is a \code{data.frame} that gives the equivalent information and structure of the
#'  the formattable table output htmlwidget.
#' @importFrom stats pnorm anova update terms
#' @importFrom flipData GetData CleanSubset CleanWeights DataFormula
#' EstimationData CleanBackticks RemoveBackticks ErrorIfInfinity
#' AddDummyVariablesForNAs
#' @importFrom flipFormat Labels OriginalName BaseDescription
#' @importFrom flipU OutcomeName IsCount
#' @importFrom flipTransformations AsNumeric
#' CreatingBinaryDependentVariableIfNecessary Factor Ordered
#' @importFrom lmtest coeftest
#' @importFrom stats drop.terms terms.formula
#' @importFrom verbs Last Sum
#' @export
Regression <- function(formula = as.formula(NULL),
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
                       outlier.prop.to.remove = NULL,
                       stacked.data.check = FALSE,
                       unstacked.data = NULL,
                       ...)
{
    if (identical(formula, formula(NULL)) && !stacked.data.check)
        stop(dQuote("formula"), " argument is missing and is required unless stackable data is provided via the ",
             dQuote("stacked.data.check"), " and ", dQuote("unstacked.data"), " arguments. ",
             "Please provide a formula or stackable data and re-run the Regression.")
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
    } else if (output %in% c("Jaccard Coefficient", "Correlation"))
        output
    else
        NULL

    if (!is.null(importance) && is.null(importance.absolute))
        importance.absolute <- FALSE

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
    # Check if stackable data is input
    if (stacked.data.check)
    {
        stacked.data.output <- processAndStackData(unstacked.data, formula, interaction, subset, weights)
        # Update relevant terms
        data <- stacked.data.output[["data"]]
        formula <- input.formula <- stacked.data.output[["formula"]]
        interaction <- stacked.data.output[["interaction"]]
        subset <- stacked.data.output[["subset"]]
        weights <- stacked.data.output[["weights"]]
    } else
        input.formula <- formula # Hack to work past scoping issues in car package: https://cran.r-project.org/web/packages/car/vignettes/embedding.pdf.

    if (!is.null(interaction.formula))
    {
        # Inside internal loop, data is already set up properly
        formula.with.interaction <- interaction.formula
        data <- GetData(interaction.formula, data, auxiliary.data)
        interaction.name <- Last(colnames(data), 1)
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
    # Remove/Update dataset references and non-syntactic names if they exist
    ## Dataset references first
    ## Remove any dataset references e.g. mydata$Variables$Y
    all.variable.names <- AllVariablesNames(input.formula, data)
    if (any(vars.with.data.refs <- checkVariablesForDataSetNames(all.variable.names)))
    {
        names(all.variable.names) <- all.variable.names
        dataset.names <- lookupDataSetNames(all.variable.names[vars.with.data.refs], data)
        new.var.names <- removeDataSetReferences(vars.with.data.refs)
        new.var.names[vars.with.data.refs] <- paste0(new.var.names[vars.with.data.refs],
                                                     " from ",
                                                     dataset.names)
        original.labels <- Labels(data, all.variable.names)
        if (length(unique(dataset.names)) == 1L || !anyDuplicated(original.labels))
            labels.to.update <- FALSE
        else
            labels.to.update <- vars.with.data.refs
        if (any(labels.to.update))
        {
            new.labels <- paste0(original.labels[labels.to.update], " from ", dataset.names[labels.to.update])
            data <- updateAttribute(data, attr.to.update = "label", updated.values = new.labels)
        }
        syntactic.new.names <- make.names(new.var.names, unique = TRUE)
        if (!identical(unname(new.var.names), syntactic.new.names))
            names(syntactic.new.names) <- all.variable.names
        else
            syntactic.new.names <- new.var.names
        relabelled.outputs <- relabelFormulaAndData(syntactic.new.names, input.formula, data)
        input.formula <- relabelled.outputs$formula
        data <- relabelled.outputs$data
        all.variable.names <- AllVariablesNames(input.formula, data = data)
    }
    non.syntactic.names <- checkForNonSyntacticNames(all.variable.names)
    if (non.syntactic.names.exist <- !is.null(non.syntactic.names))
    {
        relabelled.outputs <- relabelFormulaAndData(non.syntactic.names, input.formula, data)
        input.formula <- relabelled.outputs$formula
        data <- relabelled.outputs$data
        all.variable.names <- AllVariablesNames(input.formula, data = data)
    }
    formula.has.dot <- "." %in% all.vars(input.formula)
    if (formula.has.dot)
        all.variable.names <- all.variable.names[all.variable.names != interaction.name]
    unique.syntactic.interaction.name <- Last(make.names(c(all.variable.names, interaction.name),
                                                         unique = TRUE),
                                              1)
    interaction.name.needs.updating <- !identical(interaction.name, unique.syntactic.interaction.name)
    if (any(vars.with.data.refs) || non.syntactic.names.exist || interaction.name.needs.updating)
    {
        if (is.null(interaction))
            formula.with.interaction <- input.formula
        else
        {
            formula.with.interaction <- update(terms(input.formula, data = data),
                                               sprintf(".~.*%s", unique.syntactic.interaction.name))
            names(data)[which(names(data) == interaction.name)] <- unique.syntactic.interaction.name
            interaction.name <- unique.syntactic.interaction.name
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
    if(Sum(outcome.name == names(data), remove.missing = FALSE) > 1)
        stop("The 'Outcome' variable has been selected as a 'Predictor'. It must be one or the other, but may not be both.")
    if (!is.null(weights) & length(weights) != nrow(data))
        stop("'weights' and 'data' are required to have the same number of observations. They do not.")
    if (!is.null(subset) & length(subset) > 1 & length(subset) != nrow(data))
        stop("'subset' and 'data' are required to have the same number of observations. They do not.")
    # Check if there are any entirely missing variables in the data and adjust accordingly.
    missing.variables <- vapply(data, function(x) all(is.na(x)), logical(1L))
    if(any(missing.variables))
    {
        missing.variable.adjustment <- removeMissingVariables(data, formula, formula.with.interaction,
                                                              missing.variables, outcome.name, input.formula)
        data <- missing.variable.adjustment$data
        formula <- missing.variable.adjustment$formula
        formula.with.interaction <- missing.variable.adjustment$formula.with.interaction
        input.formula <- missing.variable.adjustment$input.formula
    }

    if (type == "Binary Logit" || output == "Jaccard Coefficient")
    {
        if (type == "Binary Logit")
            data <- CreatingBinaryDependentVariableIfNecessary(input.formula, data)
        else
        {
            InterceptExceptions(data <- CreatingBinaryDependentVariableIfNecessary(input.formula, data),
                                warning.handler = function(w) {
                                    if (w$message == "The Outcome variable needs to contain two or more categories. It does not.")
                                        stop("The Outcome variable needs to be a binary variable. It is not. ",
                                             "It is constant with no variation (all values in the variable are the same). ",
                                             "Please replace the outcome variable with a binary variable.")
                                    else
                                        warning(w$message)
                                })
            # Convert it to numeric binary, 0,1, if required.
            if (is.factor(data[[outcome.name]]))
                data[[outcome.name]] <- unclass(data[[outcome.name]]) - 1
        }
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
        if (!is.null(importance) && importance %in% c("Relative Importance Analysis", "Shapley Regression"))
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
            variable.count <- Sum(sapply(data.for.levels, function(x) abs(length(levels(x)) - 1)), remove.missing = FALSE)
            n.data <- Sum(processed.data$post.missing.data.estimation.sample, remove.missing = FALSE)
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
            final.model$model <- data
            final.model$weights <- weights
            final.model$stacked <- stacked.data.check
            final.model <- setChartData(final.model, output)
            return(final.model)
        }
        unfiltered.weights <- processed.data$unfiltered.weights
        .estimation.data <- processed.data$estimation.data

        if (missing == "Dummy variable adjustment")
        {
            # Check for aliased dummy variables
            dummy.variables <- names(.estimation.data)[grepDummyVars(names(.estimation.data))]
            predictor.names <- names(.estimation.data)[-which(names(.estimation.data) == outcome.name)]
            mapping.variables <- lapply(dummy.variables,
                                        function(x) attr(.estimation.data[[x]], "predictors.matching.dummy"))
            names(mapping.variables) <- dummy.variables
            aliased.dummy.vars <- vapply(mapping.variables, function(x) length(x) > 1, logical(1))
            if (any(aliased.dummy.vars))
                aliasedDummyVariableWarning(data, mapping.variables[aliased.dummy.vars], show.labels, predictor.names)
            # Update formula to include dummy variables
            new.formulae <- updateDummyVariableFormulae(input.formula, formula.with.interaction,
                                                        data = processed.data$estimation.data)
            input.formula <- new.formulae$formula
            formula.with.interaction <- new.formulae$formula.with.interaction
        }



        n <- nrow(.estimation.data)
        if (n < ncol(.estimation.data) + 1)
            stop(warningSampleSizeTooSmall())
        post.missing.data.estimation.sample <- processed.data$post.missing.data.estimation.sample
        .weights <- processed.data$weights
        .formula <- DataFormula(input.formula, data)
        fit <- FitRegression(.formula, .estimation.data, .weights, type, robust.se,
                             outlier.prop.to.remove, seed = seed, ...)
        .estimation.data <- fit$.estimation.data
        .formula <- fit$formula
        if (internal)
        {
            fit$subset <- row.names %in% rownames(.estimation.data)
            fit$sample.description <- processed.data$description
            fit$stacked <- stacked.data.check
            return(fit)
        }
        original <- fit$original
        .design <- fit$design
        result <- list(original = original, call = cl)

        result$non.outlier.data <- fit$non.outlier.data

        if (!is.null(.design))
            result$design <- .design
        requireNamespace("car")
        if (missing == "Imputation (replace missing values with estimates)")
            data <- processed.data$data
        result$subset <- row.names %in% rownames(.estimation.data)
        result$n.predictors <- sum(!(names(result$original$coefficients) %in% "(Intercept)"))
        result$n.observations <- sum(.estimation.data$non.outlier.data_GQ9KqD7YOf)
        # If outliers are removed, remake the footer
        if (!is.null(outlier.prop.to.remove) && outlier.prop.to.remove != 0)
        {
            n.subset <- attr(CleanSubset(subset, nrow(data)), "n.subset")
            n.estimation <- result$n.observations
            # Create new base description up to the first ;
            new.base.description <- BaseDescription(paste0("n = ", FormatAsReal(n.estimation),
                                                           " cases used in estimation"),
                                                    n.total = nrow(data), n.subset = n.subset,
                                                    n.estimation = n.estimation,
                                                    subset.label = Labels(subset),
                                                    weighted = FALSE)
            # Match start of footer until first ;
            base.footer.pattern <- paste0("^.+(estimation|\\d|\\Q", Labels(subset), "\\E\\));")
            processed.data$description <- sub(base.footer.pattern, new.base.description,
                                              processed.data$description, perl = TRUE)
        }
        result$sample.description <- processed.data$description
        result$estimation.data <- .estimation.data
    }
    class(result) <- c("Regression", switch(type,
        "Linear" = "LinearRegression",
        "Binary Logit" = "BinaryLogitRegression",
        "Ordered Logit" = "OrderedLogitRegression",
        "Multinomial Logit" = "MultinomialLogitRegression",
        "Poisson" = "PoissonRegression",
        "Quasi-Poisson" = "QuasiPoissonRegression",
        "NBD" = "NBDRegression"))
    result$correction <- correction
    result$formula <- input.formula
    # Inserting the coefficients from the partial data.
    if (missing != "Dummy variable adjustment")
        result$model <- data
    else
        result$model <- AddDummyVariablesForNAs(data, outcome.name, checks = FALSE)

    result$robust.se <- robust.se
    result$type <- type
    result$weights <- unfiltered.weights
    result$filtered.weights <- weights
    result$output <- output
    result$outlier.prop.to.remove <- outlier.prop.to.remove
    result$show.labels <- show.labels
    result$missing <- missing
    result$test.interaction <- !is.null(interaction)
    result$effects.format <- effects.format

    # remove environment attribute to reduce size
    attr(result$original$terms, ".Environment") <- NULL
    attr(attr(result$original$model, "terms"), ".Environment") <- NULL
    ## remove names from residuals and fitted values to reduce size
    result$original$residuals <- unname(result$original$residuals)
    result$original$fitted.values <- unname(result$original$fitted.values)

    suppressWarnings(tmpSummary <- summary(result$original))
    result$summary <- tidySummary(tmpSummary, result$original, result)
    result$summary$call <- cl

    # Replacing the variables with their labels
    result$outcome.name <- outcome.name
    result$outcome.label <- RemoveBackticks(outcome.name)
    # Retain raw variable names for later use
    if (type == "Multinomial Logit")
        nms <- colnames(result$summary$coefficients)
    else
        nms <- rownames(result$summary$coefficients)
    # Add fancy labels to summary table if requested
    if (show.labels)
    {
        if (type == "Multinomial Logit")
            colnames(result$summary$coefficients) <- colnames(result$summary$standard.errors) <- Labels(data, nms)
        else
            rownames(result$summary$coefficients) <- Labels(data, nms)
        label <- Labels(outcome.variable)
        if (!is.null(label))
            result$outcome.label <- label
    }
    aliased.warning.labels <- if (show.labels) Labels(data, names(result$summary$aliased)) else NULL
    if (!recursive.call)
        aliased.preds <- aliasedPredictorWarning(result$summary$aliased, aliased.warning.labels)
    else
        aliased.preds <- suppressWarnings(aliasedPredictorWarning(result$summary$aliased, aliased.warning.labels))

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
        signs <- if (importance.absolute || partial) 1 else sign(extractVariableCoefficients(result$original, type))
        if (missing == "Dummy variable adjustment")
        {
            # Update the formula silently (don't throw a warning)
            signs <- if (importance.absolute) 1 else signs[!grepDummyVars(names(signs))]
            # The data only needs to be adjusted and the formula updated for the RIA if dummy variable
            # adjustment was used
            dummy.predictors <- grepDummyVars(names(result$original$coefficients))
            if (any(dummy.predictors))
            { # First check if any predictors with dummy vars are factors and throw error since RIA cannot be conducted
                preds.with.dummy <- extractDummyNames(names(result$original$coefficients)[dummy.predictors])
                factor.preds <- vapply(data[-which(names(data) == outcome.name)],
                                       inherits, what = "factor", logical(1))
                if (!is.null(interaction) && interaction.name %in% names(factor.preds))
                    factor.preds <- factor.preds[-which(names(factor.preds) == interaction.name)]
                if (any(factor.preds) && any(names(which(factor.preds)) %in% preds.with.dummy))
                {
                    factor.preds <- names(which(factor.preds))
                    factor.names <- if (show.labels) Labels(data, factor.preds) else factor.preds
                    stop("Dummy variable adjustment method for missing data is not supported for categorical predictor ",
                         "variables in ", output, ". Please remove the categorical predictors: ",
                         paste0(sQuote(factor.names, q = FALSE), collapse = ", "), " and re-run the analysis.")
                } # Otherwise, conduct the imputation type step in the data and update formula, removing dummy vars
                .estimation.data <- adjustDataMissingDummy(data, result$original, .estimation.data, show.labels)
                input.formula <- updateDummyVariableFormulae(formula = input.formula, formula.with.interaction = NULL,
                                                             data = processed.data$estimation.data,
                                                             update.string = " - ")$formula
                nms <- nms[!grepDummyVars(nms)]
            }
            result$formula <- input.formula
        }
        if (partial) # missing = "Use partial data (pairwise correlations)", possible option for Correlation and Jaccard output
        {
            result$subset <- subset
            result$estimation.data <- .estimation.data <- CopyAttributes(data[subset, , drop = FALSE], data)
            .weights <- weights[subset]
        }
        relevant.coefs <- !grepDummyVars(rownames(result$summary$coefficients))
        result$importance.names <- nms
        labels <- rownames(result$summary$coefficients)[relevant.coefs]
        if (result$type == "Ordered Logit")
        {  # Remove the response transition coefficients
            n.remove <- (nlevels(.estimation.data[[outcome.name]]) - 2)
            labels <- labels[-length(labels):-(length(labels) - n.remove)]
            n.importance.names <- length(result$importance.names)
            result$importance.names <- result$importance.names[-n.importance.names:-(n.importance.names - n.remove)]
        } else if (output == "Correlation")
        {
            labels <- attr(terms.formula(input.formula, data = data), "term.labels")
            labels <- result$importance.names <- labels[!grepDummyVars(labels)]
            if (show.labels)
                labels <- Labels(data, labels)
        } else
        {
            result$importance.names <- result$importance.names[-1]
            labels <- labels[-1]
        }
        result$importance.labels <- labels
        # Process the data suitable for Jaccard coefficient analysis
        if (output == "Jaccard Coefficient")
        {
            jaccard.processed <- processDataSuitableForJaccard(.estimation.data, input.formula,
                                                               interaction.name, show.labels)
            result$estimation.data <- .estimation.data <- jaccard.processed$data
            input.formula <- jaccard.processed$formula
            labels <- result$importance.labels <- jaccard.processed$labels
        }
        # Correlation outputs and Jaccard don't need to be checked since predictor importance is computed pairwise against the outcome.
        check.for.aliased.vars <- !output %in% c("Correlation", "Jaccard Coefficient")
        if (check.for.aliased.vars)
            validateDataForRIA(input.formula, .estimation.data, outcome.name, show.labels, output)
        # Remove prefix if possible
        extracted.labels <- ExtractCommonPrefix(labels)
        if (!is.na(extracted.labels$common.prefix))
            labels <- extracted.labels$shortened.labels
        result$importance <- estimateImportance(input.formula, .estimation.data, .weights,
                                                type, signs, result$r.squared,
                                                labels, robust.se, outlier.prop.to.remove,
                                                !recursive.call, correction, importance, ...)
        result$importance.type <- importance
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
    if (!is.null(result$outlier.prop.to.remove) && result$outlier.prop.to.remove > 0)
    {
        result$footer <- paste0(result$footer, "; the ", FormatAsPercent(result$outlier.prop.to.remove),
                                " most outlying observations in the data have been removed and the model refitted;")
        if (!is.null(result$importance))
            result$importance.footer <- paste(result$importance.footer, "the",
                                              FormatAsPercent(result$outlier.prop.to.remove),
                                              "most outlying observations in the data have been removed",
                                              "and the model refitted;")
    }
    result <- setChartData(result, output)
    result$stacked <- stacked.data.check
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
    rsummary$deviance.resid <- unname(rsummary$deviance.resid)
    rsummary$model <- rsummary$design <- NULL  # reduce size of summary
    rsummary$lp <- unname(rsummary$lp)

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
    # Suppress the footer addition for the Jaccard and Correlation outputs
    if (!x$test.interaction && !x$importance %in% c("Jaccard Coefficient", "Correlation"))
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
#' @param .weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param type See \link{Regression}.
#' @param robust.se Computes standard errors that are robust to violations of
#'   the assumption of constant variance, using the HC1 (degrees of freedom)
#'   modification of White's (1980) estimator (Long and Ervin, 2000). This parameter is ignored
#'   if weights are applied (as weights already employ a sandwich estimator).
#' @param outlier.prop.to.remove A single numeric value that determines the percentage of data points to remove from the
#'   analysis. The data points removed correspond to those in the proportion with the largest residuals.
#'   A value of 0 or NULL would denote no points are removed. A value x, with 0 < x < 0.5 (not inclusive) would
#'   denote that a percentage between none and 50\% of the data points are removed.
#' @param seed A integer seed to generate the surrogate residuals.
#' @param ... Arguments to the wrapped functions.
#' @importFrom flipData CalibrateWeight WeightedSurveyDesign
#' @importFrom flipFormat FormatAsPercent
#' @importFrom flipU InterceptExceptions
#' @importFrom MASS polr glm.nb
#' @importFrom nnet multinom
#' @importFrom stats glm lm poisson quasipoisson binomial pt quasibinomial
#' @importFrom survey svyglm svyolr
#' @export
FitRegression <- function(.formula, .estimation.data, .weights, type, robust.se, outlier.prop.to.remove,
                          seed = 12321, ...)
{
    .design <- NULL
    # Initially fit model on all the data and then refit with outlier removal if necessary
    fitted.model <- fitModel(.formula, .estimation.data, .weights, type,
                             robust.se, subset = NULL, ...)
    model <- fitted.model$model
    .design <- fitted.model$design
    .estimation.data <- fitted.model$estimation.data
    .formula <- fitted.model$formula
    remove.outliers <- checkAutomaterOutlierRemovalSetting(outlier.prop.to.remove, .estimation.data)
    # Don't support Multinomial Logit for now.
    if (remove.outliers && type == "Multinomial Logit")
    {
        warning("Automated outlier removal and re-fitting a 'Multinomial Logit' model is not supported",
                "Regression model is fitted without outlier removal.")
        remove.outliers <- FALSE
    }
    if (remove.outliers)
    {
        refitted.model.data <- refitModelWithoutOutliers(model, .formula, .estimation.data, .weights,
                                                         type, robust.se, outlier.prop.to.remove, seed = seed,...)
        model <- refitted.model.data$model
        .estimation.data <- refitted.model.data$.estimation.data
        .design <- refitted.model.data$.design
        non.outlier.data <- refitted.model.data$non.outlier.data
    } else
        non.outlier.data <- rep(TRUE, nrow(.estimation.data))

    result <- list(original = model, formula = .formula, .estimation.data = .estimation.data,
                   design = .design, weights = .weights, robust.se = robust.se,
                   non.outlier.data = non.outlier.data)
    class(result) <- "FitRegression"
    result
}

# Fits the models. Now in a separate function to FitRegression for repeated use
# to prevent code duplication with the automated outlier removal refitting.
fitModel <- function(.formula, .estimation.data, .weights, type, robust.se, subset, ...)
{
    weights <- .weights #Does nothing, except remove notes from package check.
    .design <- non.outlier.data_GQ9KqD7YOf <- NULL
    if (is.null(subset))
    {
        non.outlier.data <- rep(TRUE, nrow(.estimation.data))
        .estimation.data$non.outlier.data_GQ9KqD7YOf <- non.outlier.data
    } else
        non.outlier.data <- subset
    # Protect against . operator used in RHS of formula
    # If so, it will include non.outlier.data as a predictor now.

    formula.terms <- terms.formula(.formula, data = .estimation.data)
    if (any(non.outlier.in.data.frame <- attr(formula.terms, "term.labels") == "non.outlier.data_GQ9KqD7YOf"))
        .formula <- update.formula(.formula, drop.terms(formula.terms,
                                                        which(non.outlier.in.data.frame),
                                                        keep.response = TRUE))
    if (is.null(.weights))
    {
        if (type == "Linear")
        {
            model <- lm(.formula, .estimation.data, subset = non.outlier.data_GQ9KqD7YOf, model = TRUE)
            model$aic <- AIC(model)
        }
        else if (type == "Poisson" | type == "Quasi-Poisson" | type == "Binary Logit")
            model <- glm(.formula, .estimation.data, subset = non.outlier.data_GQ9KqD7YOf,
                         family = switch(type,
                                         "Poisson" = poisson,
                                         "Quasi-Poisson" = quasipoisson,
                                         "Binary Logit" = binomial(link = "logit")))
        else if (type == "Ordered Logit")
        {
            model <- fitOrderedLogit(.formula, .estimation.data, NULL,
                                     non.outlier.data_GQ9KqD7YOf = non.outlier.data, ...)
            model$aic <- AIC(model)
        }
        else if (type == "Multinomial Logit")
        {
            model <- multinom(.formula, .estimation.data, subset = non.outlier.data_GQ9KqD7YOf,
                              Hess = TRUE, trace = FALSE, MaxNWts = 1e9, maxit = 10000, ...)
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

            result <- withWarnings(glm.nb(.formula, .estimation.data, subset = non.outlier.data_GQ9KqD7YOf))
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
            model <- svyglm(.formula, .design, subset = non.outlier.data_GQ9KqD7YOf, ...)
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
            model <- fitOrderedLogit(.formula, .estimation.data, weights,
                                     non.outlier.data_GQ9KqD7YOf = non.outlier.data, ...)
        else if (type == "Multinomial Logit")
        {
            .estimation.data$weights <- CalibrateWeight(.weights)
            model <- multinom(.formula, .estimation.data, weights = weights, subset = non.outlier.data_GQ9KqD7YOf,
                              Hess = TRUE, trace = FALSE, maxit = 10000, MaxNWts = 1e9, ...)
            model$aic <- AIC(model)
        }
        else if (type == "NBD")
        {
            .estimation.data$weights <- CalibrateWeight(.weights)
            model <- InterceptExceptions(
                glm.nb(.formula, .estimation.data, weights = weights, subset = non.outlier.data_GQ9KqD7YOf, ...),
                warning.handler = function(w) {
                    if (w$message == "iteration limit reached")
                        warning("Model may not have converged. If the dispersion parameter from the Detail",
                                " output is large, a Poisson model may be appropriate.")
                    else
                        warning(w$message)
                })
        }
        else
        {
            .design <- WeightedSurveyDesign(.estimation.data, .weights)
            model <- switch(type,
                            "Binary Logit" = svyglm(.formula, .design, subset = non.outlier.data_GQ9KqD7YOf,
                                                    family = quasibinomial()),
                            "Poisson" = svyglm(.formula, .design, subset = non.outlier.data_GQ9KqD7YOf,
                                               family = poisson()),
                            "Quasi-Poisson" = svyglm(.formula, .design, subset = non.outlier.data_GQ9KqD7YOf,
                                                     family = quasipoisson()))
            assign(".design", .design, envir=.GlobalEnv)
            aic <- extractAIC(model)
            remove(".design", envir=.GlobalEnv)
            model$df <- aic[1]
            model$aic <- aic[2]
        }
    }
    return(list(model = model, formula = .formula, design = .design, estimation.data = .estimation.data))
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
#' @importFrom stats vcov
#' @export
vcov.Regression <- function(object, robust.se = FALSE, ...)
    vcov2(object$original, robust.se, ...)

#' @importFrom car hccm
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
        # Catch the case where singularities in the robust.se calculation.
        hat.values <- hatvalues(fit.reg)
        if (any(hat.values == 1) && !robust.se %in% c("hc0", "hc1"))
            v <- hccmAdjust(fit.reg, robust.se, hat.values)
        else
            v <- hccm(fit.reg, type = robust.se)
    }
    FixVarianceCovarianceMatrix(v)
    v
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
#' @importFrom stats vcov
#' @export
vcov.FitRegression <- function(object, robust.se = FALSE, ...)
    vcov.Regression(object, robust.se, ...)

#' FixVarianceCovarianceMatrix
#'
#' Makes some adjustments to permit a covariance-marix to be inverted, if required.
#' @param x A variance-covariance matrix of parameter estimates.
#' @param min.eigenvalue Minimm acceptable eigenvalue.
#' @details Sandwich and sandwich-like standard errors can result uninvertable
#' covariance matrices (e.g., if a parameter represents a sub-group, and the sub-group has no
#' residual variance). This function checks to see if there are any eigenvalues less than \code{min.eigenvalue},
#' which defaults to 1e-12. If there are, an attempt is made to guess the offending variances, and they are multiplied by 1.01.
#' @export
FixVarianceCovarianceMatrix <- function(x, min.eigenvalue = 1e-12)
{
    wng <- paste0("This is most likely due to either a problem or the appropriateness of the statistical ",
                  "model (e.g., using weights or robust standard errors where a sub-group in the analysis ",
                  "has no variation in its residuals, or lack of variation in one or more predictors.)")
    v <- x
    v <- try(
        {
            if (min(eigen(v)$values) >= min.eigenvalue)
                return(v)
            v.diag <- diag(v)
            n.similar.to.diag <- abs(sweep(v, 1, v.diag, "/"))
            high.r <- apply(n.similar.to.diag > 0.99, 1, sum) > 1
            # Adjust appropriate parts of diagonal if possible
            if (any(high.r))
                diag(v)[high.r] <- v.diag[high.r] * 1.01
            else # Otherwise give overall adjustment if no offending terms found.
                diag(v) <- diag(v) * 1.01
            v
        }, silent = TRUE
    )
    if (tryError(v))
        stop("There is a technical problem with the parameter variance-covariance matrix.", wng)
    else
        warning("There is a technical problem with the parameter variance-covariance matrix ",
                "that has been corrected.", wng)
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
        names.aliased <- names(aliased)
        dummy.aliased <- aliased[grepDummyVars(names.aliased)]
        alias.vars <- if (!is.null(aliased.labels)) aliased.labels else names(aliased)
        regular.aliased <- aliased[!grepDummyVars(names.aliased)]
        regular.warning <- paste0("The following variable(s) are colinear with other variables and no",
                                  " coefficients have been estimated: ",
                                  paste(sQuote(alias.vars[regular.aliased], q = FALSE), collapse = ", "))
        if (any(regular.aliased))
            warning(regular.warning)
        return(regular.aliased)
    }
    aliased
}

#' @importFrom stats model.frame model.matrix model.response
fitOrderedLogit <- function(.formula, .estimation.data, weights, non.outlier.data_GQ9KqD7YOf, ...)
{
    .orderedLogitWarnings <- function(w) {
        if (w$message == "design appears to be rank-deficient, so dropping some coefs")
            warning("Some variable(s) are colinear with other variables ",
                    "and they have been removed from the estimation.")
        else
            warning(w$message)
    }
    .orderedLogitErrors <- function(e) {
        if (exists(".design"))
        {
            m <- model.frame(.formula, model.frame(.design), na.action = na.pass)
            y <- model.response(m)
            unobserved.levels <- paste0(setdiff(levels(y), y), collapse = ", ")
            if (unobserved.levels != "")
                stop("Outcome variable has level(s): ", unobserved.levels, " that are not observed in the data. ",
                     "If possible, this issue could be solved by merging the categories of the outcome variable ",
                     "such that all categories appear in all sub-groups.")
        }
        if (grepl(c("response must have 3 or more levels|cannot be dichotimized as it only contains one level.$"),
                  e$message))
        {
            outcome <- OutcomeVariable(.formula, .estimation.data)
            outcome.levels <- levels(outcome)
            base.error.msg <- paste0("Fitting an Ordered Logit model requires the outcome variable ",
                                     "to have three or more levels. The outcome variable here has ")
            levels.msg <- paste0(sQuote(outcome.levels), collapse = " and ")
            if (length(outcome.levels) == 1)
                stop(base.error.msg, "only one level: ", levels.msg, ". A Regression model cannot be computed ",
                     "when the outcome variable has no variation.")
            else
                stop(base.error.msg, "two levels: ", levels.msg, ". Consider using a Binary Logit model instead.")
        } else if (grepl("attempt to find suitable starting values failed|initial value in 'vmmin' is not finite", e$message))
        {
            stop("It is not possible to fit the Ordered Logit Regression model since a suitable starting point ",
                 "cannot be found for the iterative algorithm used for fitting the model. ",
                 "It is recommended to check the input data to see if it is appropriate. ",
                 "If possible, consider merging categories in the outcome variable that don't have many observations. ",
                 "It is also worth checking that there is sufficient variation in the predictor variables for ",
                 "each level in the outcome variable.")
        } else
            stop("An error occurred during model fitting. ",
                 "Please check your input data for unusual values: ", e$message)
    }
    model <- InterceptExceptions(
        if (is.null(weights))
        {
            withRestarts(
                tryCatch(polr(.formula, .estimation.data, subset = non.outlier.data_GQ9KqD7YOf,
                              Hess = TRUE, ...),
                         error = function(e) {
                             invokeRestart("polrWithStart", findAppropriateStartingValueForOrderedLogit(.formula, .estimation.data))
                             }),
                polrWithStart = function(x0) polr(.formula, .estimation.data, subset = non.outlier.data_GQ9KqD7YOf, Hess = TRUE, start = x0))
        } else
        {
            # svyolr (currently 21/01/2020) doesn't have a subset argument
            # Instead manually filter the data using the provided non.outlier.data column
            .estimation.data <- .estimation.data[, names(.estimation.data) != "non.outlier.data_GQ9KqD7YOf",
                                                 drop = FALSE]
            .estimation.data <- .estimation.data[non.outlier.data_GQ9KqD7YOf, , drop = FALSE]
            .design <- WeightedSurveyDesign(.estimation.data, weights[non.outlier.data_GQ9KqD7YOf])
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
            # Retain the design and formula for later use
            out$formula <- .formula
            out$design <- .design
            out
        },
        warning.handler = .orderedLogitWarnings,
        error.handler = .orderedLogitErrors)
}

#' @importFrom flipTransformations DichotomizeFactor
#' @importFrom stats glm.fit
findAppropriateStartingValueForOrderedLogit <- function(.formula, .estimation.data, cutoff = 0.5)
{
    outcome.name <- OutcomeName(.formula, .estimation.data)
    y <- DichotomizeFactor(.estimation.data[[outcome.name]], cutoff = cutoff, warning = FALSE)
    X <- model.matrix(.formula, .estimation.data)
    initial.logit.fit <- glm.fit(X, y, family = binomial())
    if (!initial.logit.fit$converged)
        stop("attempt to find suitable starting values failed")
    coefs <- initial.logit.fit$coefficients
    if (any(is.na(coefs))) {
        warning("design appears to be rank-deficient, so dropping some coefs")
        keep <- names(coefs)[!is.na(coefs)]
        coefs <- coefs[keep]
        x <- x[, keep[-1L], drop = FALSE]
        pc <- ncol(x)
    }
    q <- nlevels(.estimation.data[[outcome.name]]) - 1L
    first.level <- levels(y)[1]
    cut.point <- substr(first.level, 4L, nchar(first.level))
    q1 <- which(levels(.estimation.data[[outcome.name]]) == cut.point)
    logit <- function(p) log(p/(1 - p))
    spacing <- logit((1L:q)/(q + 1L))
    gammas <- -coefs[1L] + spacing - spacing[q1]
    c(coefs[-1L], gammas)
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
                                         "Shapley Regression",
                                         "Jaccard Coefficient",
                                         "Correlation"))
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
    return(list(data = data, formula = formula, input.formula = input.formula,
                formula.with.interaction = formula.with.interaction))
}

# Helper function to check user has input a valid value.
checkAutomaterOutlierRemovalSetting <- function(outlier.prop.to.remove, estimation.data)
{
    if ((remove.outliers <- (!is.null(outlier.prop.to.remove) && outlier.prop.to.remove > 0)) && outlier.prop.to.remove >= 0.5)
        stop("At most, 50% of the data can be removed as part of the Automated Outlier Removal process. ",
             FormatAsPercent(outlier.prop.to.remove), " of outliers were asked to be removed, please set this ",
             " to a lower setting and re-run the analysis.")
    n <- nrow(estimation.data)
    p <- ncol(estimation.data)
    outlier.prop.to.remove <- if (is.null(outlier.prop.to.remove)) 0 else outlier.prop.to.remove
    if (floor(n * (1 - outlier.prop.to.remove)) < p + 1 && outlier.prop.to.remove > 0)
        stop(warningSampleSizeTooSmall(), " If ", outlier.prop.to.remove * 100, "% of the outlying data is ",
             "removed there will be less data than parameters to predict in the model which is not possible. ",
             " Consider a simpler model with less parameters or change the automated outlier removal setting ",
             " to a smaller value.")
    remove.outliers
}

# Identifies the outliers and refits the model
#' @importFrom flipU InterceptExceptions
refitModelWithoutOutliers <- function(model, formula, .estimation.data, .weights,
                                      type, robust.se, outlier.prop.to.remove, seed, ...)
{
    non.outlier.data <- findNonOutlierObservations(.estimation.data,
                                                   outlier.prop.to.remove,
                                                   model,
                                                   type,
                                                   .weights,
                                                   seed)
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
                    stop("Removing outliers has removed all the observations in the outcome variable with level(s): ",
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

# Returns a logical vector of observations that are not deemed outlier observations
#' @importFrom sure resids
#' @importFrom stats rstudent
findNonOutlierObservations <- function(data, outlier.prop.to.remove, model, type, weights, seed)
{
    n.model <- nrow(data)
    # In the Ordered Logit and Binary Logit cases use the Surrogate residuals
    # for both the weighted and non-weighted models
    if (type %in% c("Ordered Logit", "Binary Logit"))
    {
        set.seed(seed)
        if (type == "Ordered Logit")
        {
            if (!is.null(weights))
            {
                assign(".design", model$design, envir=.GlobalEnv)
                assign(".formula", model$formula, envir=.GlobalEnv)
                model.residuals <- resids(model, method = "latent")
                remove(".design", envir=.GlobalEnv)
                remove(".formula", envir=.GlobalEnv)
            } else
                model.residuals <- resids(model, method = "latent")
        }
        else
            model.residuals <- resids(model, method = "jitter", type = "response")
    }
    else
    {
        # use standardised deviance residuals in unweighted cases.
        # otherwise use the Pearson sampling re-weighted residuals.
        # These are computed in residuals.svyglm except in the NBD case.
        # NBD kept as Pearson for consistency.
        if (is.null(weights))
        {
            if (type %in% c("Linear", "Poisson", "Quasi-Poisson", "NBD"))
                model.residuals <- rstudent(model, type = "deviance")
            else
                stop("Unexpected or unsupported regression for automated outlier removal type: ", type)
        } else {
            if (type %in% c("Linear", "Poisson", "Quasi-Poisson", "NBD"))
                model.residuals <- residuals(model, type = "pearson")
            else
                stop("Unexpected or unsupported regression for automated outlier removal type: ", type)
        }
    }
    bound <- ceiling(n.model * (1 - outlier.prop.to.remove))
    valid.data.indices <- unname(rank(abs(model.residuals), ties.method = "random") <= bound)
    return(valid.data.indices)
}

# Takes the input unstacked data, interaction, subset, weights and formula terms
# Processes the unstacked data and stacks it.
# If the stacking is successful, the interaction, subset, weights are also updated
# to be the appropriate size
processAndStackData <- function(unstacked.data, formula, interaction, subset, weights)
{
    checkDataFeasibleForStacking(unstacked.data)
    unstacked.data <- removeDataReduction(unstacked.data)
    validated.unstacked.output <- validateDataForStacking(unstacked.data)
    unstacked.data <- validated.unstacked.output[["data"]]
    stacks <- validated.unstacked.output[["stacks"]]
    data <- stackData(unstacked.data)
    # Update interaction, subset and weights if necessary
    # if interaction vector supplied
    # it should be original n, needs to be stacked to n = nv where v is number oof outcome vars
    if (!is.null(interaction))
    {
        if (length(interaction) != nrow(data))
        {
            old.interaction <- interaction
            interaction <- rep(old.interaction, stacks)
            interaction <- CopyAttributes(interaction, old.interaction)
        }

        # Update subset to be consistent with interaction
        old.subset <- subset
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
    } else if (!is.null(subset) && length(subset) > 1)
    {
        old.subset <- subset
        subset <- rep(old.subset, stacks)
        subset <- CopyAttributes(subset, old.subset)
    }
    # Update weights
    if (!is.null(weights) && length(weights) != nrow(data))
    {
        old.weights <- weights
        weights <- rep(weights, stacks)
        weights <- CopyAttributes(weights, old.weights)
    }

    # Update formula
    formula <- updateStackedFormula(data, formula)
    list(data = data, formula = formula, interaction = interaction, subset = subset, weights = weights)
}

# Removes the data reduction columns and the reduction via the codeframe attribute, if available
# Also warn the user if necessary for NETs with both hidden and visible codes.
# Input is expected to be a list with two elements, Y, the outcome variable dataset and element X,
# containing the Predictor variable dataset
removeDataReduction <- function(data)
{
    # This list keeps track of the metadata for the warning
    reduction.list <- list(nets = NULL, variable.type = NULL)

    # Inspect the outcome multi first
    secondary.codeframe <- attr(data[["Y"]], "secondarycodeframe")
    codeframe <- attr(data[["Y"]], "codeframe")
    if (is.null(secondary.codeframe) && !is.null(codeframe))
    {
        reduction.columns <- flagCodeframeReduction(codeframe)
        data[["Y"]][reduction.columns] <- NULL
        attr(data[["Y"]], "codeframe")[reduction.columns] <- NULL
        reduction.list[['nets']] <- attr(reduction.columns, "nets")
        reduction.list[['variable.type']] <- "Outcome"
    } else if(!is.null(attr(data[["Y"]], "questiontype")))
    {# If older Q user, check question type and remove NET or SUM (default reduction)
        reduction.columns <- names(data[["Y"]]) %in% c("NET", "SUM")
        data[["Y"]][reduction.columns] <- NULL
    }

    # Clean the DataReduction for the predictor variables
    if (!is.null(question.type <- attr(data[["X"]], "questiontype")))
    {
        if (!all(c("codeframe", "secondarycodeframe") %in% names(attributes(data[["X"]]))))
        { # Use the default reduction names if reduction cant be deduced
            data.reduction.string <- if(question.type == "PickAnyGrid") "NET" else "SUM"
            grep.pattern <- paste0("(^", data.reduction.string, ", )|(, ", data.reduction.string,"$)")
            reduction.columns <- grepl(grep.pattern, names(data$X))
        } else
        { # Determine the data reduction columns from the codeframe and secondary codeframe
            codeframe <- attr(data[["X"]], "codeframe")
            secondary.codeframe <- attr(data[["X"]], "secondarycodeframe")
            reduction.rows <- flagCodeframeReduction(codeframe)
            reduction.columns <- flagCodeframeReduction(secondary.codeframe)
            if (!is.null(attr(reduction.rows, "net")) || !is.null(attr(reduction.columns, "net")))
            { # Retain the metadata
                reduction.list[['nets']] <- c(reduction.list[['nets']],
                                             c(attr(reduction.rows, "nets"), attr(reduction.columns, "nets")))
                reduction.list[['variable.type']] <- c(reduction.list[['variable.type']], "Predictor")
            }
            # Remove the data reduction from the codeframes for later use when determining the names
            if (any(reduction.rows))
                attr(data[["X"]], "codeframe")[reduction.rows] <- NULL
            if (any(reduction.columns))
                attr(data[["X"]], "secondarycodeframe")[reduction.columns] <- NULL
            # Remap to the columns in the data.frame
            reduction.columns <- apply(expand.grid(reduction.rows, reduction.columns), 1, any)
        }
        if (any(reduction.columns))
            data[["X"]][reduction.columns] <- NULL
    }
    # Warn the user if necessary
    if (!is.null(reduction.list[['nets']]))
        throwCodeReductionWarning(reduction.list)
    data
}

# Identify if there are any elements in a codeframe (or secondarycodeframe)
# that are completely redundant data reductions.
# This is achieved by looking at the longest list element in the codeframe and
# checking that all the numeric indicies in the longest element exist
# in other elements.
# Input x is the codeframe attribute
flagCodeframeReduction <- function(x)
{
    flags <- rep(FALSE, length(x))
    names(flags) <- names(x)
    # Check if there are any duplicated variables and flag them for removal
    # unname incase the user renames the duplicated variable
    if (any(duplicated.vars <- duplicated(unname(x))))
        flags[duplicated.vars] <- TRUE
    # Check the remaining non duplicates
    non.duplicated <- x[!duplicated.vars]
    lengths <- vapply(non.duplicated, length, numeric(1))
    # Inspect possible NETs after duplicated vars are removed
    possible.nets <- lengths > 1
    if (any(possible.nets))
    {
        removed.vals <- list() # Keep track of values that have been removed.
        complete.reduction <- list() # Keep track of nets that are complete data reductions.
        possible.redundant.nets <- which(possible.nets)
        # Order by decreasing size and remove one-by-one to handle supersets.
        net.sizes <- sort(lengths[possible.redundant.nets], decreasing = TRUE)
        for (net in names(net.sizes))
        {
            # Remove current net candidate from search list
            reduced.codeframe <- non.duplicated[-which(names(non.duplicated) == net)]
            if (any(non.duplicated[[net]] %in% unlist(reduced.codeframe)))
            { # Update flags and remove from original list
                flags[which(names(flags) == net)] <- TRUE
                removed.vals[[net]] <- non.duplicated[[net]]
                complete.reduction[[net]] <- all(unlist(reduced.codeframe) %in% non.duplicated[[net]])
                non.duplicated[net] <- NULL
            }
        }
        # If there are some nets to be removed, check the codes and warn if necessary (NET not mutually exclusive or complete)
        if (length(removed.vals) != 0)
        {
            # Check if there are any codes in the removed constructed nets that are not observed anywhere else
            unobserved.codes <- lapply(removed.vals, function(x) {
                y <- !x %in% unlist(reduced.codeframe)
                names(y) <- x
                y
            })
            # Only worry about nets that have unobserved codes and are not a complete reduction (complete NET or SUM)
            unobserved.in.nets <- vapply(unobserved.codes, any, logical(1)) & !vapply(complete.reduction, any, logical(1))
            # Add the metadata to the return logical vector for a simpler warning message.
            if (any(unobserved.in.nets))
            { # Reverse the the order of identified NETs with unobserved codes to show smaller NET groups first.
                attr(flags, "nets")  <- rev(names(which(unobserved.in.nets)))
            }
        }
    }
    flags
}

# Checks to be coded
checkDataFeasibleForStacking <- function(data)
{
    checkListStructure(data)
    checkNumberObservations(data)
    validMultiOutcome(data[["Y"]])
    validGridPredictor(data[["X"]])
}

checkListStructure <- function(data)
{
    named.elements <- c("X", "Y") %in% names(data)
    if ((is.null(data) || !(is.list(data) && all(named.elements))))
        stop("'unstacked.data' needs to be a list with two elements, ",
             "'Y' containing a data.frame with the outcome variables and ",
             "'X' containing a data.frame with the predictor variables.")
}

validateDataForStacking <- function(data)
{
    outcome.names <- getMultiOutcomeNames(data[["Y"]])
    # Validate the Grid predictors, transpose if necessary and error if no matches between X and Y
    data[["X"]] <- validateNamesInGrid(data)
    names.in.predictor.grid <- getGridNames(data[["X"]])

    outcome.names.in.grid.elements <- vapply(names.in.predictor.grid, function(x) {
        any(outcome.names %in% x)
        }, logical(1))
    outcome.names.in.grid <- names.in.predictor.grid[[which(outcome.names.in.grid.elements)]]
    unique.outcome.names.in.grid <- unique(outcome.names.in.grid)
    # Remove any outcome variables that aren't seen in predictors and warn
    data[["Y"]] <- validateOutcomeVariables(data, outcome.names, unique.outcome.names.in.grid)
    outcome.names <- getMultiOutcomeNames(data[["Y"]])

    # Remove any predictor variables that aren't seen in outcome variables and warn
    data[["X"]] <- validatePredictorVariables(data, outcome.names,
                                              unique.outcome.names.in.grid,
                                              outcome.names.in.grid)
    names.in.predictor.grid <- getGridNames(data[["X"]])
    outcome.names.in.grid.elements <- vapply(names.in.predictor.grid, function(x) {
        any(outcome.names %in% x)
    }, logical(1))
    outcome.names.in.grid <- names.in.predictor.grid[[which(outcome.names.in.grid.elements)]]
    unique.outcome.names.in.grid <- unique(outcome.names.in.grid)
    # Ensure columns align before stacking
    data[["Y"]] <- checkStackAlignment(data, outcome.names, unique.outcome.names.in.grid)
    return(list(data = data, stacks = ncol(data[["Y"]])))
}

# The stacking requires names of the grid data.frame to be in the form predictor, outcome (comma separated)
# If metadata available in the codeframe, the names are uniquely identified
# If metadata is unavailable, no commas allowed in names to avoid ambiguity.
#' @importFrom verbs Sum
validateNamesInGrid <- function(data)
{
    outcome.names <- getMultiOutcomeNames(data[["Y"]])
    if (!is.null(outcome.question.name <- attr(data[["Y"]], "question")))
        outcome.variable.set.name <- sQuote(outcome.question.name)
    else
        outcome.variable.set.name <- sQuote("Y")
    # Determine which dimension labels in the grid match the outcome.names
    # getGridNames extracts a list with two elements, the "a, b" parts of the grid names
    grid.names <- getGridNames(data[["X"]])
    # Check if any labels match
    matches <- lapply(grid.names, function(x) outcome.names %in% x)
    any.matches <- vapply(matches, any, logical(1))
    # No labels match at all, error since there is nothing to align for stacking
    if (all(!any.matches))
        stop("It is not possible to stack these variables since none of the outcome variable labels ",
             "match the variable labels in the predictor variables. The outcome variables ",
             outcome.variable.set.name, " have labels: ", paste0(sQuote(outcome.names), collapse = ", "),
             " which don't appear in the labels of the grid of predictor variables.")
    # Check if is a clear match (no clash of predictor names with outcome names) and no codeframe available,
    # then 'transpose' the grid labels, i.e. outcome, predictor labels changed to predictor, outcome
    dimensions.matching <- Sum(any.matches, remove.missing = FALSE)
    if (dimensions.matching == 1 && any.matches[1] && is.null(attr(data[["X"]], "codeframe")))
        names(data[["X"]]) <- paste0(grid.names[[2]], ", ", grid.names[[1]])
    # Throw error for ambiguous cases, i.e. outcome labels appear in both grid label dimensions.
    if (dimensions.matching == 2)
    {
        matched.outcomes <- outcome.names[unique(unlist(lapply(matches, which)))]
        ambiguous.message <- paste0("The outcome variable ", outcome.variable.set.name, " has labels: ",
                                    paste0(sQuote(matched.outcomes), collapse = ", "), " and these labels appear ",
                                    "in both dimensions of the grid predictor variables. Please rename the ",
                                    "labels in either the outcome variables or grid predictor variables to ",
                                    "stack the variables and proceed.")
        stop("Ambiguous labels in the grid predictors need to be reconciled before stacking can occur. ",
             ambiguous.message)
    }
    return(data[["X"]])
}

validMultiOutcome <- function(data)
{
    if (class(data) != "data.frame")
        stop("Outcome variable to be stacked needs to be a data.frame. " ,
             "Please assign a data.frame to the \"Y\" element of the 'unstacked.data' argument.")
}

checkNumberObservations <- function(data)
{
    if (!diff(unlist(nrows <- lapply(data, NROW))) == 0)
    {
        stop("Size of variables doesn't agree, the provided outcome variables have ", nrows[["Y"]],
             " observations while the provided predictor variables have ", nrows[["X"]],
             " observations. Please input variables that have the same size.")
    }
}

validGridPredictor <- function(data)
{
    if (class(data) != "data.frame")
        stop("Predictor variables to be stacked needs to be a data.frame. " ,
             "Please assign a data.frame to the \"X\" element of the 'unstacked.data' argument.")
}

validateOutcomeVariables <- function(data, outcome.names, predictor.names)
{
    if (any(missing.stack <- !outcome.names %in% predictor.names))
    {
        data[["Y"]][missing.stack] <- NULL
        removed.outcome.variables <- paste0(sQuote(outcome.names[missing.stack]), collapse = ", ")
        outcome.variable.set.name <- attr(data[["Y"]], "question")
        predictor.variable.set.name <- attr(data[["X"]], "question")
        if (is.null(outcome.variable.set.name) | is.null(predictor.variable.set.name))
            warning("The variable(s): ", removed.outcome.variables, " have been removed from the set of outcome ",
                    "variables since these variables don't appear in the set of predictor variables.")
        else
            warning("The variable(s): ", removed.outcome.variables, " have been removed from the set of outcome ",
                    "variables in ", sQuote(outcome.variable.set.name), " since they don't appear in the set of ",
                    "predictor variables in ", sQuote(predictor.variable.set.name))
        # Remove the name from the codeframe too,
        if (!is.null(attr(data[["Y"]], "secondarycodeframe")))
            attr(data[["Y"]], "secondarycodeframe")[missing.stack] <- NULL
        else if (!is.null(attr(data[["Y"]], "codeframe")))
            attr(data[["Y"]], "codeframe")[missing.stack] <- NULL
    }
    return(data[["Y"]])
}

validatePredictorVariables <- function(data, outcome.names, predictor.names, unstacked.names)
{
    if (any(unstackable.predictors <- !predictor.names %in% outcome.names))
    {
        unstackable.predictor.names <- predictor.names[unstackable.predictors]
        data[["X"]][unstacked.names %in% unstackable.predictor.names] <- NULL
        removed.predictor.variables <- paste0(sQuote(unstackable.predictor.names), collapse = ", ")
        outcome.variable.set.name <- attr(data[["Y"]], "question")
        predictor.variable.set.name <- attr(data[["X"]], "question")
        if (is.null(outcome.variable.set.name) | is.null(predictor.variable.set.name))
            warning("The variable(s): ", removed.predictor.variables, " have been removed from the set of predictor ",
                    "variables since these variables don't appear in the outcome variables.")
        else
            warning("The variable(s): ", removed.predictor.variables, " have been removed from the set of predictor ",
                    "variables in ", sQuote(predictor.variable.set.name), " since they don't appear in the set of ",
                    "outcome variables in ", sQuote(outcome.variable.set.name))

        # Remove the name from the codeframe too
        if (!is.null(attr(data[["X"]], "codeframe")))
        {
            # Determine if the outcome labels are stored in codeframe or secondarycodeframe
            codeframe.names <- names(attr(data[["X"]], "codeframe"))
            correct.codeframe <- if (any(predictor.names %in% codeframe.names))
                                    "codeframe"
                                 else
                                    "secondarycodeframe"
            attr(data[["X"]], correct.codeframe)[unstackable.predictors] <- NULL
        }

    }
    return(data[["X"]])
}

#' @importFrom flipU CopyAttributes
checkStackAlignment <- function(data, outcome.names, predictor.names)
{
    if (!identical(outcome.names, predictor.names))
    {
        new.column.order <- match(predictor.names, outcome.names)
        tmp <- data[["Y"]]
        if (!is.null(attr(tmp, "secondarycodeframe")))
            attr(tmp, "secondarycodeframe") <- attr(tmp, "secondarycodeframe")[new.column.order]
        else if (!is.null(attr(tmp, "codeframe")))
            attr(tmp, "codeframe") <- attr(tmp, "codeframe")[new.column.order]
        data[["Y"]] <- data[["Y"]][new.column.order]
        data[["Y"]] <- CopyAttributes(data[["Y"]], tmp)
    }
    return(data[["Y"]])
}

stackData <- function(data)
{
    outcome.names <- getMultiOutcomeNames(data[["Y"]])
    stacked.outcome <- stackOutcome(data[["Y"]], outcome.names)
    stacked.predictors <- stackPredictors(data[["X"]], outcome.names)
    if (!all(row.names(stacked.outcome) == row.names(stacked.predictors)))
        stop("Stacked variables are not aligned properly. Contact support for further help.")
    stacked.data <- cbind(stacked.outcome, stacked.predictors)
    return(stacked.data)
}

#' @importFrom stats reshape
stackPredictors <- function(data, outcome.names)
{
    question.label <- attr(data, "question")
    if (!is.null(codeframe <- attr(data, "codeframe")) &&
        !is.null(secondary.codeframe <- attr(data, "secondarycodeframe")))
    {
        if (any(outcome.names %in% names(codeframe)))
        {
            predictor.names <- names(secondary.codeframe)
            variables.to.stack <- lapply(predictor.names, function(x) paste0(x, ", ", outcome.names))
            names(variables.to.stack) <- predictor.names
        } else
        {
            predictor.names <- names(codeframe)
            variables.to.stack <- lapply(predictor.names, function(x) paste0(outcome.names, ", ", x))
            names(variables.to.stack) <- predictor.names
        }
        stacked.data <- reshape(data, varying = variables.to.stack, times = outcome.names, sep = ",",
                                v.names = predictor.names, direction = "long")
    }
    else
        stacked.data <- reshape(data, varying = names(data), sep = ", ",
                                times = outcome.names, direction = "long")
    stacked.data <- removeReshapingHelperVariables(stacked.data)
    stacked.data <- addLabelAttribute(stacked.data, label = question.label)
    names(stacked.data) <- paste0("X", 1:ncol(stacked.data))
    stacked.data
}

#' @importFrom stats reshape
stackOutcome <- function(data, outcome.names)
{
    v.name <- if (!is.null(question.attr <- attr(data ,"question"))) question.attr else "Y"
    stacked.data <- reshape(data, varying = names(data), v.names = v.name,
                            times = outcome.names, direction = "long")
    stacked.data <- removeReshapingHelperVariables(stacked.data)
    stacked.data <- addLabelAttribute(stacked.data)
    names(stacked.data) <- "Y"
    stacked.data
}

addLabelAttribute <- function(data, label = NULL)
{
    variable.names <- colnames(data)
    if (!is.null(label))
        variable.names <- paste0(label, ": ", variable.names)
    for (i in seq_along(data))
        attr(data[[i]], "label") <- variable.names[i]
    data
}

removeReshapingHelperVariables <- function(data)
{
    data[["id"]] <- NULL
    data[["time"]] <- NULL
    data
}

# Return the names of the predictors and their associated matched response values
# It assumes that outcome names are given as the second comma separate value
# and predictor names would be the first or alternatively
# it assumes that the codeframe has the outcome names and secondarycodeframe has
# the predictor names.
# If this is incorrect, then it will be corrected or matched in either
# validateNamesInGrid or validateDataForStacking
getGridNames <- function(data)
{
    if (all(c("codeframe", "secondarycodeframe") %in% names(attributes(data))))
    {
        outcome.names <- names(attr(data, "codeframe"))
        m <- length(outcome.names)
        predictor.names <- names(attr(data, "secondarycodeframe"))
        p <- length(predictor.names)
        predictor.names <- rep(predictor.names, each = m)
        outcome.names <- rep(outcome.names, p)
    } else
    {
        split.names <- strsplit(names(data), ", ")
        splits <- vapply(split.names, length, numeric(1))
        if (any(ambiguous.splits <- splits != 2))
            stop("The variable labels in the predictor grid should be comma separated to determine the columns ",
                 "that belong to the appropriate outcome variable. This means that the variable labels cannot ",
                 "use commas. Please remove the commas in the names in the predictor grid to continue ",
                 "the analysis. The variable labels that are ambiguous and require fixing are: ",
                 paste0(sQuote(names(data)[ambiguous.splits]), collapse = ", "))
        outcome.names <- sapply(split.names, "[", 2)
        predictor.names <- sapply(split.names, "[", 1)
    }
    list(predictor.names, outcome.names)
}

getMultiOutcomeNames <- function(data)
{
    if (!is.null(secondary.codeframe <- attr(data, "secondarycodeframe")))
        names(secondary.codeframe)
    else if (!is.null(code.frame <- attr(data, "codeframe")))
        names(code.frame)
    else
        names(data)
}

updateStackedFormula <- function(data, formula)
{
    new.formula <- as.formula(paste0("Y ~ ", paste0("X", 1:(ncol(data) - 1), collapse = " + ")),
                              env = environment(formula))
    return(new.formula)
}

#' Throw warning to the user that there are some codes used in a data reduction that are not seen
#' elsewhere in the codeframe, e.g. a NET has code A and B and the code A is seen in the codeframe
#' but the code B is not seen elsewhere in the codeframe. So removing the data reduction will lose
#' the information about code B.
#' @param reduction.list List that contains two elements, \itemize{
#'  \item nets Character vector of net(s) that exhibit this situation
#'  \item variable.type Character vector stating the variable set structure that this applies to.
#' e.g. The predictor grid or outcome variables. Could be a single element or vector with two elements
#' if both outcome and predictor grids are affected.
#' }
#' @noRd
throwCodeReductionWarning <- function(reduction.list)
{
    nets <- reduction.list[["nets"]]
    variable.type <- reduction.list[["variable.type"]]
    net.name.txt <- paste0(sQuote(nets, q = FALSE))
    if (length(nets) > 1)
        net.name.txt <- paste0(": (", paste0(net.name.txt, collapse = "; "), ")")
    net.txt <- ngettext(length(nets), "a NET, ", "NETs")
    variable.type.and <- paste0(paste0(variable.type, collapse = " and "), " variables")
    variable.type.or <- paste0(paste0(variable.type, collapse = " or "), " variables")
    net.txt.2 <- ngettext(length(nets), "this NET was", "these NETs were")
    net.txt.3 <- ngettext(length(nets), "this NET", "any of these NETs")
    warning("NETs are removed from this analysis unless all their source values are mutually exclusive ",
            "to other codes. The ", variable.type.and, " have ", net.txt, net.name.txt, " that contains ",
            "source values that partially overlap with other codes. Consequently, ", net.txt.2, " not used in ",
            "the analysis. If you wish ", net.txt.3, " to be used in the analysis then please modify the ",
            variable.type.or, " via the Table view options appropriately.")
}

# Updates a formula and optionally a formula with interaction
# It checks if the formula has any dummy variables in the data and either adds or removes
# predictors for those dummy variables in the formula
# The control to add or remove is via the update.string argument, " + " adds to the formulae
# while " - " removes dummy variables from the formulae
updateDummyVariableFormulae <- function(formula, formula.with.interaction, data,
                                        update.string = " + ")
{
    if (!any(dummy.vars <- grepDummyVars(names(data))))
        return(list(formula = formula, formula.with.interaction = formula.with.interaction))

    dummy.var <- paste0(names(data)[dummy.vars], collapse = update.string)
    new.formula <- update(terms(formula, data = data), as.formula(paste0(". ~ .", update.string, dummy.var)))
    if (!is.null(formula.with.interaction))
        new.formula.with.interaction <- update(terms(formula.with.interaction, data = data),
                                               as.formula(paste0(". ~ .", update.string, dummy.var)))
    else
        new.formula.with.interaction <- NULL
    return(list(formula = new.formula, formula.with.interaction = new.formula.with.interaction))
}

grepDummyVars <- function(string, dummy.pattern = ".dummy.var_GQ9KqD7YOf$") grepl(dummy.pattern, string)

# Function to impute the missing values in original data to give the equivalent regression
# coefficients to those generated in the dummy adjusted model.
# Requires the original data, the fitted model to access the regression coefficients along with the
# design matrix from the estimation.data and the show.labels logical value in case any warnings
# are to be reported if the missing values caused aliased dummy predictors.
#' @importFrom flipFormat Labels
#' @importFrom stats terms.formula
adjustDataMissingDummy <- function(data, model, estimation.data, show.labels)
{
    model.formula <- formula(model)
    formula.terms <- terms(model.formula)
    outcome.name <- as.character(attr(terms(model.formula), "variables"))[2]
    outcome.variable <- data[[outcome.name]]
    design.data <- data[-which(names(data) == outcome.name)]
    if (!any(sapply(design.data, function(x) any(is.na(x)))))
        return(estimation.data)
    # Compute means
    predictor.names <- attr(terms(model.formula), "term.labels")
    data.for.means <- data[which(names(data) %in% predictor.names)]
    means.from.data <- lapply(data.for.means, function(x) {
        if (is.numeric(x)) mean(x, na.rm = TRUE) else NULL})
    # Check if any aliasing of dummy variables has occurred, first extract mapping
    dummy.variables <- names(estimation.data)[grepDummyVars(names(estimation.data))]
    mapping.variables <- lapply(dummy.variables,
                                function(x) attr(estimation.data[[x]], "predictors.matching.dummy"))
    names(mapping.variables) <- dummy.variables
    # dummy variables were aliased if mapping points to more than one original variable
    aliased <- vapply(mapping.variables, function(x) length(x) > 1, logical(1))
    if (any(aliased))
    {
        mapping.variables <- mapping.variables[aliased]
        extra.coefs <- lapply(names(mapping.variables),
                              function(x) {
                                  vars <- mapping.variables[[x]][-1] # 1st element is redundant
                                  y <- rep(model$coefficients[[x]], length(vars))
                                  names(y) <- paste0(vars, ".dummy.var_GQ9KqD7YOf")
                                  y
                              })
        model.coefs <- c(model$coefficients, unlist(extra.coefs))
    } else
        model.coefs <- model$coefficients
    missing.replacements <- extractDummyAdjustedCoefs(model.coefs, means.from.data)
    missing.numeric <- vapply(names(missing.replacements),
                              function(x) is.numeric(design.data[[x]]),
                              logical(1))
    for (pred.name in names(missing.numeric)[which(missing.numeric)])
    {
        x.col <- design.data[[pred.name]]
        x.col[is.na(x.col)] <- missing.replacements[[pred.name]]
        design.data[[pred.name]] <- x.col
    }
    new.data <- cbind.data.frame(data[outcome.name], design.data, deparse.level = 0)
    # Check if any removed cases in .estimation.data
    if (!all(cases.to.include <- row.names(data) %in% row.names(estimation.data)))
        new.data <- new.data[cases.to.include, ]
    # Add outlier removal column
    new.data[["non.outlier.data_GQ9KqD7YOf"]] <- estimation.data[["non.outlier.data_GQ9KqD7YOf"]]
    new.data <- CopyAttributes(new.data, data)
    return(new.data)
}

# Gives an informative message to the user if there are any aliased dummy variables
# data is the entire regression dataset with attributes if provided.
# mapping.variables is the list showing the mapping from the dummy variables to the group of
# predictors
# show.labels is the logical used to print either names or labels in warning
# predictor.names is the character vector of all predictor names used in data
#' @importFrom flipFormat Labels ExtractCommonPrefix
aliasedDummyVariableWarning <- function(data, mapping.variables, show.labels, predictor.names)
{
    all.mapped.variables <- unlist(mapping.variables, use.names = FALSE)
    if (show.labels)
    {
        # Look up names from the full predictor set of names to ensure appropriate truncation
        predictor.names <- predictor.names[!grepDummyVars(predictor.names)]
        lbls <- Labels(data, names.to.lookup = predictor.names)
        names(lbls) <- predictor.names
        extracted <- ExtractCommonPrefix(lbls)
        if (!is.na(extracted$common.prefix))
            lbls <- extracted$shortened.labels
        lbls <- lbls[names(lbls) %in% all.mapped.variables]
    } else
    { # Otherwise just use the names and retain the
        lbls <- all.mapped.variables
        names(lbls) <- all.mapped.variables
    }
    n.groups <- length(mapping.variables)
    groups <- lapply(mapping.variables, function(x) paste0(lbls[x], collapse = "; "))
    if (n.groups > 1)
    {
        groups <- lapply(1:n.groups, function(x) paste0("Group ", x, " (", groups[[x]], ")"))
        groups <- unlist(groups)
        group.msg <- paste0(paste0(groups[1:(n.groups - 1)], collapse = ", "), " and ", groups[n.groups])
        warning.msg <- paste0("Some groups of predictors have exactly the same cases with ",
                              "missing values and consequently, only a single dummy variable was ",
                              "used to adjust the data for each group. ", group.msg, ". The dummy ",
                              "variables would be aliased if each predictor in each group had ",
                              "its own dummy variable.")
    } else
    {
        group <- unlist(groups, use.names = FALSE)
        warning.msg <- paste0("The predictor variables: (", group, ") have exactly the same cases ",
                              "with missing values. Consequently, only a single dummy variable was used to ",
                              "adjust the data for these predictors. The dummy variables would be ",
                              "aliased if each predictor in this group had its own dummy variable.")
    }
    warning(warning.msg)
}

extractDummyAdjustedCoefs <- function(coefficients, computed.means)
{
    dummy.variables <- grepDummyVars(names(coefficients))
    dummy.variable.names <- extractDummyNames(names(coefficients)[dummy.variables])
    standard.variables <- grepl(paste0("^", dummy.variable.names, "$", collapse = "|"), names(coefficients))
    standard.variable.names <- names(coefficients)[standard.variables]
    slope.values <- as.list(coefficients[standard.variables])
    names(slope.values) <- standard.variable.names
    dummy.values <- as.list(coefficients[dummy.variables])
    names(dummy.values) <- dummy.variable.names
    adjusted.vals <- lapply(dummy.variable.names, function(x) {
        computed.means[[x]] + dummy.values[[x]]/slope.values[[x]]})
    names(adjusted.vals) <- dummy.variable.names
    return(adjusted.vals)
}

extractDummyNames <- function(string, dummy.pattern = ".dummy.var_GQ9KqD7YOf$")
    sapply(strsplit(string, dummy.pattern), "[", 1)

# This function should only be called when
# 1. Robust.se is requested and the method is not hc0 or hc1
# 2. There are hat values at 1 (causing a singularity)
# Arguments required are
# 1. fit.reg: The original R regression object
# 2. robust.se: The hc method string options are ("hc2", "hc3" or "hc4")
# 3. h: the numeric hat values for all n observations.
#' @importFrom stats df.residual model.matrix residuals coef
hccmAdjust <- function(fit.reg, robust.se, h)
{
    # Setup the calculation like a standard call to hccm
    V <- summary(fit.reg)$cov.unscaled
    e <- residuals(fit.reg)
    df.res <- df.residual(fit.reg)
    n <- length(e)
    aliased <- is.na(coef(fit.reg))
    X <- model.matrix(fit.reg)[, !aliased, drop = FALSE]
    p <- ncol(X)
    # Replace the singularities with compuatable values.
    # Using the the hc1 calculation for those cases.
    factor <- switch(robust.se, hc2 = 1 - h, hc3 = (1 - h)^2, hc4 = (1 - h)^pmin(4, n * h/p))
    factor[h == 1] <- df.res/n
    V %*% t(X) %*% apply(X, 2, "*", (e^2)/factor) %*% V
}
