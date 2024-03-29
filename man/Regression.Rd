% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regression.R
\name{Regression}
\alias{Regression}
\title{Generalized Regression Outputs}
\usage{
Regression(
  formula = NULL,
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
  interaction.formula = NULL,
  recursive.call = FALSE,
  effects.format = list(max.label = 10),
  outlier.prop.to.remove = NULL,
  stacked.data.check = FALSE,
  unstacked.data = NULL,
  ...
)
}
\arguments{
\item{formula}{An object of class \code{\link{formula}} (or one that can be
coerced to that class): a symbolic description of the model to be fitted.
The details of type specification are given under \sQuote{Details}.}

\item{data}{A \code{\link{data.frame}}.}

\item{subset}{An optional vector specifying a subset of observations to be
used in the fitting process, or, the name of a variable in \code{data}. It
may not be an expression. \code{subset} may not}

\item{weights}{An optional vector of sampling weights, or, the name or, the
name of a variable in \code{data}. It may not be an expression.}

\item{missing}{How missing data is to be treated in the regression. Supplied parameter needs to be one
of the following strings:
\itemize{
\item \code{"Error if missing data"}: Throws an error if any case has a missing value,
\item \code{"Exclude cases with missing data"}: Filters out cases that have any missing values,
\item \code{"Use partial data (pairwise correlations)"}: This method is only valid for \code{type = "Linear"}
   and it computes the pairwise correlations using all the available data. Given the pairwise computed
   correlation matrix, a linear regression model is computed.
\item \code{"Dummy variable adjustment"}: This method assumes that the missing data is structurally missing and
   that a predictor could be impossible for some cases and hence coded as missing. E.g. if a non-married
   person is asked to rate the quality of their marriage (or a person with no pets is asked the age of their
   pet). A model with a structure to allow this is used whereby the missing predictor is removed and an
   intercept adjustment is performed. This is implemented by adding a dummy variable for each predictor that
   has at least one missing value. The dummy indicator variables take the value zero if the original predictor
   has a non-missing value and the value one if the original predictor is missing. The original missing value
   is then recoded to a new value. In particular, missing numeric predictors are recoded to be the mean of the
   predictor (excluding the missing data) and factors are recoded to have their missing values recoded as the
   reference level of the factor.
\item \code{"Imputation (replace missing values with estimates)"}: Missing values in the data are imputed via the
   use of the predictive mean matching method implemented in \code{\link[mice]{mice}}. Missing values in the
   outcome variable are excluded from the analysis after the imputation has been performed. If the
   \code{\link[mice]{mice}} method fails, hot-decking is used via the \code{\link[hot.deck]{hot.deck}} package.
\item \code{"Multiple imputation"}: Implements the imputation method described above with option
    \code{"Imputation (replace missing values with estimates)"}. However, repeated imputation data is computed
    \code{m} times (see parameter below). The repeated set of imputed datasets are used to create \code{m} sets
    of separate regression coefficients that are then combined to produce a single set of estimated regression
    coefficients.
}}

\item{type}{Defaults to \code{"Linear"}. Other types are: \code{"Poisson"},
\code{"Quasi-Poisson"}, \code{"Binary Logit"}, \code{"NBD"},
\code{"Ordered Logit"}, and \code{"Multinomial Logit"}}

\item{robust.se}{If \code{TRUE}, computes standard errors that are robust to violations of
the assumption of constant variance for linear models, using the HC3 modification of White's (1980) estimator
(Long and Ervin, 2000). This parameter is ignored if weights are applied (as weights already
employ a sandwich estimator). Other options are \code{FALSE} and \code{"FALSE"No}, which do the same
thing, and \code{"hc0"}, \code{"hc1"}, \code{"hc2"}, \code{"hc4"}. A singularity can occur when a robust
estimator is requested that depends on the influence (\code{"hc2"}, \code{"hc3"} or \code{"hc4"}) and
at least one observation has a hat value of one. In that case, division by zero could occur and to avoid
this the robust estimator is still computed but the observations with a singularity are adjusted
with the degrees of freedom adjustment (\code{"hc1"}).}

\item{method}{The method to be used; for fitting. This will only do something if
method = "model.frame", which returns the model frame.}

\item{output}{\code{"Coefficients"} returns a table of coefficients and various
summary and model statistics. It is the default.
\code{"ANOVA"} returns an ANOVA table.
\code{"Detail"} returns a more traditional R output.
\code{"Relative Importance Analysis"} returns a table with Relative Importance scores.
\code{"Shapley Regression"} returns a table with Shapley Importance scores.
\code{"Effects Plot"} returns the effects plot per predictor.
\code{"Jaccard Coefficient"} returns the a relative importance table using the computed Jaccard
  coefficient of the outcome variable against each predictor. The outcome and predictor variables need
  to be binary variables for this output.
\code{"Correlation"} returns a relative importance table using the Pearson correlation
  of the outcome variable against each predictor.}

\item{detail}{This is a deprecated function. If \code{TRUE}, \code{output} is set to \code{R}.}

\item{m}{The number of imputed samples, if using multiple imputation.}

\item{seed}{The random number seed used in imputation and residual computations.}

\item{statistical.assumptions}{A Statistical Assumptions object.}

\item{auxiliary.data}{A \code{\link{data.frame}} containing additional variables
to be used in imputation (if required). While adding more variables will improve
the quality of the imputation, it will dramatically slow down the time to estimate.
Factors and Character variables with a large number of categories should not be included,
as they will both slow down the data and are unlikely to be useful}

\item{show.labels}{Shows the variable labels, as opposed to the names, in the outputs, where a
variables label is an attribute (e.g., attr(foo, "label")).}

\item{internal}{If \code{TRUE}, skips most of the tidying at the end. Only for use when it is
desired to call a relatively light version of Regression for other purposes (e.g., in ANOVA).
This leads to creation of an object of class \code{FitRegression}.)}

\item{contrasts}{A vector of the contrasts to be used for \code{\link{factor}} and
\code{\link{ordered}} variables. Defaults to \code{c("contr.treatment", "contr.treatment"))}.
Set to \code{c("contr.treatment", "contr.poly"))} to use orthogonal polynomials for \code{\link{factor}}
See \code{\link{contrasts}} for more information.}

\item{relative.importance}{Deprecated. To run Relative Importance Analysis, use the output variable.}

\item{importance.absolute}{Whether the absolute value of the relative importance should be shown.}

\item{interaction}{Optional variable to test for interaction with other variables in the model.
Output will be a crosstab showing coefficients from both both models.}

\item{correction}{Method to correct for multiple comparisons. Can be one of \code{"None"},
\code{"False Discovery Rate", "Benjamini & Yekutieli", "Bonferroni", "Hochberg", "Holm"} or \code{"Hommel"}.}

\item{interaction.formula}{Used internally for multiple imputation.}

\item{recursive.call}{Used internally to indicate if call is a result of recursion (e.g., multiple imputation).}

\item{effects.format}{A list of items \code{max.label} (the maximum length of a factor label on the x-axis)
and \code{y.title} (the title of the y-axis, defaults to outcome label).}

\item{outlier.prop.to.remove}{A single numeric value that determines the percentage of data points to remove from the
analysis. The data points removed correspond to those in the proportion with the largest residuals.
A value of 0 or NULL would denote no points are removed. A value x, with 0 < x < 0.5 (not inclusive) would
denote that a percentage between none and 50\% of the data points are removed.}

\item{stacked.data.check}{Logical value to determine if the Regression should be the data and formula based off
the \code{unstacked.data} argument by stacking the input and creating a formula based off attributes and provided
labels in the data. More details are given in the argument details for \code{unstacked.data}}

\item{unstacked.data}{A list with two elements that provide the outcome and predictor variables respectively for data
that needs to be stacked. See details section for more information.}

\item{...}{Additional argments to be passed to  \code{\link{lm}} or, if the
data is weighted,  \code{\link[survey]{svyglm}} or \code{\link[survey]{svyolr}}.}
}
\value{
Generally, a list of class \code{Regression}. The exception being when \code{method = 'model.frame'}
 is a specified input argument. In that case, a \code{data.frame} is returned which returns only the \code{data}
 element from the \code{Regression} return list.
 The \code{Regression} return list contains the fitted regression object and other statistical outputs.
 These include elements \itemize{
 \item \code{robust.se} A logical specifying if robust standard error calculations were performed.
 \item \code{type} A character string specifying the Regression type (matches the input argument)
 \item \code{weights} A numeric vector of weights applied in the regression
 \item \code{output} A character vector specifying the output type specified in the input argument.
  Could be the table type or a separate analysis (see input argument for more details).
 \item \code{outlier.prop.to.remove} A numeric value specifying the proportion of outliers removed in the analysis.
 \item \code{show.labels} A logical value specifying if variable labels were used (\code{TRUE}) or variable names (\code{FALSE})
 \item \code{test.interaction} A logical value specifying if an interaction test was assessed in the output.
 \item \code{effects.format} A list containing input for the the relevant X and Y values for an effects plot output.
 \item \code{original} The initial standard R regression output (possibly refitted with outliers removed)
 \item \code{sample.description} A character string describing the regression and its inputs and outputs for
 printing in a footer of the output table.
 \item \code{summary} A \code{summary} of the \code{original} regression object above tidied up.
 \item \code{design} The survey design object accompanying any survey weighted regression (computed using the input \code{weights})
 \item \code{subset} The logical vector specifying which observations were filtered into the regression.
 \item \code{n.predictors} An integer specifying the number of predictors (not including the intercept) in the regression.
 \item \code{n.observations} An integer specifying the number of observations used in the regression after outliers are removed.
 \item \code{estimation.data} A \code{data.frame} containing the regression design matrix.
 This design matrix takes into account the subset, missing data options.
  \item \code{correction} A character string specifying the multiple comparisons correction used (see input arguments)
 \item \code{formula} A \code{formula} object for the regression model (before interaction term added)
 \item \code{model} A single \code{data.frame} of the input data with both predictors and outcome variable, possibly stacked
  and including imputed values or interaction term if applicable.
 \item \code{outcome.name} A character string of the outcome variable name as used in the formula.
 \item \code{outcome.label} A character string of the outcome variable label or
 name with possible back ticks removed if labels are not requested.
 \item \code{terms} A \code{terms} object from the \code{original} output element.
 \item \code{coef} The computed coefficients from the \code{original} output element.
 \item \code{r.squared} The \code{original} output element R squared (or equivalent)
 \item \code{z.statistics} Computed z-statistics from the set of coefficients in a Mulitnomial Logit model
 \item \code{p.values} Computed p-values for the z-statistics above
 \item \code{importance} A list of output relevant when the selected output is either a \code{"Relative Importance Analysis"},
 \code{"Shapley Regression"}, \code{"Jaccard Coefficient"} or \code{"Correlation"}. This list has elements
 \itemize{
     \item \code{raw.importance} The raw importance scores (regression coefficients, jaccard coefficients or correlations)
     \item \code{importance} The raw importance scores scaled to 0-100% for interpretative purposes.
     \item \code{standard.errors} The computed standard errors for the raw importance scores.
     \item \code{statistics} The computed standardised statistics of the raw importance scores
     \item \code{statistic.name} Character showing either the t or z statistic being used.
     \item \code{p.values} The vector of p-values for the relevant statistics computed above.
   }
 \item \code{importance.type} Character string specifying the type of Importance analysis requested
 \item \code{importance.names} Character vector of the names of the predictors in the importance analysis
 \item \code{importance.labels} Character vector of the labels of the predictors in the importance analysis
 \item \code{relative.importance} A copy of the \code{importance} output, kept for legacy purposes.
 \item \code{interaction} A list containing the regression analysis with an interaction term. The list has elements
 \itemize{
      \item \code{label} Character string of the variable label of the interaction variable.
      \item \code{split.size} Numeric vector of counts of each level of the interaction variable and a total NET count.
      \item \code{pvalue} p-value of overall test of significance of the Regression using a call to \code{stats::anova}
      \item \code{original.r2} Either the R squared for linear regression or
      proportion of deviance in model without interaction
      \item \code{full.r2} Either the R squared for linear regression or
      proportion of deviance in model with interaction
      \item \code{fit} Regression model with interaction
      \item \code{net.coef} Vector of regression coefficients or importance.scores
      \item \code{importance} The importance list of the Regression without interaction if applicable, \code{NULL} otherwise.
      \item \code{anova.output} The anova output for the Regression output without interaction.
      \item \code{anova.test} Character string of the Overall test of significance used (F or Chis-square)
      \item \code{coef.pvalues} Matrix of pvalues for the coefficients or importance scores used at each interaction level
      \item \code{coef.tstat} Matrix of statistics for the coefficients or importance scores used at each interaction level
      \item \code{coefficients} Matrix of coefficients or raw.importance scores used at each interaction level
   }
   \item \code{anova} Essentially is the return output of \code{\link[car]{Anova}} with relevant metadata added.
   This element only added when the input argument \code{output = 'ANOVA'} or \code{'Effects plot'}.
   \item \code{footer} Character string of the footer to appear in the output table
   \item \code{importance.footer} Character string of the footer to appear in the output table of an importance analysis
   \item \code{stacked} Logical element to specify if the data was stacked (\code{TRUE}) or not (\code{FALSE})
 }
 The \code{Regression} list also has a \code{'ChartData'} attribute that is used when exporting to XLS files.
 The contents of this attribute is a \code{data.frame} that gives the equivalent information and structure of the
 the formattable table output htmlwidget.
}
\description{
Computes output for seven different regression types.
Those being linear, binary logistic, ordered logistic, binomial, poisson, quasi-poisson and
multinomial. Output includes general coefficient estimates and importance analysis estimates
with possibilities for handling missing data and interaction terms.
}
\details{
In the case of Ordered Logistic regression, this function computes a proportional odds model using
 the cumulative link (logistic). In the case of no weights, the \code{\link[MASS]{polr}} function is used.
 In the case of a weighted regression, the \code{\link[survey]{svyolr}} function is used.

 "Imputation (replace missing values with estimates)". All selected
 outcome and predictor variables are included in the imputation, along with
 all \code{auxiliary.data}, excluding cases that are excluded via subset or
 have invalid weights, but including cases with missing values of the outcome variable.
 Then, cases with missing values in the outcome variable are excluded from
 the analysis (von Hippel 2007). See \code{\link[flipImputation]{Imputation}}.

 Outlier removal is performed by computing residuals for the regression model and removing the largest residuals
 from the dataset (outlier removal). The model is then refit on the reduced dataset after outliers are removed.
 The residuals used in this process depend on the regression type. For a regression with a numeric response
 (\code{type} is "Linear", "Poisson", "Quasi-Poisson" or  "NBD") in an unweighted regression, the studentised
 deviance residuals are used (see Davison and Snell (1991) and \code{\link[stats]{rstudent}}). In the weighted case
 of the numeric response, the Pearson residuals are used (see Davison and Snell (1991) and
 \code{\link[stats]{residuals.glm}}). In the case of Binary and Ordinal data for both the unweighted and weighted
 regression cases, the Surrogate residuals (SURE) are used (via the implementation in Greenwell, McCarthy and
 Boehmke (2017) with their sure R package). This was based on the recent theoretical paper in Liu and Zhang (2018).
 Currently "Multinomial Logit" is unsupported for automated outlier removal. Possible surrogate residual to be used
 in a future version.

 In the case of stacking using the \code{unstacked.data} argument, it is designed to work best with input that is
 created with Q or Displayr which contains  \code{data.frame}s with a particular structure. If the Q/Displayr
 \code{data.frame}s are not available then simple \code{data.frame}s can be provided. In particular, a list is
 required with two elements, \itemize{
  \item \code{Y} A \code{data.frame} with \code{m} columns that represent the \code{m} variables to be stacked.
  \item \code{X} A \code{data.frame} where each column represents a column of a design matrix relevant to one of the
  \code{m} variables given in element \code{Y} above. So if the overall regression model has \code{p} predictors.
  Then this \code{data.frame} should contain \code{m * p} columns. In the absence of Q/Displayr metadata, the naming
  structure each column is comma separated of the form 'predictor, outcome' where 'predictor' denotes the
  predictor name in the regression design matrix and 'outcome' denotes the name of the variable in element \code{Y}.
  This format is required to ensure that the columns are appropriately matched and stacked. The function also
  accepts column names of the reverse order with 'outcome, predictor', so long as there isn't any ambiguity.
  In the absense of Q/Displayr meta data, the identification split is attempted via an assumed single comma separator
  }
  Also, when using Q/Displayr, some columns in the \code{data.frame} for the  \code{unstacked.data} argument will contain
  data reductions or NETs based off the codeframe and assigned codes to each NET. During the stacking process a NET is
  removed from analysis unless it is entirely comprised of codes that are not observed elsewhere in the \code{data.frame}
}
\references{
Davison, A. C. and Snell, E. J. (1991) Residuals and diagnostics. In: Statistical Theory and Modelling.
  In Honour of Sir David Cox, FRS, eds. Hinkley, D. V., Reid, N. and Snell, E. J., Chapman & Hall.

  Greenwell, B., McCarthy, A. and Boehmke, B. (2017). sure: Surrogate Residuals for Ordinal and General
  Regression Models. R package version 0.2.0. https://CRAN.R-project.org/package=sure

  Gromping, U. (2007). "Estimators of Relative Importance in Linear
  Regression Based on Variance Decomposition", The American Statistician,
  61, 139-147.

  von Hippel, Paul T. 2007. "Regression With Missing Y's: An
  Improved Strategy for Analyzing Multiply Imputed Data." Sociological
  Methodology 37:83-117.

  Johnson, J.W. (2000). "A Heuristic Method for Estimating the Relative Weight",
  Multivariate Behavioral Research, 35:1-19.

  Long, J. S. and Ervin, L. H. (2000). Using heteroscedasticity consistent
  standard errors in the linear regression  model. The American Statistician, 54(3): 217-224.

  Lui, D. and Zhang, H. (2018). Residuals and Diagnostics for Ordinal Regression Models: A Surrogate Approach.
  Journal of the American Statistical Association, 113:522, 845-854.

  Lumley, T. (2004) Analysis of complex survey samples. Journal of Statistical Software 9(1): 1-19

  White, H. (1980), A heteroskedastic-consistent  covariance matrix estimator
  and a direct test of heteroskedasticity. Econometrica, 48, 817-838.
}
