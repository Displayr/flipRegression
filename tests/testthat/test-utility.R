context("Utilities")

data(bank, package = "flipExampleData")
small.bank <- na.omit(bank)
small.bank <- transform(small.bank,
                        Overall_Binary = factor(as.numeric(Overall >= 4)))
small.wts <- runif(nrow(small.bank))
bank.formula <- Overall ~ Fees
regression.types <- c("Linear", "Binary Logit", "Ordered Logit",
                      "NBD", "Poisson", "Quasi-Poisson", "Multinomial Logit")
n.types <- length(regression.types)
weights <- replicate(n.types, NULL, simplify = FALSE)
weights <- c(weights, replicate(n.types, small.wts, simplify = FALSE))
regression.types <- rep(regression.types, 2L)

createFittedModel <- function(type, weights) {
    if (type == "Binary Logit") bank.formula <- Overall_Binary ~ Fees
    if (type == "Ordered Logit")  small.bank[["Overall"]] <- Ordered(small.bank[["Overall"]])
    warn.msg <- if (type == "NBD") "Model may not have converged" else NA
    expect_warning(model <- fitModel(.formula = bank.formula,
                                     .estimation.data = small.bank,
                                     .weights = weights,
                                     type = type,
                                     robust.se = FALSE,
                                     subset = NULL),
                  warn.msg)
    model[["model"]]
}

createFakeRegression <- function(model) {
    structure(list(original = model), class = "Regression")
}

raw.models <- mapply(createFittedModel, regression.types, weights)
full.models <- lapply(raw.models, createFakeRegression)

test_that("getModelType and isWeightedModel operate correctly", {
    for (i in seq_along(regression.types)) {
        expect_equal(getModelType(raw.models[[i]]), regression.types[i])
        expect_equal(getModelType(full.models[[i]]), regression.types[i])
        expect_equal(isWeightedModel(raw.models[[i]]), !is.null(weights[[i]]))
        expect_equal(isWeightedModel(full.models[[i]]), !is.null(weights[[i]]))
    }
})

# Test arguments to Regression
test_that("DS-4096: Informative error if arguments are not valid", {
    expect_error(Regression(bank.formula,
                            small.bank, missing = "Multiple imputation",
                            outlier.prop.to.remove = 0.1,
                            type = "Linear"),
                 "Multiple imputation is not supported with automated outlier removal")
    # Check regression types
    valid.regression.types <- c("Linear", "Binary Logit", "Poisson", "Quasi-Poisson",
                                "Ordered Logit", "Multinomial Logit", "NBD")
    not.linear <- setdiff(valid.regression.types, "Linear")
    expected.error <- paste0(sQuote("type"), " should be one of ",
                             paste0(dQuote(valid.regression.types), collapse = ", "), ".")
    expect_error(throwErrorInvalidArgument("type"), expected.error)
    expect_error(Regression(bank.formula, small.bank, type = "foo"), expected.error)
    expect_error(Regression(bank.formula, small.bank, type = "linear"), expected.error)
    # Check missing value arguments
    valid.missing.args <- c("Error if missing data", "Exclude cases with missing data",
                            "Dummy variable adjustment", "Use partial data (pairwise correlations)",
                            "Imputation (replace missing values with estimates)", "Multiple imputation")
    expected.error <- paste0(sQuote("missing"), " should be one of ",
                             paste0(dQuote(valid.missing.args), collapse = ", "), ".")
    expect_error(throwErrorInvalidArgument("missing"), expected.error, fixed = TRUE)
    expect_error(Regression(bank.formula, small.bank, type = "Linear", missing = "foo"),
                 expected.error, fixed = TRUE)
    expect_error(Regression(bank.formula, small.bank, type = "Linear", missing = "multiple imp"),
                 expected.error, fixed = TRUE)
    ## Check robust.se not supported outside linear regression
    for (invalid.type in not.linear)
        expect_error(Regression(bank.formula, small.bank, type = invalid.type, robust.se = TRUE),
                     "Robust standard errors are only supported for Linear regression.")
    ## Multiple imputation not valid with outlier removal
    expect_error(Regression(bank.formula, small.bank, type = "Linear",
                            missing = "Multiple imputation",
                            outlier.prop.to.remove = 0.1),
                    "Multiple imputation is not supported with automated outlier removal")
    ## Partial data not supported in internal calls
    expect_error(Regression(bank.formula, small.bank, type = "Linear",
                            internal = TRUE, missing = "Use partial data (pairwise correlations)"),
                 "'internal' may not be selected with regressions based on correlation matrices.")
    ## Partial data not supported outside linear regression
    for (invalid.type in not.linear)
        expect_error(Regression(bank.formula, small.bank, type = invalid.type,
                                missing = "Use partial data (pairwise correlations)"),
                     "Use partial data (pairwise correlations) is only supported for Linear regression.",
                     fixed = TRUE)
    ## Robust SE not supported if using partial data or multiple imputation
    for (missing in c("Use partial data (pairwise correlations)", "Multiple imputation"))
        expect_error(Regression(bank.formula, small.bank, type = "Linear",
                                missing = missing, robust.se = TRUE),
                     "Robust standard errors cannot be computed with 'missing' set to")

})
