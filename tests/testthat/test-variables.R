context("variables")

library(flipData)
y <- factor(rep(1:2, 5))
x <- runif(10)
z <- Regression(y ~ x, type = "Binary Logit")
y.degenerate <- factor(rep(1L, 10L))

test_that("Probabilities",{
    expect_equal(Observed(z), y)
})


test_that("Probabilities",{
    expect_equal(sum(Probabilities(z)[, 1]), as.numeric(table(y)[2]))

    pp <- Probabilities(z, data.frame(x=(1:5)/10))
    expect_equal(dim(pp), c(5, 2))
    expect_equal(rowSums(pp), rep(1, 5), check.attributes = FALSE)
    # Expect warning at both model fitting and Probabilities requested when poor Binary Logit model used
    expect_warning(z.degenerate <- Regression(y.degenerate ~ x, type = "Binary Logit"),
                   "needs to contain two or more categories")
    expect_warning(Probabilities(z.degenerate),
                   paste0("The Outcome variable only has a single category for this Binary Logit Regression model. ",
                          "The computed probabilities here are very likely to be uninformative and the outcome ",
                          "variable of the original Binary Logit model inspected. It should have the second category ",
                          "added and the Binary Logit model recomputed. The computed probabilities are all near zero ",
                          "as it is attempting to compute the probability of observing a category that wasn't ",
                          "included in the original data"),
                   fixed = TRUE)
})

test_that("Observed with .",{
    dat <- data.frame(y = rnorm(100), z = rbinom(100, 1, .5), x = rnorm(100))
    fit <- Regression(y~., data = dat)
    expect_equal(Observed(fit), dat[["y"]])
})

data(bank, package = "flipExampleData")

test_that("na.exclude behaviour of Regression methods",
{
    fit <- suppressWarnings(
        Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                   data = bank, missing = "Exclude cases with missing data")
    )
    resid <- residuals(fit)
    resid.expect <- flipRegression:::fillInMissingRowNames(rownames(bank),
                                                           residuals(fit$original))
    expect_equal(resid, resid.expect)
    expect_equal(names(resid), rownames(bank))
    expect_length(resid, nrow(bank))

    fitted <- fitted(fit)
    fitted.expect <- flipRegression:::fillInMissingRowNames(rownames(bank),
                                                           fitted(fit$original))
    expect_equal(fitted, fitted.expect)
    expect_equal(names(fitted), rownames(bank))
    expect_length(fitted, nrow(bank))
})

test_that("labels adding back for infIndexPlot; DS-3339",
{
    expect_error(infIndexPlot(z), NA)
})

test_that("DS-4360 Estimation Data template created correctly", {
    data(bank, package = "flipExampleData")
    bank.formula <- Overall ~ Fees + Interest
    all.variable.names <- flipU::AllVariablesNames(bank.formula, bank)
    sbank <- subset(bank, select = all.variable.names) |> na.omit()
    interest.levels <- c("Very dissatisfied", "unsatisfied", "a little unsatisfied",
                         "Neutral", "A little satisfied", "Satisfied", "Very satisfied")
    sbank <- transform(sbank, Interest = factor(Interest, labels = interest.levels))
    # Helper function to test same data across all model types
    fitModelForTest <- function(type, data = sbank) {
        if (type == "Binary Logit") {
            bank.formula <- OverallBinary ~ Fees
            data <- transform(data, OverallBinary = factor(as.numeric(Overall >= 4)))
        }
        if (type == "Ordered Logit") data[["Overall"]] <- Ordered(data[["Overall"]])
        warn.msg <- if (type == "NBD") "Model may not have converged" else NA
        expect_warning(model <- Regression(formula = bank.formula,
                                           data = data,
                                           type = type),
                       warn.msg)
        model
    }
    # Function to check each regression model
    checkEstimationDataTemplate <- function(regression.model, expected.template) {
        expect_true("estimation.data.template" %in% names(regression.model))
        expect_equal(regression.model[["estimation.data.template"]], expected.template)
    }
    # Basic tests for all model types, no dummy variable adjustment
    expected.template <- list(
        Overall = list(
            type = "numeric",
            default.value = 1
        ),
        Fees = list(
            type = "numeric",
            default.value = 1
        ),
        Interest = list(
            type = "factor",
            levels = interest.levels,
            observed.levels = interest.levels,
            has.unobserved.levels = FALSE,
            ordered = FALSE,
            default.value = interest.levels[1]
        )
    )
    for (type in model.types) {
        fit <- fitModelForTest(type = type)
        checkEstimationDataTemplate(fit, expected.template)
    }
})
