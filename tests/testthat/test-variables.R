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
