context("variables")

library(flipData)
y <- factor(rep(1:2, 5))
x <- runif(10)
z <- Regression(y ~ x, type = "Binary Logit")

test_that("Probabilities",{
    expect_equal(Observed(z), y)
})


test_that("Probabilities",{
    expect_equal(sum(Probabilities(z)[, 1]), as.numeric(table(y)[2]))

    pp <- Probabilities(z, data.frame(x=(1:5)/10))
    expect_equal(dim(pp), c(5, 2))
    expect_equal(rowSums(pp), rep(1, 5), check.attributes = FALSE)
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
