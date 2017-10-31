context("variables")

library(flipData)
y <- factor(rep(1:2, 5))
x <- runif(10)
z <- Regression(y ~ x, type = "Binary Logit")

test_that("Probabilities",{
    expect_equal(Observed(z), y)
})


test_that("Probabilities",{
    expect_equal(sum(Probabilities(z)), as.numeric(table(y)[2]))
})

test_that("Observed with .",{
    dat <- data.frame(y = rnorm(100), z = rbinom(100, 1, .5), x = rnorm(100))
    fit <- Regression(y~., data = dat)
    expect_equal(Observed(fit), dat[["y"]])
})
