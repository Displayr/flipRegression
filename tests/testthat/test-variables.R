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

