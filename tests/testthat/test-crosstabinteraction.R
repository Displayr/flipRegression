context("Crosstab interaction")

data(bank, package = "flipExampleData")
test_that("Basic output", {
    zz <- suppressWarnings(Regression(Overall ~ Fees + Interest, interaction = ATM, data = bank))
    expect_equal(nrow(zz$combined.coefs), 3)
    expect_equal(ncol(zz$combined.coefs), 7)
    expect_equal(sum(is.na(zz$combined.coefs)), 1)
    expect_equal(zz$interaction.pvalue, 0.002888697)
})

all.types <- c("Binary Logit", "Poisson", "Quasi-Poisson", "NBD", "Ordered Logit", "Multinomial Logit")
test_that("Other types", {

    for (tt in all.types[-6])
        expect_error(suppressWarnings(Regression(Overall ~ Fees + Interest, interaction = ATM, data = bank, type = tt)), NA)
    expect_error(suppressWarnings(Regression(Overall ~ Fees + Interest, interaction = ATM, data = bank, type = "Multinomial Logit")))
})

#test_that("Multiple imputation", {
#    zz <- suppressWarnings(Regression(Overall ~ Fees + Interest, interaction = ATM, data = bank, missing = "Multiple imputation", seed=123))
#    expect_equal(nrow(zz$combined.coefs), 3)
#    expect_equal(ncol(zz$combined.coefs), 7)
#    expect_equal(sum(is.na(zz$combined.coefs)), 1)
#    expect_equal(zz$interaction.pvalue, 0.002152381)
#})

f4 <- (1:nrow(bank)) %% 4
test_that("Robust SE", {
    expect_warning(Regression(Overall ~ Fees + Interest, interaction = ATM, data = bank, robust.se = T), "Robust SE not used")
    expect_error(suppressWarnings(Regression(Overall ~ Fees + Interest, interaction = f4, data = bank, robust.se = T)), NA)
})

test_that("Coefficients", {

    n <- 500
    x1 <- rnorm(n)
    x2 <- rnorm(n)
    x3 <- rnorm(n)
    f3 <- round(runif(n, 5, 10))
    ee <- 0.1 * rnorm(n)

    # Only interactions to the x3 coefficient should be significant
    yy <- 1 * x1 + 1 * x2 + 1 * x3 * f3 + ee
    zz <- Regression(yy~x1+x2+x3, interaction=f3)
    expect_equal(sum(abs(zz$coef.sign[1,])), 0)
    expect_equal(sum(abs(zz$coef.sign[4,])), 5)

    # No interaction effect - but there is heteroskacity
    yh <- 3 * x1 + 5 * x2 + 1 * x3 +  f3 * ee
    zh1 <- Regression(yh~x1+x2+x3, interaction=f3)
    zh2 <- Regression(yh~x1+x2+x3, interaction=f3, robust.se=T)

})







