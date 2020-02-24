context("warnings")

library(flipU)

test_that("Predictor is outcome",
          {
              y  <- 1:100 + .001
              x <- rnorm(100, y, y)

              expect_error(Regression(y ~ y, robust.se = FALSE), "A variable may only appear once")
              ExpectNoWarning(Regression(y ~ x, robust.se = TRUE), "Outcome' variable has been s")

          })
test_that("Heteroskedasticity",
          {
              y  <- 1:100 + .001
              x <- rnorm(100, y, y)
              expect_error(out <- Regression(y ~ x, robust.se = FALSE), NA)
              expect_warning(print(out), "Breusch")
              ExpectNoWarning(Regression(y ~ x, robust.se = TRUE), "Breusch")
          })

test_that("Outliers",
          {
              set.seed(133452)
              y  <- 1:10 + rnorm(10, .1)
              x <- 1:10
              expect_error(out <- Regression(y ~ x), NA)
              ExpectNoWarning(out, "Unusual observations")
              x <- c(10, 1:9)
              expect_error(out <- Regression(y ~ x), NA)
              ExpectWarning(out, "Unusual observations")
          })

test_that("DS-2704: Check user prompts for automated outlier detection in unusual observation case",
          {
              set.seed(133452)
              y  <- 1:10 + rnorm(10, .1)
              x <- 1:10
              x <- c(10, 1:9)
              expect_error(out <- Regression(y ~ x), NA)
              expect_warning(print(out),
                             "Unusual observations detected. Consider re-running the analysis using automated outlier removal")
              expect_error(out <- Regression(y ~ x, outlier.prop.to.remove = 0.1), NA)
              expect_warning(print(out), NA)
              x <- c(0, x)
              y <- c(10, y)
              expect_error(out <- Regression(y ~ x, outlier.prop.to.remove = 0.1), NA)
              expect_warning(print(out),
                             paste0("Unusual observations detected. After removing a proportion of the data from the ",
                                    "analysis, unusual observations exist in the"))
          })



test_that("Dichotomized",
          {
              set.seed(12)
              y  <- 1:10 + rnorm(10, .1)
              x <- 1:10
              expect_warning(Regression(y ~ x, type = "Binary Logit"), "dichotimized")
              y <- factor(round(y / 15) + 1)
              ExpectNoWarning(Regression(y ~ x, type = "Binary Logit"), "dichotimized")
          })

test_that("Missing",
          {
                y  <- 1:50 + rnorm(50, .1)
                x <- 1:50
                ExpectNoWarning(Regression(y ~ x), "the data is missing")
                y  <- c(rep(NA, 500), 1:50 + rnorm(50, .1))
                x <- 1:550
                expect_warning(Regression(y ~ x), "the data is missing")

          })


for (type in c("Linear", "Poisson", "Quasi-Poisson", "Binary Logit", "NBD", "Multinomial Logit", "Ordered Logit"))
    test_that(paste("Categories", type),
          {
              set.seed(213)
              y  <- 1:100
              x <- rnorm(100, y, y)
              warn <- switch(type, "Ordered Logit" = "NaNs produced",
                             "Binary Logit" = "y has been dichotimized", NA)
              expect_warning(out <- Regression(y ~ x, type = type), warn)
              if (type == "Linear")
                  ExpectWarning(out, "appears to contain categories")
              else
                  ExpectNoWarning(out, "appears to contain categories")
              set.seed(213)
              y  <- 101:200
              x <- rnorm(100, y, y)
              expect_warning(out <- Regression(y ~ x, type = type), warn)
              if (type == "Linear")
                  ExpectWarning(out, "appears to contain categories")
              else
                  ExpectNoWarning(out, "appears to contain categories")
          })


test_that("Removed aliased predictors",
          {
              x  <- 1:100
              y <- z <- rnorm(100)

              expect_warning(Regression(x ~ y + z),
                             "The following variable(s) are colinear with other variables and no coefficients have been estimated: z",
                             fixed = TRUE)
          })

test_that("Removed aliased predictors (ordered logit)",
          {
              x  <- 1:100
              y <- z <- rnorm(100)

              expect_warning(Regression(x ~ y + z, type = "Ordered Logit"),
                             paste0("Some variable(s) are colinear with other ",
                                    "variables and they have been removed from ",
                                    "the estimation."),
                             fixed = TRUE)
          })

test_that("VIF with dummy variables", {
    data(bank, package = "flipExampleData")
    z <- Regression(Overall ~ Fees + ATM + Branch, data = bank, missing = "Dummy variable adjustment")
    expect_warning(print(z), paste0("The Variance Inflation Factor of the coefficients are: Fees: 1.6; ",
                                    "ATM: 1.6; Branch: 6.8. At least one of the dummy variable predictors has ",
                                    "a Variance Inflation Factor larger than 4. A value of 4 or more indicates ",
                                    "the confidence interval for the coefficient is twice as wide as they would ",
                                    "be for uncorrelated predictors. A value of 10 or more indicates high ",
                                    "multicollinearity"), fixed = TRUE)
})

