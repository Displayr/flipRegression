context("warnings")

library(flipU)

test_that("Predictor is outcome",
          {
              y  <- 1:100 + .001
              x <- rnorm(100, y, y)

              expect_error(Regression(y ~ y, robust.se = FALSE))
              ExpectNoWarning(Regression(y ~ x, robust.se = TRUE), "Outcome' variable has been s")

          })
test_that("Heteroskedasticity",
          {
              y  <- 1:100 + .001
              x <- rnorm(100, y, y)
              out <- Regression(y ~ x, robust.se = FALSE)
              ExpectWarning(out, "Breusch")
              ExpectNoWarning(Regression(y ~ x, robust.se = TRUE), "Breusch")
          })

test_that("Outliers",
          {
              set.seed(133452)
              y  <- 1:10 + rnorm(10, .1)
              x <- 1:10
              out <- Regression(y ~ x)
              ExpectNoWarning(out, "Unusual observations")
              x <- c(10, 1:9)
              out <- Regression(y ~ x)
              ExpectWarning(out, "Unusual observations")
          })


test_that("Dichotomized",
          {
              set.seed(12)
              y  <- 1:10 + rnorm(10, .1)
              x <- 1:10
              ExpectWarning(Regression(y ~ x, type = "Binary Logit"), "dichotimized")
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
                ExpectWarning(Regression(y ~ x), "the data is missing")

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
