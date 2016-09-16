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
              ExpectWarning(Regression(y ~ x, robust.se = FALSE), "Breusch")
              ExpectNoWarning(Regression(y ~ x, robust.se = TRUE), "Breusch")

          })

test_that("Outliers",
          {
              set.seed(133452)
              y  <- 1:10 + rnorm(10, .1)
              x <- 1:10
              ExpectNoWarning(Regression(y ~ x), "Unusual observations")
              x <- c(10, 1:9)
              ExpectWarning(Regression(y ~ x), "Unusual observations")
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


for (type in c("Linear", "Poisson", "Quasi-Poisson","Binary Logit",  "NBD", "Multinomial Logit", "Ordered Logit"))
    test_that(paste("Categories", type),
          {
              set.seed(213)
              y  <- 1:100
              x <- rnorm(100, y, y)
              if (type == "Linear")
                  ExpectWarning(Regression(y ~ x, type = type), "appears to contain categories")
              else
                  ExpectNoWarning(Regression(y ~ x, type = type), "appears to contain categories")
              set.seed(213)
              y  <- 101:200
              x <- rnorm(100, y, y)
              if (type == "Linear")
                  ExpectWarning(Regression(y ~ x, type = type), "appears to contain categories")
              else
                  ExpectNoWarning(Regression(y ~ x, type = type), "appears to contain categories")
          })

