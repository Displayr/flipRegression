context("warnings")


test_that("Heteroskedasticity",
          {
              y  <- 1:100
              x <- rnorm(100, y, y)
              # Show warning
              expect_warning(Regression(y ~ x, robust.se = FALSE), "Breusch")
              # No warning
              expect_warning(Regression(y ~ x, robust.se = TRUE), "Breusch")

          })
