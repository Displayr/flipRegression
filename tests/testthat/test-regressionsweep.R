context("Sweep regression (pairwise correlations)")
data(bank, package = "flipExampleData")
zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 100"
wgt <- bank$ID
attr(wgt, "label") <- "ID"
bank$dep <- (unclass(bank$Overall) - 1) / 6
attr(bank$dep, "label") <- "Overall satisfaction"
attr(bank$Fees, "label") <- "Fees paid"
attr(bank$Online, "label") <- "Online banking"

test_that("Check for NAs in correlation matrix",
          {
              df <- data.frame(y = 1:10, x = c(1:5, NA, NA, NA, NA, NA),
                               z = c(NA, NA, NA, NA, NA, 3, 4, 7, 12, 8), j = 2)
              expect_error(suppressWarnings(Regression(y ~ x + z + j, data = df,
                                                       missing = "Use partial data (pairwise correlations)")))
              expect_error(suppressWarnings(Regression(y ~ x + z, data = df,
                                                       missing = "Use partial data (pairwise correlations)")))
              Regression(y ~ j, data = df,
                         missing = "Use partial data (pairwise correlations)") |>
                  expect_warning("Only a single predictor variable with non-missing values has been provided for analysis")
          })



missing <- "Use partial data (pairwise correlations)"

test_that("Use partial data (pairwise correlations)",
          {
              z <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                              data = bank,  missing = missing)
              expect_equal(0.482560, unname(z$original$original$R2), tolerance=1e-5)
              expect_equal(51.914326, unname(z$original$original$F), tolerance=1e-5)
              expect_equal(0.370859, as.numeric(z$original$coef[2]), tolerance=1e-5)
              expect_equal(334.000000, z$original$original$df[2], tolerance=1e-5)
              expect_equal(0.042810, summary(z$original)$coef[2,2], tolerance=1e-5)
    # Prevent pops
    mockery::stub(print.Regression, "print.htmlwidget", NULL)
              expect_warning(print(z), "The outcome variable appears to contain categories")
          })

test_that("Use partial data (pairwise correlations) - filtered",
          {
              z <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                              data = bank, missing = missing, subset = bank$weight > 1,
                              detail = FALSE)
              expect_equal(0.429566, unname(z$original$original$R2), tolerance=1e-5)
              expect_equal(13.304, unname(z$original$original$F), tolerance=1e-5)
              expect_equal(0.298166, as.numeric(z$original$coef[2]), tolerance=1e-5)
              expect_equal(106, z$original$original$df[2], tolerance=1e-5)
              expect_equal(0.077491, summary(z$original)$coef[2,2], tolerance=1e-5)
    # Prevent pops
    mockery::stub(print.Regression, "print.htmlwidget", NULL)
              expect_warning(print(z), "The outcome variable appears to contain categories")
          })


test_that("Use partial data (pairwise correlations) - weighted",
          {
              expect_warning(z <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                              data = bank, missing = missing, weights = bank$weight), "MCAR")
              expect_equal(0.458160, unname(z$original$original$R2), tolerance=1e-5)
              expect_equal(41.17282, unname(z$original$original$F), tolerance=1e-5)
              expect_equal(0.3603956, as.numeric(z$original$coef[2]), tolerance=1e-5)
              expect_equal(292.1565, z$original$original$df[2], tolerance=1e-5)
              expect_equal(0.04625331, summary(z$original)$coef[2,2], tolerance=1e-5)
    # Prevent pops
    mockery::stub(print.Regression, "print.htmlwidget", NULL)
              expect_warning(print(z), "The outcome variable appears to contain categories")
          })


test_that("Use partial data (pairwise correlations) - popoulation weighted and filtered",
{
    wght <- bank$ID
    sub <- bank$weight > 1
    bank$ID <- bank$weight <- bank$dep <- NULL
    expect_warning(z <- Regression(Overall ~ .,
                    data = bank, missing = missing, weights = wght,
                    subset = sub), "MCAR")
    expect_equal(0.414488, unname(z$original$original$R2), tolerance=1e-5)
    expect_equal(8.858319, unname(z$original$original$F), tolerance=1e-5)
    expect_equal(0.3024816, as.numeric(z$original$coef[2]), tolerance=1e-5)
    expect_equal(75.08045, z$original$original$df[2], tolerance=1e-5)
    expect_equal(0.09039705, summary(z$original)$coef[2, 2], tolerance=1e-5)
    # Prevent pops
    mockery::stub(print.Regression, "print.htmlwidget", NULL)
    expect_warning(print(z), "The outcome variable appears to contain categories")
})
