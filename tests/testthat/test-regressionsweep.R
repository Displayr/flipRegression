context("Sweep regression (pairwise correlations")
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
#
#
# z = LinearRegressionFromCorrelations(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)
# z = LinearRegressionFromCorrelations(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = wgt, data = bank, subset = sb)
# summary(z)

missing <- "Use partial data (pairwise correlations)"
test_that("Use partial data (pairwise correlations)",
          {
              z <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank,  missing = missing)
              expect_equal(0.482560, z$original$original$R2[1, 1], tolerance=1e-5)
              expect_equal(51.914326, z$original$original$F[1, 1], tolerance=1e-5)
              expect_equal(0.370859, as.numeric(z$original$coef[2]), tolerance=1e-5)
              expect_equal(334.000000, z$original$original$df[2], tolerance=1e-5)
              expect_equal(0.042810, summary(z$original)$coef[2,2], tolerance=1e-5)
          })

test_that("Use partial data (pairwise correlations) - filtered",
          {
              z <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing, subset = bank$weight > 1, detail = FALSE)
              expect_equal(0.429566, z$original$original$R2[1, 1], tolerance=1e-5)
              expect_equal(13.304, z$original$original$F[1,1], tolerance=1e-5)
              expect_equal(0.298166, as.numeric(z$original$coef[2]), tolerance=1e-5)
              expect_equal(106, z$original$original$df[2], tolerance=1e-5)
              expect_equal(0.077491, summary(z$original)$coef[2,2], tolerance=1e-5)
          })


test_that("Use partial data (pairwise correlations) - weighted",
          {
              z <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing, weights = bank$weight)
              expect_equal(0.458160, z$original$original$R2[1, 1], tolerance=1e-5)
              expect_equal(43.744, z$original$original$F[1,1], tolerance=1e-5)
              expect_equal(0.360394, as.numeric(z$original$coef[2]), tolerance=1e-5)
              expect_equal(310.400635, z$original$original$df[2], tolerance=1e-5)
              expect_equal(0.044873, summary(z$original)$coef[2,2], tolerance=1e-5)
          })


test_that("Use partial data (pairwise correlations) - popoulation weighted and filtered",
          {
              z <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing, weights = bank$ID, subset = bank$weight > 1)
              expect_equal(0.414488, z$original$original$R2[1, 1], tolerance=1e-5)
              expect_equal(2414.432, z$original$original$F[1, 1], tolerance=1e-5)
              expect_equal(0.302402, as.numeric(z$original$coef[2]), tolerance=1e-5)
              expect_equal(20464.000000, z$original$original$df[2], tolerance=1e-5)
              expect_equal(0.005474, summary(z$original)$coef[2, 2], tolerance=1e-5)
          })


