context("Outputs")
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
bank$fBranch <- factor(bank$Branch)
attr(bank$fBranch, "label") <- "Branch as a factor"
attr(bank$Overall, "label") <- "Overall satisfaction"

for (type in c("Linear", "Poisson", "Quasi-Poisson","Binary Logit"))
        test_that(paste("allEffects :", type),
          {
              zw = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = type, subset = sb, weights = wgt))
              expect_error(plot(effects::allEffects(zw)), NA)
              z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = type, subset = sb))
              expect_error(plot(effects::allEffects(z)), NA)
          })

for (type in c("Linear", "Poisson", "Quasi-Poisson","Binary Logit"))
        test_that(paste("allEffects :", type),
          {
              zw = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = type, subset = sb, weights = wgt))
              expect_error(plot(effects::allEffects(zw)), NA)
              z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = type, subset = sb))
              expect_error(plot(effects::allEffects(z)), NA)
          })

test_that("allEffects : Labels", {
    expect_error(z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM,
                                                  data = bank, show.labels = TRUE, detail = FALSE)),
                 NA)
    expect_error(z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM,
                                                  missing = "Multiple imputation", data = bank, show.labels = TRUE,
                                                  detail = FALSE)),
                 NA)
    expect_error(flipFormat::Labels(z$model), NA)
})




