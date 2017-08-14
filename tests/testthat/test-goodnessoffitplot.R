context("Regression GoodnessOfFitPlot")
zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
data(bank, package = "flipExampleData")
sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 100"
wgt <- bank$ID
wgt[is.na(wgt)] = 0
attr(wgt, "label") <- "ID"
attr(bank$Overall, "label") <- "Overall satisfaction"
attr(bank$Fees, "label") <- "Fees paid"
attr(bank$Online, "label") <- "Online banking"


for(missing in c("Multiple imputation", "Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
    for (type in c( "Linear","Poisson", "Quasi-Poisson", "Binary Logit", "Ordered Logit", "NBD", "Multinomial Logit"))
        test_that(paste("GoodnessOfFitPlot:", missing, type),
                  {
                      z = suppressWarnings(z <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, type = type))
                      expect_warning(GoodnessOfFitPlot(z))
                      z = suppressWarnings(z <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  type = type))
                      expect_warning(GoodnessOfFitPlot(z))
                      z = suppressWarnings(z <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = wgt, missing = missing, data = bank, type = type))
                      expect_warning(GoodnessOfFitPlot(z))
                      z = suppressWarnings(z <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = wgt, missing = missing, data = bank, subset = sb,  type = type))
                      expect_warning(GoodnessOfFitPlot(z))
                  })
