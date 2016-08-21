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

# test_that("Outputs",{
#     library(car)
# #  #   expect_that(
# #     z = Regression(dep ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, detail  = FALSE)
# #     expect_error(residualPlots(z), NA)
# # #    expect_that(Regression(dep ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = NULL, type= "NBD"), throws_error())
# #    expect_that(Regression(dep ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = NULL, type= "Quasi-Poisson"), throws_error())
# })




for (type in c("Linear", "Poisson", "Quasi-Poisson","Binary Logit"))
    test_that(paste("residualPlots :", type),
    {
        zw = Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = type, subset = sb, weights = wgt)
        expect_error(plot(car::residualPlots(zw)), NA)
        z = Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = type, subset = sb)
        expect_error(plot(car::residualPlots(z)), NA)
    })


for (type in c("Linear", "Poisson", "Quasi-Poisson","Binary Logit"))
        test_that(paste("allEffects :", type),
          {
              zw = Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = type, subset = sb, weights = wgt)
              expect_error(plot(effects::allEffects(zw)), NA)
              z = Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = type, subset = sb)
              expect_error(plot(effects::allEffects(z)), NA)
          })


