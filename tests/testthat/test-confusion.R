context("Confusion")
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
type  = "Linear"
bank$overalldiv3 = bank$Overall / 3

test_that(paste("Confusion matrix for linear with non-integer dependent variables"),
{
    z <- suppressWarnings(Regression(overalldiv3 ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, subset = sb, weights = wgt))
    expect_error(ConfusionMatrix(z), NA)

})


for (type in c("Linear", "Poisson", "Quasi-Poisson","Binary Logit",  "NBD", "Multinomial Logit", "Ordered Logit"))
   test_that(paste("Confusion matrix :", type),
    {
        z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, subset = sb, weights = wgt))
        expect_error(ConfusionMatrix(z), NA)
})

test_that(paste("Confusion matrix different weights"),
          {
              z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = "Linear", subset = sb, weights = wgt))
              expect_warning(ConfusionMatrix(z, weights = NULL), "Model was fitted with weights but no weights have been specified.")
})
