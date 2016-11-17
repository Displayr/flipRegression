
data(bank, package = "flipExampleData")
zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 100"
wgt <- bank$ID
attr(wgt, "label") <- "ID"



test_that("DS-884 MNL with 2 category dependent variable")
{

    bank$o2 <- factor(unclass(bank$Overall) > 3)
    type = "Multinomial Logit"
    for(missing in c("Multiple imputation", "Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
            test_that(paste("Type by residual", missing, type),
          {
              # no weight, no filter
              z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = NULL, type = type))
              z = suppressWarnings(Regression(o2 ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = NULL, type = type))
              # weight
              expect_error(suppressWarnings(Regression(o2 ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = NULL, type = type)), NA)
              # weight, filter
              expect_error(suppressWarnings(Regression(o2 ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = wgt, type = type)), NA)
              # weight, filter
              expect_error(z <- suppressWarnings(Regression(o2 ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = wgt, type = type)), NA)
              expect_error(capture.output(suppressWarnings(print(z))),NA)
          })

}
