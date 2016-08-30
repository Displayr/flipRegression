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

attach(bank)
z$q <- Fees
Regression(Overall ~ z$q + Phone, details = FALSE, show.labels = TRUE)
detach(bank)

for (type in c("Linear", "Poisson", "Quasi-Poisson","Binary Logit"))
        test_that(paste("allEffects :", type),
          {
              zw = Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = type, subset = sb, weights = wgt)
              expect_error(plot(effects::allEffects(zw)), NA)
              z = Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = type, subset = sb)
              expect_error(plot(effects::allEffects(z)), NA)
          })

zw = Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = type, subset = sb, weights = wgt)


for (type in c("Linear", "Poisson", "Quasi-Poisson","Binary Logit"))
        test_that(paste("allEffects :", type),
          {
              zw = Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = type, subset = sb, weights = wgt)
              expect_error(plot(effects::allEffects(zw)), NA)
              z = Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = type, subset = sb)
              expect_error(plot(effects::allEffects(z)), NA)
          })


test_that("Variable names to labels",
{

    # Variable names
    z <- Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Linear", subset = sb, weights = wgt, detail = FALSE, show.labels = FALSE)
    expect_equal(rownames(z$summary$coefficients)[5], "fBranch2")
    expect_equal(rownames(z$summary$coefficients)[2], "Fees")
    z <- Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Binary Logit", subset = sb, weights = wgt, detail = FALSE, show.labels = FALSE)
    expect_equal(rownames(z$summary$coefficients)[5], "fBranch2")
    expect_equal(rownames(z$summary$coefficients)[2], "Fees")

    # Variable labels
    z <- Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Linear", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE)
    expect_equal(rownames(z$summary$coefficients)[2], "Fees paid")
    expect_equal(rownames(z$summary$coefficients)[5], "Branch as a factor: 2")
    z <- Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Binary Logit", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE)
    expect_equal(rownames(z$summary$coefficients)[2], "Fees paid")
    expect_equal(rownames(z$summary$coefficients)[5], "Branch as a factor: 2")
    z <- Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Multinomial Logit", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE)
    expect_equal(colnames(z$summary$coefficients)[2], "Fees paid")
    expect_equal(colnames(z$summary$coefficients)[5], "Branch as a factor: 2")
    z <- Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Ordered Logit", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE)
    expect_equal(rownames(z$summary$coefficients)[1], "Fees paid")
    expect_equal(rownames(z$summary$coefficients)[4], "Branch as a factor: 2")
    z <- Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Poisson", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE)
    expect_equal(rownames(z$summary$coefficients)[2], "Fees paid")
    expect_equal(rownames(z$summary$coefficients)[5], "Branch as a factor: 2")

    # Small binary logit
    data(colas, package = "flipExampleData")
    attr(colas$Q2, "label") <- "Gender"
    attr(colas$Q3, "label") <- "Age of person"
    z <- Regression(Q3 ~ Q2, data = colas, type = "Binary Logit", detail = FALSE, show.labels = TRUE)
    expect_equal(rownames(z$summary$coefficients)[2], "Gender: Female")

    # Multiple imputation
    z <- Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Linear", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE, missing = "Multiple imputation")
    expect_equal(rownames(z$summary$coefficients)[2], "Fees paid")
    expect_equal(rownames(z$summary$coefficients)[5], "Branch as a factor: 2")
    Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Multinomial Logit", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE, missing = "Multiple imputation")
})
