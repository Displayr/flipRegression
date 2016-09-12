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



test_that("Labels are extracted from variables containinging $",
          {
              library(flipRegression)
              attach(bank)
              z = data.frame(q = Fees)
              zz <- rownames(Regression(Overall ~ z$q + Phone, detail = FALSE, show.labels = TRUE)$summary$coef)[2]
              expect_equal(zz, "Fees paid")
              detach(bank)
          })

test_that("PrettyRegressionTable",{

    ft <- "Yo! This footer specifically designed
          to communicate important information.
    Since it is so important, it will of course
    extend over many lines.  In fact, on narrow tables,
    it might take >3.  On wide tables, it might only
    require one.  Feel free to adjust the width,
    and the importance and significance does not
    go away."

    data(weight, package = "flipExampleData")
    z = summary(lm(Weight ~ Height + Age, data = weight))$coef
    expect_error(PrettyRegressionTable(z, TRUE, footer = ft,  title = "My awesome regression", subtitle = "Big brown dog"), NA)
    PrettyRegressionTable(z, TRUE, footer = ft,  title = "My awesome regression", subtitle = "Big brown dog")

    ## Linear regression
    suppressWarnings(Regression(Overall ~  Fees + Interest + Phone + Branch + Online  +ATM, data = bank, detail = FALSE))

    # Linear regression with robust se
    suppressWarnings(Regression(Overall ~  Fees + Interest + Phone + Branch + Online  +ATM, data = bank, robust.se = TRUE, detail = FALSE))

    # Ordered logit (has a z statistic rather than a t)
    suppressWarnings(Regression(Overall ~  Fees + Interest + Phone + Branch + Online  +ATM, data = bank, type = "Ordered Logit", detail = FALSE))

    coef.matrix <- summary(lm(Sepal.Length ~ Species * Sepal.Width, iris))$coef
    rownames(coef.matrix)[1] <- "Big dog"
    PrettyRegressionTable(coef.matrix, TRUE, footer = ft,  title = "My awesome regression", subtitle = "Big brown dog")

    expect_error(PrettyRegressionTable(coef.matrix, TRUE, footer = ft,  title = "My awesome regression", subtitle = "Big brown dog"), NA)
})


for (type in c("Linear", "Poisson", "Quasi-Poisson","Binary Logit"))
        test_that(paste("allEffects :", type),
          {
              zw = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = type, subset = sb, weights = wgt))
              expect_error(plot(effects::allEffects(zw)), NA)
              z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = type, subset = sb))
              expect_error(plot(effects::allEffects(z)), NA)
          })

zw = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = type, subset = sb, weights = wgt))


for (type in c("Linear", "Poisson", "Quasi-Poisson","Binary Logit"))
        test_that(paste("allEffects :", type),
          {
              zw = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = type, subset = sb, weights = wgt))
              expect_error(plot(effects::allEffects(zw)), NA)
              z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = type, subset = sb))
              expect_error(plot(effects::allEffects(z)), NA)
          })


test_that("Variable names to labels",
{

    # Variable names
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Linear", subset = sb, weights = wgt, detail = FALSE, show.labels = FALSE))
    expect_equal(rownames(z$summary$coefficients)[5], "fBranch2")
    expect_equal(rownames(z$summary$coefficients)[2], "Fees")
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Binary Logit", subset = sb, weights = wgt, detail = FALSE, show.labels = FALSE))
    expect_equal(rownames(z$summary$coefficients)[5], "fBranch2")
    expect_equal(rownames(z$summary$coefficients)[2], "Fees")

    # Variable labels
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Linear", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE))
    expect_equal(rownames(z$summary$coefficients)[2], "Fees paid")
    expect_equal(rownames(z$summary$coefficients)[5], "Branch as a factor: 2")
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Binary Logit", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE))
    expect_equal(rownames(z$summary$coefficients)[2], "Fees paid")
    expect_equal(rownames(z$summary$coefficients)[5], "Branch as a factor: 2")
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Multinomial Logit", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE))
    expect_equal(colnames(z$summary$coefficients)[2], "Fees paid")
    expect_equal(colnames(z$summary$coefficients)[5], "Branch as a factor: 2")
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Ordered Logit", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE))
    expect_equal(rownames(z$summary$coefficients)[1], "Fees paid")
    expect_equal(rownames(z$summary$coefficients)[4], "Branch as a factor: 2")
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Poisson", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE))
    expect_equal(rownames(z$summary$coefficients)[2], "Fees paid")
    expect_equal(rownames(z$summary$coefficients)[5], "Branch as a factor: 2")

    # Small binary logit
    data(cola, package = "flipExampleData")
    attr(cola$Q2, "label") <- "Gender"
    attr(cola$Q3, "label") <- "Age of person"
    z <- suppressWarnings(Regression(Q3 ~ Q2, data = cola, type = "Binary Logit", detail = FALSE, show.labels = TRUE))
    expect_equal(rownames(z$summary$coefficients)[2], "Gender: Female")

    # Multiple imputation
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Linear", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE, missing = "Multiple imputation"))
    expect_equal(rownames(z$summary$coefficients)[2], "Fees paid")
    expect_equal(rownames(z$summary$coefficients)[5], "Branch as a factor: 2")
})





















