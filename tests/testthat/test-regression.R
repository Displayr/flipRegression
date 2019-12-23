context("Regression")

data(bank, package = "flipExampleData")
zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 100"
wgt <- bank$ID
attr(wgt, "label") <- "ID"
bank$dep <- (unclass(bank$Overall) - 1) / 6

missing <- "Exclude cases with missing data"
test_that(missing,
          {
              z <- as.numeric(suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing))$coef[3])
              expect_equal(round(z,4), round(0.27732,4))
              z <- as.numeric(suppressWarnings(Regression(zformula, data = bank, subset = sb,  missing = missing))$coef[3])
              expect_equal(round(z,4), round(0.25451,4))
              z <- as.numeric(suppressWarnings(Regression(zformula, data = bank, weights = wgt, missing = missing))$coef[3])
              expect_equal(round(z,4), round(0.2611546, 4))
              z <- as.numeric(suppressWarnings(Regression(zformula, data = bank, weights = wgt, subset = sb, missing = missing))$coef[3])
              expect_equal(round(z,4),round(0.2539403,4))
          })


missing <- "Imputation (replace missing values with estimates)"
test_that(missing,
          {
              if (.Platform$OS.type == "windows")
              {
                  z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)$coef[3])
                  expect_equal(round(z, 3), 0.316)
                  z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing)$coef[3])
                  expect_equal(round(z, 3), 0.286)
                  z <- suppressWarnings(as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, missing = missing)$coef[3]))
                  expect_equal(round(z, 3), 0.291)
                  z <- suppressWarnings(as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = sb, missing = missing)$coef[3]))
                  expect_equal(round(z, 3), 0.293)
              } else
              {
                  z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)$coef[3])
                  expect_equal(round(z, 3), 0.285)
                  z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing)$coef[3])
                  expect_equal(round(z, 3), 0.302)
                  z <- suppressWarnings(as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, missing = missing)$coef[3]))
                  expect_equal(round(z, 3), 0.304)
                  z <- suppressWarnings(as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = sb, missing = missing)$coef[3]))
                  expect_equal(round(z, 3), 0.301)
              }
          })

missing <- "Multiple imputation"
test_that(missing,
          {
              if (.Platform$OS.type == "windows")
              {
                  z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)$coef[3])
                  expect_equal(round(z, 3), 0.296)
                  z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing)$coef[3])
                  expect_equal(round(z, 3), 0.302)
                  z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, missing = missing)$coef[3])
                  expect_equal(round(z, 3), 0.309)
                  z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = sb, missing = missing)$coef[3])
                  expect_equal(round(z, 3), 0.309)
              } else
              {
                  z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)$coef[3])
                  expect_equal(round(z, 3), 0.299)
                  z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing)$coef[3])
                  expect_equal(round(z, 3), 0.305)
                  z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, missing = missing)$coef[3])
                  expect_equal(round(z, 3), 0.307)
                  z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = sb, missing = missing)$coef[3])
                  expect_equal(round(z, 3), 0.314)
              }
          })

missing <- "Multiple imputation"
test_that("DS-2645 - Entirely Missing predictor", {
    bank$Bogus <- rep(NA, nrow(bank))
    expect_warning(Regression(Overall ~ ., data = bank, missing = missing),
                   "Data has variable(s) that are entirely missing values (all observed values of the variable are missing). These variable(s) have been removed from the analysis: Bogus.", fixed = TRUE)
    bank$Nothing <- bank$Bogus
    expect_warning(Regression(Overall ~ ., data = bank, missing = missing),
                   "Data has variable(s) that are entirely missing values (all observed values of the variable are missing). These variable(s) have been removed from the analysis: Bogus, Nothing.", fixed = TRUE)
    bank$Bogus <- NULL
    expect_error(Regression(Nothing ~ ., data = bank),
                 "Response variable is entirely missing (all observed values of the variable are missing).", fixed = TRUE)
    bank$Nothing <- NULL
})

test_that("DS-2645 - Missing predictors and/or interaction", {
    bank$Bogus <- rep(NA, nrow(bank))
    expect_warning(Regression(Overall ~ Fees + Interest + ATM + Bogus, interaction = Branch, data = bank, missing = missing),
                   "^Data has variable\\(s\\) that are entirely missing values \\(all observed values of the variable are missing\\). These variable\\(s\\) have been removed from the analysis: Bogus.", perl = TRUE)
    bank$Bogus = NULL
    bank$Branch2 = rep(NA, nrow(bank))
    expect_error(Regression(Overall ~ Fees + Interest + ATM, interaction = Branch2, data = bank, missing = missing),
                 "Crosstab interaction variable must contain more than one unique value.")
    expect_error(Regression(Overall ~ Fees + Interest + ATM, interaction = Branch, data = bank, missing = missing),
                 NA)
    bank$Branch2= NULL
})

#### REDUCE DATA SIZE FOR TESTS WITHOUT NUMERICAL EQUALITY ###

data(bank, package = "flipExampleData")
bank <- bank[sample(nrow(bank), 200), ] # random sample of 200 rows to improve perfomance
zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 100"
wgt <- bank$ID
attr(wgt, "label") <- "ID"
bank$dep <- (unclass(bank$Overall) - 1) / 6

test_that(paste("Alternative ways of passing data in"),
{
      type = "Linear"
      # no weight, no filter
      z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = NULL, type = type))
      attach(bank)
      z1 = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = NULL, type = type))
      expect_true(all.equal(z$coef, z1$coef))
      detach(bank)
      # filter and weight a part of the data frame.
      zbank <- cbind(bank, w = wgt, ff = sb)
      z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = zbank, subset = ff, weights = w, type = type))
      attach(zbank)
      z1 = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = w, subset = ff, type = type))
      detach(zbank)
      expect_true(all.equal(z$coefficients, z1$coefficients))
      # filter and weight a part of the data frame and are formulas.
      zbank <- cbind(bank, w = wgt, ff = sb)
      z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = zbank, subset = ff == TRUE, weights = w, type = type))
      attach(zbank)
      z1 = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = w, subset = ff == TRUE, type = type))
      detach(zbank)
      expect_true(all.equal(z$coefficients, z1$coefficients))
      # filter and weight are not part of the data frame.
      z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, weights = wgt, type = type))
      attach(zbank)
      z1 = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, subset = sb, weights = wgt, type = type))
      detach(zbank)
      expect_true(all.equal(z$coefficients, z1$coefficients))
      # data frame referenced in formula.
      z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                                      data = bank, subset = sb, weights = wgt, type = type))
      z1 = suppressWarnings(Regression(bank$Overall ~ bank$Fees + bank$Interest + bank$Phone + bank$Branch + bank$Online + bank$ATM,
                                       subset = sb, weights = wgt, type = type))
      expect_true(all.equal(z$coefficients, z1$coefficients))
})

test_that(paste("Robust se does something"),
{
      type = "Linear"
      # no weight, no filter
      z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = TRUE,  weights = NULL, type = type))
      zs = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, robust.se = TRUE, subset = TRUE,  weights = NULL, type = type))
      expect_false(isTRUE(all.equal(z$summary$coefficients[,2], zs$summary$coefficients[,2])))
      z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, detail = FALSE, subset = TRUE,  weights = NULL, type = type))
      zs = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, detail = FALSE, robust.se = TRUE, subset = TRUE,  weights = NULL, type = type))
      expect_false(isTRUE(all.equal(z$summary$coefficients[,2], zs$summary$coefficients[,2])))
      expect_error(((z)), NA)
      expect_error(((zs)), NA)

      # type = "Poisson"
      # # no weight, no filter
      # z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = TRUE,  weights = NULL, type = type))
      # zs = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, robust.se = TRUE, subset = TRUE,  weights = NULL, type = type))
      # expect_false(isTRUE(all.equal(z$summary$coefficients[,2], zs$summary$coefficients[,2])))
      # z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, detail = FALSE, subset = TRUE,  weights = NULL, type = type))
      # zs = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, detail = FALSE, robust.se = TRUE, subset = TRUE,  weights = NULL, type = type))
      # expect_false(isTRUE(all.equal(z$summary$coefficients[,2], zs$summary$coefficients[,2])))
      # expect_error(((z)), NA)
      # expect_error(((zs)), NA)

})

type = "Multinomial Logit"
missing = "Multiple imputation"
for(missing in c("Multiple imputation", "Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
    for (type in c("Multinomial Logit", "Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered Logit", "NBD"))
        test_that(paste("Type by residual", missing, type),
      {
          # no weight, no filter
          z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = NULL, type = type))
          # weight
          expect_error(suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = NULL, type = type)), NA)
          # weight, filter
          expect_error(suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = wgt, type = type)), NA)
          # weight, filter
          expect_error(z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = wgt, type = type)), NA)
          expect_error(capture.output(suppressWarnings(print(z))),NA)
      })

test_that("allEffects works on Regression object",
{
    data(cpus, package = "MASS")
    z <- Regression(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, data = cpus)
    zlm <- lm(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, data = cpus)
    expect_equal(effects::allEffects(z), effects::allEffects(zlm), check.attributes = FALSE)
})

for(missing in c("Multiple imputation", "Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
    for (type in c("Multinomial Logit","Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered Logit", "NBD"))
        test_that(paste("Stops gracefully with small sample size", missing, type),
{
     expect_that(suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = wgt > 30000, type = type)), throws_error())
     expect_that(suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = wgt > 30000,  weights = wgt, type = type)), throws_error())
})


test_that("Error due to missing data",
{
    missing = "Error if missing data"
    expect_error(Regression(zFormula, data = bank, missing = missing), tolerance = 1.0e-8)
})


for(missing in c("Multiple imputation", "Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
    for (type in c("Multinomial Logit", "Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered Logit", "NBD"))
        for (detail in c(FALSE, TRUE))
            test_that(paste("No error", missing, type, "detail =", detail),
{
    if (type!= "Multinomial Logit" | detail)
    {
         # no weight, no filter
         expect_error(suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE, detail = detail, weights = NULL, type = type)), NA)
         # weight
         expect_error(suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb, detail = detail, weights = NULL, type = type)), NA)
         # weight, filter
         expect_error(suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE, detail = detail, weights = wgt, type = type)), NA)
         # weight, filter
         expect_error(suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = wgt, detail = detail, type = type)), NA)
    }
})


for(missing in c("Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
    for (type in c("Multinomial Logit", "Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered Logit", "NBD"))
        test_that(paste(type, " save variables"),{
            z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, missing = missing, weights = wgt / 100, subset = sb))
            expect_equal(length(predict(z)), 200)
          })


for (type in c("Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered Logit", "NBD"))
    test_that(paste(type, "does not have an error when producing non-detailed outputs"),{
            z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, missing = missing, weights = wgt / 100, subset = sb, detail = FALSE))
            expect_error(suppressWarnings(print(z)), NA)
          })

test_that("Error if too many dummy predictors",
          {
              df <- data.frame(outcome = runif(10), pred1 = as.factor(letters[1:10]), pred2 = as.factor(LETTERS[1:10]))
              expect_error(Regression(outcome ~ ., data = df),
                           "There are fewer observations.")
          })


test_that("Outcome labels", {
    attr(bank$Overall, "label") <- "lbl"
    bank$Overall <- as.factor(bank$Overall)
    result <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                                          data = bank, show.labels = TRUE))
    expect_equal(result$outcome.label, "lbl")
})
