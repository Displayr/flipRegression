context("Regression Diagnostics")
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


test_that(paste("Grand mean"),
{
    type  = "Linear"
    z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank[, c("Overall", "Fees", "Interest","Phone", "Branch", "Online", "ATM")], type = type, subset = sb, weights = wgt))
    subset <- sb & wgt > 0 & !is.na(sb) & !is.na(wgt)
    subset <- subset & complete.cases(cbind(sb, wgt, bank[, c("Overall", "Fees", "Interest","Phone", "Branch", "Online", "ATM")]))
    y <- bank$Overall[subset]
    w <- wgt[subset]
    mn <- sum(y * w) / sum(w)
    expect_equal(mn, GrandMean(z))
})

for (type in c("Linear", "Poisson", "Quasi-Poisson", "Binary Logit", "NBD", "Multinomial Logit", "Ordered Logit"))
    test_that(paste("extractAIC :", type),
    {
        z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, subset = sb, weights = wgt))
        expect_error(extractAIC(z), NA)
})


for (type in c("Linear", "Poisson", "Quasi-Poisson","Binary Logit",  "NBD"))#, "Multinomial Logit")) #"Ordered Logit",
    test_that(paste("Testing outliers:", type),
    {
        z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, subset = sb, weights = wgt))
        expect_error(outlierTest(z), NA)
        expect_error(capture.output(HatValues(z)), NA)
        expect_error(capture.output(CooksDistance(z)), NA)

})

#
# oldcon <- options(contrasts = c("contr.treatment", "contr.poly"))
# ## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
# ## Page 9: Plant Weight Data.
# ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
# trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
# group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
# weight <- c(ctl, trt)
# wgt = runif(length(group))
# lm.D9 <- Regression(weight ~ group, weight = wgt)
# lm.D9
# summary(lm.D90 <- update(lm.D9, . ~ . - 1))
# update(lm.D9)
# getCall(lm.D90)  # "through the origin"
#
#
# update(lm.D9, NULL, method = "model.frame")


for (type in c("Linear", "Linear","Poisson", "Quasi-Poisson", "Binary Logit", "NBD"))
    test_that(paste("Cooks distance works:",type),
              {
                  missing = "Exclude cases with missing data"
                  z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, detail = FALSE, data = bank, weights = wgt, type = type))
                  expect_error((cooks.distance(z)), NA)
              })

for (type in c("Poisson", "Quasi-Poisson","Binary Logit", "Ordered Logit", "NBD", "Multinomial Logit"))
    test_that(paste("Goodness of fit statistics are well behaved:", type),
    {
        z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type))
        expect_true(McFaddensRhoSquared(z) > 0.2)
        expect_true(GoodnessOfFit(z)$value > 0.2)
        expect_error(AIC(z), NA)
        z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, weights = wgt))
        expect_true(McFaddensRhoSquared(z) > 0.2)
        expect_true(GoodnessOfFit(z)$value > 0.2)
        expect_error(AIC(z), NA)
})


test_that("Durbin Watson",
{
    # Negative auto correlation
    set.seed(1)
    x = runif(100) * 2
    y = x + rep(1:2,50)
    dat = data.frame(x, y)
    reg = suppressWarnings(Regression(y ~ x, data = dat, type = "Binary Logit"))
    z = suppressWarnings(DurbinWatson(reg))
    expect_equal(z$statistic, c(d = 2.62), tolerance = 1.0e-3)
    expect_equal(z$p, 0, tolerance = 1.0e-5)
    reg = suppressWarnings(Regression(y ~ x, data = dat))
    z = DurbinWatson(reg)
    expect_equal(z$statistic, c(d = 3.94), tolerance = 1.0e-3)
    expect_equal(z$p, 0, tolerance = 1.0e-3)
    # Positive autocorrelation
    y = x + c(rep(0,50),rep(1,50))
    dat = data.frame(x, y)
    reg = suppressWarnings(Regression(y ~ x, data = dat, type = "Binary Logit"))
    z = DurbinWatson(suppressWarnings(reg))
    expect_equal(z$statistic, c(d = 1.49), tolerance = 1.0e-2)
    expect_equal(z$p, 0.004, tolerance = 1.0e-2)
    reg = Regression(y ~ x, data = dat)
    z = DurbinWatson(reg)
    expect_equal(z$statistic, c(d = 0.0444), tolerance = 1.0e-3)
    expect_equal(z$p, 0, tolerance = 1.0e-3)
    # Random (no autocorrelation)
    y = x + rnorm(100)
    dat = data.frame(x, y)
    reg = suppressWarnings(Regression(y ~ x, data = dat, type = "Binary Logit"))
    z = DurbinWatson(reg)
    expect_equal(z$statistic, c(d = 2.06), tolerance = 1.0e-2)
    expect_equal(z$p, 0.712, tolerance = 1.0e-3)
    reg = (Regression(y ~ x, data = dat))
    z = DurbinWatson(reg)
    expect_equal(z$statistic, c(d = 2.08), tolerance = 1.0e-3)
    expect_equal(z$p, 0.646, tolerance = 1.0e-3)
    # Comparing with car package
    z = car::durbinWatsonTest(lm(y ~ x, data = dat), simulate = TRUE, reps = 100000, alternative = "two.sided")
    reg = suppressWarnings(Regression(y ~ x, data = dat))
    z1 <- DurbinWatson(reg, n.permutations = 100000)
    expect_equal(as.numeric(z$dw), as.numeric(z1$statistic), tolerance = 1.0e-3)
    expect_equal(z$p, z1$p, tolerance = 1.0e-2)
    missing <- missing <- "Exclude cases with missing data"
    for (type in c( "Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered Logit", "NBD"))
    {
        z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, detail = FALSE, data = bank, type = type))
        expect_error(DurbinWatson(z), NA)
    }
})

for (type in c("Linear", "Linear","Poisson", "Quasi-Poisson", "Binary Logit", "NBD"))
    test_that(paste("Cooks distance works:",type),
    {
        missing = "Exclude cases with missing data"
        z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, detail = FALSE, data = bank, type = type))
        expect_error((cooks.distance(z)), NA)
    })


for (type in c("Ordered Logit",  "Multinomial Logit"))
    test_that(paste("Cooks distance does not works:",type),
    {
        missing = "Exclude cases with missing data"
        z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, detail = FALSE, data = bank, type = type))
        expect_that(cooks.distance(z), throws_error())
    })

missing = "Multiple imputation"
type = "Ordered Logit"
for(missing in c("Multiple imputation", "Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
    for (type in c( "Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered Logit", "NBD", "Multinomial Logit"))
        test_that(paste("Accuracy and R-square:",missing, type),
        {
            z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, type = type))
            expect_true(Accuracy(z) > 0.2)
            expect_true(GoodnessOfFit(z)$value > 0.17)
            z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  type = type))
            expect_true(Accuracy(z) > 0.2)
            expect_true(GoodnessOfFit(z)$value > 0.17)
            z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = wgt, missing = missing, data = bank, type = type))
            expect_true(Accuracy(z) > 0.2)
            expect_true(GoodnessOfFit(z)$value > 0.17)
            z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = wgt, missing = missing, data = bank, subset = sb,  type = type))
            expect_true(Accuracy(z) > 0.2)
            expect_true(GoodnessOfFit(z)$value > 0.17)
        })


test_that("Tests of non-constant variance (Breush-Pagen test)",
{
    # Unfilitered
    z = ncvTest(suppressWarnings(Regression(zformula, data = bank)))
    z1 = car::ncvTest(lm(zformula, data = bank))
    expect_equal(z$p, z1$p, tolerance = 1.0e-8)

    # Filitered
    z = ncvTest(suppressWarnings(Regression(zformula, data = bank, subset = sb)))
    z1 = car::ncvTest(lm(zformula, data = bank, subset = sb))
    expect_equal(z$p, z1$p, tolerance = 1.0e-8)

    # Weighted.
    expect_that(ncvTest(suppressWarnings(Regression(zformula, data = bank,  weights = wgt))), throws_error())

    # Weighted and filtered
    expect_that(ncvTest(suppressWarnings(Regression(zformula, data = bank, subset = sb,  weights = wgt))), throws_error())

    # Unweighted
    zRegression <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank))
    zWLS <- lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)
    expect_equal(ncvTest(zRegression)$p, ncvTest(zWLS)$p)
    # Filtered
    zRegression <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100))
    zWLS <- lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100)
    expect_equal(ncvTest(zRegression)$p, ncvTest(zWLS)$p)
    # Weighted.
    expect_that(ncvTest(suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, weights = bank$ID))), throws_error())

})

test_that("VIF",
           {
     library(car)
     # Unweighted - linear
     zRegression <- vif(suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)))
     zR <- vif(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank))
     expect_equal(zRegression, zR)
     # Filtered - linear
     zRegression <- vif(suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100)))
     zR <- vif(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100))
     expect_equal(zRegression, zR)
     # Weighted.
     expect_error(vif(suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, weights = bank$ID))), NA)
     # Logit (used as a proxy for all the glms)
     type = "Binary Logit"
     zRegression <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type))
     zR <- glm(Overall >= 4 ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank,  family = binomial(link = "logit"))
     expect_equal(vif(zRegression), vif(zR))
     # Logit - filtered
     zRegression <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, subset = bank$ID > 100))
     zR <- glm(Overall >= 4 ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank,  family = binomial(link = "logit"), subset = bank$ID > 100)
     expect_equal(vif(zRegression), vif(zR))
     # Logit - filtered and weighted
     z = wgt > 0 & complete.cases(bank[,c("Overall","Fees","Interest","Phone","Branch","Online","ATM")])
     zBank = bank[z, ]
     zBank$dd = zBank$Overall >= 4
     zwgt = wgt[z]
     zRegression <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = zBank, weights = zwgt, type = type))
     zR <- survey::svyglm(dd ~ Fees + Interest + Phone + Branch + Online + ATM, data = zBank, design = flipData::WeightedSurveyDesign(zBank, zwgt), family = quasibinomial())
     expect_equal(vif(zRegression), vif(zR))
     # Checking for no errors
     for (type in c("Poisson","NBD", "Quasi-Poisson"))
        expect_error(capture.output(vif(suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, subset = bank$ID > 100, weights = bank$ID)))), NA)
     # Checking for errors in other types of models
     for (type in c( "Ordered Logit",  "Multinomial Logit"))
        expect_that(capture.output(vif(suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, subset = bank$ID > 100, weights = bank$ID)))), (throws_error()))
 })
