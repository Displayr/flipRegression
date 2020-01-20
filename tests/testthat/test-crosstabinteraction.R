context("Crosstab interaction")

data(bank, package = "flipExampleData")


test_that("Basic output", {
    zz <- suppressWarnings(Regression(Overall ~ Fees + Interest, interaction = ATM, data = bank))
    expect_equal(nrow(zz$interaction$coefficients), 3)
    expect_equal(ncol(zz$interaction$coefficients), 7)
    expect_equal(sum(is.na(zz$interaction$coefficients)), 3)
    expect_equal(round(zz$interaction$pvalue,4), 0.0029)
    expect_equal(round(zz$interaction$coefficients[2,1],4), 0.3345)
    expect_equal(round(zz$interaction$coef.pvalue[2,1],5), 0.70458)
    expect_error(suppressWarnings(Regression(bank$Overall~bank$Fees+bank$Interest,
                                             interaction=bank$ATM)), NA)
})

all.types <- c("Linear", "Binary Logit", "Poisson", "Quasi-Poisson", "NBD", "Ordered Logit", "Multinomial Logit")
w1 <- rep(1, nrow(bank))
f1 <- bank$ID < 200
test_that("Weights", {

    for (tt in all.types[-7])
    {
        expect_error(suppressWarnings(Regression(Overall ~ Fees + Interest,
                                                 interaction = ATM, data = bank, type = tt)), NA)
        # svyolr (weighted Ordered Logit) will error since it wants to invert the Hessian and the response variable
        # will have unobserved levels for some sub-groups splitting by ATM for the interaction test. This gives a
        # singular Hessian (row/column for the intercept adjustments at the unobserved level)
        error.msg <- if (tt != "Ordered Logit") NA else "^Cannot perform regression split by interaction term"

        expect_error(suppressWarnings(Regression(Overall ~ Fees + Interest,
                                                 interaction = ATM, data = bank, type = tt, weights = w1)), error.msg)
        expect_error(suppressWarnings(Regression(Overall ~ Fees + Interest,
                                                 interaction = ATM, data = bank, type = tt, weights = w1, subset = f1)), error.msg)
        expect_error(suppressWarnings(Regression(Overall ~ Fees + Interest,
                                                 interaction = ATM, data = bank, type = tt,
                                                     weights = w1, subset = f1, output = "Relative Importance Analysis")), NA)
    }
    expect_error(suppressWarnings(Regression(Overall ~ Fees + Interest,
                                             interaction = ATM, data = bank, type = "Multinomial Logit")))
})

test_that("Multiple imputation", {
    z1 <- suppressWarnings(Regression(Overall ~ Fees + Interest, interaction = ATM,
                                      data = bank))
    z2 <- suppressWarnings(Regression(Overall ~ Fees + Interest, interaction = ATM,
                                      data = bank, missing = "Multiple imputation", seed=123))
    z3 <- suppressWarnings(Regression(Overall ~ Fees + Interest, interaction = ATM, subset = bank$Branch==4,
                                      data = bank, missing = "Multiple imputation", seed=123))
    expect_equal(round(z2$interaction$pvalue, 3), 0.002)
    expect_true(grepl("n = 648 cases used in estimation of a total sample size of 849;", z1$footer))
    expect_true(grepl("n = 107 cases used in estimation of a total sample size of 129 (bank$Branch == 4);", z3$footer, fixed = TRUE))
    c1 <- as.vector(z1$interaction$coefficients)
    c2 <- as.vector(z2$interaction$coefficients)
    p1 <- as.vector(z1$interaction$coef.pvalues)
    p2 <- as.vector(z2$interaction$coef.pvalues)
    expect_equal(cor(c1, c2, use="pairwise.complete.obs") > 0.99, TRUE)
    expect_equal(cor(p1, p2, use="pairwise.complete.obs") > 0.71, TRUE)

    z3 <- suppressWarnings(Regression(Overall ~ Fees + Interest, interaction = ATM,
                                      data = bank, output="Relative Importance Analysis",
                                      missing = "Multiple imputation", seed=123))
    z4 <- suppressWarnings(Regression(Overall ~ Fees + Interest, interaction = ATM,
                                      data = bank, output="Relative Importance Analysis"))
    expect_equal(length(grep("R-squared", z2$footer)), 1)
    expect_equal(length(grep("R-squared", z3$footer)), 0)
    expect_equal(length(grep("R-squared", z4$footer)), 0)
})

test_that("Relative importance", {
    z2 <- suppressWarnings(Regression(Overall ~ Fees + Interest, interaction = Branch,
                                      data = bank, output="Relative Importance Analysis"))
    expect_equal(round(z2$interaction$coefficients[2,1], 2), 3.27)
    expect_equal(round(z2$interaction$coef.pvalues[2,1], 4), 0.4664)

    data("cola", package="flipExampleData")
    res2 <- suppressWarnings(Regression(Q9_B~Q5_5_2+Q5_7_2+Q5_13_2+Q5_16_2+Q5_17_2+Q5_19_2+Q5_23_2+Q5_25_2+Q5_31_2, interaction=Q2, data=cola, show.labels=T, output="Relative Importance Analysis"))
    res3 <- suppressWarnings(Regression(Q9_B~Q5_5_2+Q5_7_2+Q5_13_2+Q5_16_2+Q5_17_2+Q5_19_2+Q5_23_2+Q5_25_2+Q5_31_2, interaction=Q2, data=cola, show.labels=T, output="Relative Importance Analysis", importance.absolute=T))
    res4 <- suppressWarnings(Regression(Q9_B~Q5_5_2+Q5_7_2+Q5_13_2+Q5_16_2+Q5_17_2+Q5_19_2+Q5_23_2+Q5_25_2+Q5_31_2, interaction=Q2, data=cola, show.labels=T, output="Relative Importance Analysis", correction="False Discovery Rate"))

    # coefficients with signs match Q output
    expect_equal(unname(round(res2$interaction$coefficients[,1],3)), c(-9.550,34.350,2.209,0.426,27.504,-1.173,-8.698,9.594,-6.497))
    expect_equal(round(res2$interaction$coef.pvalues[,1],4), c(0.0402,0.1360,0.8537,0.9939,0.8152,0.6605,0.9571,0.7539,0.2133))
    expect_equal(round(res2$interaction$coef.pFDR[,1],4), c(0.7238,1,1,1,1,1,1,1,1))
    expect_equal(res4$interaction$coef.pvalues, res2$interaction$coef.pFDR)

    # Q does not allow signs to be ignored - just checking values are different
    expect_equal(res3$interaction$coefficients[1,1],  9.5495906)
    expect_equal(round(res3$interaction$coef.pvalues[1,1],7),  0.4392116)

    })

f4 <- (1:nrow(bank)) %% 4
test_that("Robust SE", {
    #expect_error(Regression(Overall ~ Fees + Interest, interaction = ATM, data = bank, robust.se = T))
    expect_error(suppressWarnings(Regression(Overall ~ Fees + Interest, interaction = f4, data = bank, robust.se = T)), NA)
})

test_that("Coefficients", {

    set.seed(1232)
    n <- 500
    x1 <- rnorm(n)
    x2 <- rnorm(n)
    x3 <- rnorm(n)
    f3 <- round(runif(n, 5, 10))
    ee <- 0.1 * rnorm(n)
    yy <- 1 * x1 + 1 * x2 + 1 * x3 * f3 + ee

    # Only interactions to the x3 coefficient should be significant
    z5 <- Regression(yy~x1+x2+x3, interaction=f3)
    expect_equal(sum((z5$interaction$coef.pvalues[1,] < 0.05)), 0)
    expect_equal(sum((z5$interaction$coef.pvalues[4,] < 0.05)), 6)

})

test_that("Empty factors", {
    data("cola", package="flipExampleData")
    expect_error(suppressWarnings(Regression(Q3~Q6_A + Q6_B + Q6_C, data=cola, interaction=Q2)), NA)
    expect_error(suppressWarnings(Regression(Q3~Q6_A + Q6_B + Q6_C, data=cola, interaction=Q28)),NA)
    expect_error(suppressWarnings(Regression(Q3~Q6_A + Q6_B + Q6_C, data=cola, interaction=Q28, output="Relative Importance Analysis")),NA)
})

test_that("P-value correction", {
    data("bank", package="flipExampleData")
    zLU <- suppressWarnings(Regression(Overall~Fees+Interest, interaction=ATM, data=bank))
    zLC <- suppressWarnings(Regression(Overall~Fees+Interest, interaction=ATM, data=bank, correction="False Discovery Rate"))

    expect_equal(unname(round(zLU$interaction$coefficients[2,-6],3)), c(0.335,0.462,0.295,0.426,0.379,0.390))
    expect_equal(round(zLU$interaction$coef.pvalues[2,-6],2), c(0.70,0.28,0.06,0.51,0.94))

    # Excluding first row will give corrected p-values that match Q output
    # Because the correction in Q output does not include the intercept p-values
    p.corQ <- PValueAdjustFDR(zLU$interaction$coef.pvalues[-1,])
    expect_equal(round(p.corQ[1:10],4), c(1,1,1,1,0.5994,1,1,1,1,1))
    p.corR <- PValueAdjustFDR(zLU$interaction$coef.pvalues)
    expect_equal(zLC$interaction$coef.pvalues[2,1:5], p.corR[seq(2,15,3)])
})



test_that("Crosstab with dot in formula",
{

    set.seed(1232)
    n <- 500
    dat <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
    f3 <- round(runif(n, 5, 10))
    ee <- 0.1 * rnorm(n)
    dat$yy <- 1 * dat$x1 + 1 * dat$x2 + 1 * dat$x3 * f3 + ee

    # Only interactions to the x3 coefficient should be significant
    z5 <- Regression(yy~., interaction=f3, data = dat)
    expect_equal(sum((z5$interaction$coef.pvalues[1,] < 0.05)), 0)
    expect_equal(sum((z5$interaction$coef.pvalues[4,] < 0.05)), 6)

})

test_that("Exported data", {
    zz <- suppressWarnings(Regression(Overall ~ Fees + Interest, interaction = ATM, data = bank))
    cd <- attr(zz, "ChartData")
    expect_equal(zz$interaction$coefficients, cd[-nrow(cd), ])
})
