context("P-values")

data(bank, package = "flipExampleData")
set.seed(123456)
dummy <- rnorm(nrow(bank))
bank <- cbind(bank, dummy)

test_that("Non-significant", {
    z1 <- Regression(Overall~Fees+Interest+dummy, data=bank)
    z2 <- Regression(Overall~Fees+Interest+dummy, data=bank, correction="Bonferroni")
    z3 <- Regression(Overall~Fees+Interest+dummy, data=bank, correction="False Discovery Rate")
    expect_equal(z1$summary$coefficients[4,4] < z3$summary$coefficients[4,4], TRUE)
    expect_equal(z3$summary$coefficients[4,4] < z2$summary$coefficients[4,4], TRUE)
})

test_that("Significant", {
    z1 <- Regression(Overall~Fees+Interest+ATM, data=bank)
    z2 <- Regression(Overall~Fees+Interest+ATM, data=bank, correction="Bonferroni")
    z3 <- Regression(Overall~Fees+Interest+ATM, data=bank, correction="False Discovery Rate")
    z4 <- Regression(Overall~Fees+Interest+ATM, data=bank, correction="Bonferroni", missing = "Multiple imputation")
    z5 <- Regression(Overall~Fees+Interest+ATM, data=bank, correction="None", missing = "Multiple imputation")
    expect_equal(z1$summary$coefficients[4,4], z3$summary$coefficients[4,4])
    expect_equal(z3$summary$coefficients[4,4] < z2$summary$coefficients[4,4], TRUE)
    expect_equal(z4$summary$coefficients[4,4] != z5$summary$coefficients[4,4], TRUE)
    expect_equal(z4$summary$coefficients[4,3] == z5$summary$coefficients[4,3], TRUE)
})

test_that("Multinomial Logit", {
    z1 <- Regression(Overall~Fees+Interest+dummy, data=bank, type="Multinomial Logit")
    z2 <- Regression(Overall~Fees+Interest+dummy, data=bank, correction="Bonferroni", type="Multinomial Logit")
    z3 <- Regression(Overall~Fees+Interest+dummy, data=bank, correction="False Discovery Rate", type="Multinomial Logit")
    z4 <- Regression(Overall~Fees+Interest+dummy, data=bank, correction="Bonferroni", type="Multinomial Logit", missing="Multiple imputation")
    z5 <- Regression(Overall~Fees+Interest+dummy, data=bank, correction="None", type="Multinomial Logit", missing="Multiple imputation")
    expect_equal(z1$p.values[1,1] < z2$p.values[1,1], TRUE)
    expect_equal(z1$p.values[1,1] < z3$p.values[1,1], TRUE)
    expect_equal(sum(z4$summary$coefficients[,4] > z5$summary$coefficients[,4]), 24)
})

test_that("Relative importance analysis", {
    z1 <- Regression(Overall~Fees+Interest+dummy, data=bank, output="Relative Importance Analysis", correction="None")
    z2 <- Regression(Overall~Fees+Interest+dummy, data=bank, output="Relative Importance Analysis", correction="Bonferroni")
    expect_equal(all(z1$summary$coefficients[,4] < z2$summary$coefficients[,4]), TRUE)
})

test_that("ANOVA", {
    z1 <- Regression(Overall~Fees+Interest+dummy, data=bank, output="ANOVA", correction="None")
    z2 <- Regression(Overall~Fees+Interest+dummy, data=bank, output="ANOVA", correction="Bonferroni")
    expect_equal(sum(z1$anova$P < z2$anova$P, na.rm=T), 3)

    z3 <- Regression(Overall~Fees+Interest+dummy, data=bank, output="ANOVA", type = "Multinomial Logit", correction="None")
    z4 <- Regression(Overall~Fees+Interest+dummy, data=bank, output="ANOVA", type = "Multinomial Logit", correction="Bonferroni")
    expect_equal(sum(z3$anova$P < z4$anova$P, na.rm=T), 3)

    z5 <- Regression(Overall~Fees+Interest+dummy, data=bank, output="ANOVA", type = "Ordered Logit", correction="None")
    z6 <- Regression(Overall~Fees+Interest+dummy, data=bank, output="ANOVA", type = "Ordered Logit", correction="Bonferroni")
    expect_equal(z5$anova$P[3] < z6$anova$P[3], TRUE)
})

test_that("Crosstab interaction", {
    z1 <- Regression(Overall~Fees+Interest, interaction=Branch, data=bank, correction="None", interaction.pvalue = T)
    z2 <- Regression(Overall~Fees+Interest, interaction=Branch, data=bank, correction="Bonferroni", interaction.pvalue = T)
    expect_equal(all(z1$interaction$coef.pvalues < z2$interaction$coef.pvalues), TRUE)
})

test_that("Robust SEs", {
    z1 <- Regression(Overall~Fees+Interest, data=bank, robust.se = T)
    z2 <- Regression(Overall~Fees+Interest, data=bank, robust.se = T, correction="Bonferroni")
    expect_equal(all(z1$summary$coefficients[,3] == z2$summary$coefficients[,3]), TRUE)
    expect_equal(all(z1$summary$coefficients[,4] <  z2$summary$coefficients[,4]), TRUE)
})



