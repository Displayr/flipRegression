context("Data problems")

test_that("Removing unused factors prior to regression", {

    data(phone, package = "flipExampleData")
    levs <- attr(phone$q3, "value.labels")

    z <- phone$q3
    z[is.na(z)] <- 100
    z <- as.numeric(z)
    z <- factor(z)

    lv <- c("-9", "0", names(levs[7:1]), "100")
    levels(z) <- lv
    z[z == "100"] <- NA
    phone$q3 <- z

    expect_error(suppressWarnings(flipRegression::Regression(q3 ~ q2, data = phone, missing = "Multiple imputation")), NA)
    expect_error(suppressWarnings(flipRegression::Regression(q2 ~ q3, data = phone, missing = "Multiple imputation")), NA)

})

test_that("FDR corrected p-values", {
    p.raw <- c(1e-3, 1e-2, 0.15, 0.3, 0.5, NA)
    p1 <- p.adjust(p.raw, "fdr")
    p2 <- PValueAdjustFDR(p.raw)
    expect_equal(p1[2], p2[2])
    expect_equal(sum(p1[1:5] != p2[1:5]), 4)
})


data(adult.2000, package = "flipExampleData")
set.seed(1234)
adult.2000$race[runif(2000) > 0.9] <- NA
adult.2000$age[runif(2000) > 0.9] <- -Inf
adult.2000$hrs_per_week[runif(2000) > 0.9] <- Inf

test_that("Infinity in data", {
    expect_error(suppressWarnings(Regression(hrs_per_week ~ sex + race + age,
                  type = "Linear",
                  data = adult.2000,
                  missing = "Exclude cases with missing data")),
                 "Variable(s) hrs_per_week, age contain infinite values. Either recode the infinities to finite values or set them as missing data.",
                 fixed = TRUE)

})
