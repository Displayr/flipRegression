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

    expect_error(suppressWarnings(flipRegression::Regression(q3 ~ q2, data = phone, missing = "Multiple imputation")))
    expect_error(suppressWarnings(flipRegression::Regression(q2 ~ q3, data = phone, missing = "Multiple imputation")), NA)

})
