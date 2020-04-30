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

test_that("DS-2876: Jaccard coefficients not suitable", {
    # Outcome not suitable
    set.seed(12321)
    Y <- rnorm(100)
    X <- lapply(1:3, function(x) rbinom(10, size = 1, prob = 0.5))
    badX <- lapply(1:3, function(x) rnorm(10))
    data <- data.frame(Y, X, badX)
    names(data) <- c("Y", paste0("X", 1:3), paste0("badX", 1:3))
    error.prefix <- paste0("Both the outcome and predictor variable need to be binary variables ",
                           "(only take the values zero or one). The ")
    plural.error.prefix <- paste0("Both the outcome and predictor variables need to be binary variables ",
                                  "(only take the values zero or one). The ")
    outcome.warning <- paste0("outcome variable ", sQuote("Y"), " is not a binary variable")
    error.suffix <- paste0(", please change the variable to a binary variable if you wish to ",
                           "create output with the Jaccard coefficients.")
    plural.error.suffix <- paste0(", please change the variables to binary variables if you wish to ",
                                  "create output with the Jaccard coefficients.")
    expect_error(Regression(Y ~ X1, data = data, output = "Jaccard Coefficient"),
                 paste0(error.prefix, outcome.warning, error.suffix),
                 fixed = TRUE)
    expect_error(Regression(Y ~ badX1, data = data, output = "Jaccard Coefficient"),
                 paste0(error.prefix, outcome.warning, " and predictor variable ", sQuote("badX1"),
                        " is not a binary variable", plural.error.suffix),
                 fixed = TRUE)
    expect_error(Regression(Y ~ X1 + badX1, data = data, output = "Jaccard Coefficient"),
                 paste0(plural.error.prefix, outcome.warning, " and predictor variable ", sQuote("badX1"),
                        " is not a binary variable", plural.error.suffix),
                 fixed = TRUE)
    expect_error(Regression(Y ~ X1 + badX1 + badX2, data = data, output = "Jaccard Coefficient"),
                 paste0(plural.error.prefix, outcome.warning, " and predictor variables ", sQuote("badX1"),
                        " and ", sQuote("badX2"), " are not binary variables", plural.error.suffix), fixed = TRUE)
    expect_error(Regression(Y ~ X1 + badX1 + badX2 + badX3, data = data, output = "Jaccard Coefficient"),
                 paste0(plural.error.prefix, outcome.warning, " and predictor variables ", sQuote("badX1"), ", ",
                        sQuote("badX2"), " and ", sQuote("badX3"), " are not binary variables", plural.error.suffix), fixed = TRUE)
    # Outcome and predictors is ok.
    data$Y <- rbinom(nrow(data), size = 1, prob = 0.5)
    expect_error(Regression(Y ~ X1, data = data, output = "Jaccard Coefficient"), NA)
    # Good but bad predictors throw errors
    expect_error(Regression(Y ~ badX1, data = data, output = "Jaccard Coefficient"),
                 paste0(error.prefix, "predictor variable ", sQuote("badX1"),
                        " is not a binary variable", error.suffix), fixed = TRUE)
    expect_error(Regression(Y ~ X1 + badX1, data = data, output = "Jaccard Coefficient"),
                 paste0(plural.error.prefix, "predictor variable ", sQuote("badX1"),
                        " is not a binary variable", error.suffix), fixed = TRUE)
    expect_error(Regression(Y ~ X1 + badX1 + badX2, data = data, output = "Jaccard Coefficient"),
                 paste0(plural.error.prefix, "predictor variables ", sQuote("badX1"),
                        " and ", sQuote("badX2"), " are not binary variables", plural.error.suffix), fixed = TRUE)
    expect_error(Regression(Y ~ X1 + badX1 + badX2 + badX3, data = data, output = "Jaccard Coefficient"),
                 paste0(plural.error.prefix, "predictor variables ", sQuote("badX1"), ", ",
                        sQuote("badX2"), " and ", sQuote("badX3"), " are not binary variables", plural.error.suffix), fixed = TRUE)
    # Handles interaction terms.
    data$int <- factor(rep(LETTERS[1:4], 25))
    expect_error(Regression(Y ~ X1, interaction = int, data = data, output = "Jaccard Coefficient"), NA)

})
