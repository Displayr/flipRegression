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
    data$Yok <- rbinom(nrow(data), size = 1, prob = 0.5)
    error.prefix <- paste0("Both the outcome and predictor variable need to be binary variables ",
                           "(only take the values zero or one). The ")
    plural.error.prefix <- paste0("Both the outcome and predictor variables need to be binary variables ",
                                  "(only take the values zero or one). The ")
    outcome.warning <- paste0("outcome variable ", sQuote("Y"), " is not a binary variable")
    error.suffix <- paste0(", please change the variable to a binary variable if you wish to ",
                           "create output with the Jaccard coefficients.")
    plural.error.suffix <- paste0(", please change the variables to binary variables if you wish to ",
                                  "create output with the Jaccard coefficients.")
    expect_warning(Regression(Y ~ X1, data = data, output = "Jaccard Coefficient"),
                   "^Y has been dichotimized")
    expect_error(flipRegression:::processDataSuitableForJaccard(Yok ~ badX1,
                                                                data = subset(data, select = c("Yok", "badX1"))),
                 paste0(error.prefix, "predictor variable ", sQuote("badX1"),
                        " is not a binary variable", error.suffix),
                 fixed = TRUE)
    expect_error(flipRegression:::processDataSuitableForJaccard(Yok ~ X1 + badX1,
                                                                data = subset(data, select = c("Yok", "X1", "badX1"))),
                 paste0(plural.error.prefix, "predictor variable ", sQuote("badX1"),
                        " is not a binary variable", error.suffix),
                 fixed = TRUE)
    expect_error(flipRegression:::processDataSuitableForJaccard(Yok ~ X1 + badX1 + badX2,
                                                                data = subset(data, select = c("Yok", "X1", "badX1", "badX2"))),
                 paste0(plural.error.prefix, "predictor variables ", sQuote("badX1"),
                        " and ", sQuote("badX2"), " are not binary variables", plural.error.suffix), fixed = TRUE)
    expect_error(flipRegression:::processDataSuitableForJaccard(Yok ~ X1 + badX1 + badX2 + badX3,
                                                                data = subset(data, select = c("Yok", "X1", "badX1", "badX2", "badX3"))),
                 paste0(plural.error.prefix, "predictor variables ", sQuote("badX1"), ", ",
                        sQuote("badX2"), " and ", sQuote("badX3"), " are not binary variables", plural.error.suffix), fixed = TRUE)
    # Outcome and predictors is ok.
    data$Y <- rbinom(nrow(data), size = 1, prob = 0.5)
    expected.output.list <- list(data = subset(data, select = c("Y", "X1")),
                                 formula = Y ~ X1,
                                 formula.with.interaction = Y ~ X1,
                                 labels = "X1")
    expect_equal(flipRegression:::processDataSuitableForJaccard(Y ~ X1, data = subset(data, select = c("Y", "X1"))),
                 expected.output.list)
    # Good outcome but bad predictors throw errors
    expect_error(flipRegression:::processDataSuitableForJaccard(Y ~ badX1, data = subset(data, select = c("Y", "badX1"))),
                 paste0(error.prefix, "predictor variable ", sQuote("badX1"),
                        " is not a binary variable", error.suffix), fixed = TRUE)
    expect_error(flipRegression:::processDataSuitableForJaccard(Y ~ X1 + badX1, data = subset(data, select = c("Y", "X1", "badX1"))),
                 paste0(plural.error.prefix, "predictor variable ", sQuote("badX1"),
                        " is not a binary variable", error.suffix), fixed = TRUE)
    expect_error(flipRegression:::processDataSuitableForJaccard(Y ~ X1 + badX1 + badX2,
                                                                data = subset(data, select = c("Y", "badX1", "badX2"))),
                 paste0(plural.error.prefix, "predictor variables ", sQuote("badX1"),
                        " and ", sQuote("badX2"), " are not binary variables", plural.error.suffix), fixed = TRUE)
    expect_error(flipRegression:::processDataSuitableForJaccard(Y ~ X1 + badX1 + badX2 + badX3,
                                                                data = subset(data, select = c("Y", "badX1", "badX2", "badX3"))),
                 paste0(plural.error.prefix, "predictor variables ", sQuote("badX1"), ", ",
                        sQuote("badX2"), " and ", sQuote("badX3"), " are not binary variables", plural.error.suffix), fixed = TRUE)
    # Handles interaction terms.
    data$int <- factor(rep(LETTERS[1:4], 25))
    input.dat <- subset(data, select = c("Y", "X1", "int"))
    expected.output.list <- list(data = input.dat,
                                 formula = Y ~ X1,
                                 formula.with.interaction = Y ~ X1 + int + X1:int,
                                 labels = "X1")
    expect_equal(flipRegression:::processDataSuitableForJaccard(Y ~ X1,
                                                                interaction.name = "int",
                                                                data = subset(data, select = c("Y", "X1", "int"))),
                 expected.output.list)
    # Check fancy label are returned if requested
    attr(input.dat$X1, "label") <- "X1 from Q2"
    expected.output.list$data <- input.dat
    expect_equal(flipRegression:::processDataSuitableForJaccard(Y ~ X1,
                                                                interaction.name = "int",
                                                                data = input.dat),
                 expected.output.list)
    expected.output.list$labels <- "X1 from Q2"
    expect_equal(flipRegression:::processDataSuitableForJaccard(Y ~ X1, interaction.name = "int",
                                                                data = input.dat, show.labels = TRUE),
                 expected.output.list)

    # Check warnings thrown for data with no variation.
    data$Y1 <- 1
    error.prefix <- "Both the outcome and predictor variables should be binary variables. However, the "
    single.constant <- paste0(", it is constant with no variation (all values in the ",
                              "variable are zero or all values in the variable are one). ")
    many.constant <- paste0(", the variables are constant with no variation (all values in the ",
                            "variable are zero or all values in the variable are one). ")
    single.error.suffix <- "Consider if this variable is appropriate and remove if necessary."
    many.error.suffix <- "Consider if these variables are appropriate and remove if necessary."
    expect_error(Regression(Y1 ~ X1 + X2, data = data, output = "Jaccard Coefficient"),
                 paste0("The Outcome variable needs to be a binary variable. It is not. ",
                        "It is constant with no variation (all values in the variable are the same). ",
                        "Please replace the outcome variable with a binary variable."),
                 fixed = TRUE)
    data$X10 <- 1
    data$X21 <- 1
    expect_error(flipRegression:::processDataSuitableForJaccard(Y ~ X10 + X2, data = subset(data, select = c("Y", "X10", "X2"))),
                 paste0(error.prefix, "predictor variable ", sQuote("X10"), " is not a binary variable",
                        single.constant, single.error.suffix), fixed = TRUE)
    expect_error(flipRegression:::processDataSuitableForJaccard(Y1 ~ X10 + X2 + X21,
                                                                data = subset(data, select = c("Y1", "X10", "X2", "X21"))),
                 paste0(error.prefix, "outcome variable ", sQuote("Y1"), " is not a binary variable",
                        " and predictor variables ", sQuote("X10"), " and ", sQuote("X21"), " are not binary variables",
                        many.constant, many.error.suffix), fixed = TRUE)
    # Check binary categorical variable handled and converted to numeric binary
    binary.cat <- factor(sample(LETTERS[1:2], size = nrow(data), replace = TRUE))
    data$binary.cat <- binary.cat
    binary.splits <- lapply(1:nlevels(data$binary.cat), function(x) as.numeric(data$binary.cat == levels(binary.cat)[x]))
    output.dat <- data.frame(cbind(data[names(data) %in% c("Y", "X1")], binary.splits))
    names(output.dat) <- c("Y", "X1", "binary.cat.1", "binary.cat.2")
    attr(output.dat$binary.cat.1, "label") <- "binary.cat: A"
    attr(output.dat$binary.cat.2, "label") <- "binary.cat: B"
    input.dat <- subset(data, select = c("Y", "X1", "binary.cat"))
    expected.output.list <- list(data = output.dat,
                                 formula = Y ~ X1 + binary.cat.1 + binary.cat.2,
                                 formula.with.interaction = Y ~ X1 + binary.cat.1 + binary.cat.2,
                                 labels = c("X1", "binary.cat.1", "binary.cat.2"))
    expect_equal(flipRegression:::processDataSuitableForJaccard(Y ~ X1 + binary.cat,
                                                                data = input.dat),
                 expected.output.list)
    # Check categorical variables with more than two levels handled and converted to numeric binary
    cat <- factor(sample(LETTERS[1:3], size = nrow(data), replace = TRUE))
    data$cat <- cat
    binary.splits <- lapply(1:nlevels(data$cat), function(x) as.numeric(data$cat == levels(cat)[x]))
    output.dat <- data.frame(cbind(data[names(data) %in% c("Y", "X1")], binary.splits))
    names(output.dat) <- c("Y", "X1", "cat.1", "cat.2", "cat.3")
    attr(output.dat$cat.1, "label") <- "cat: A"
    attr(output.dat$cat.2, "label") <- "cat: B"
    attr(output.dat$cat.3, "label") <- "cat: C"
    input.dat <- subset(data, select = c("Y", "X1", "cat"))
    expected.output.list <- list(data = output.dat,
                                 formula = Y ~ X1 + cat.1 + cat.2 + cat.3,
                                 formula.with.interaction = Y ~ X1 + cat.1 + cat.2 + cat.3,
                                 labels = c("X1", "cat.1", "cat.2", "cat.3"))

    expect_equal(flipRegression::processDataSuitableForJaccard(Y ~ X1 + cat,
                                                               data = input.dat),
                 expected.output.list)
    # Check ordinal factors are split into binary numeric variables
    ordered <- factor(sample(LETTERS[1:3], size = nrow(data), replace = TRUE), ordered = TRUE)
    data$ordered <- ordered
    binary.splits <- lapply(1:nlevels(data$ordered), function(x) as.numeric(data$ordered == levels(ordered)[x]))
    output.dat <- data.frame(cbind(data[names(data) %in% c("Y", "X1")], binary.splits))
    names(output.dat) <- c("Y", "X1", "ordered.1", "ordered.2", "ordered.3")
    attr(output.dat$ordered.1, "label") <- "ordered: A"
    attr(output.dat$ordered.2, "label") <- "ordered: B"
    attr(output.dat$ordered.3, "label") <- "ordered: C"
    input.dat <- subset(data, select = c("Y", "X1", "ordered"))
    expected.output.list <- list(data = output.dat,
                                 formula = Y ~ X1 + ordered.1 + ordered.2 + ordered.3,
                                 formula.with.interaction = Y ~ X1 + ordered.1 + ordered.2 + ordered.3,
                                 labels = c("X1", "ordered.1", "ordered.2", "ordered.3"))
    expect_equal(flipRegression:::processDataSuitableForJaccard(Y ~ X2 + ordered,
                                                                data = input.dat),
                expected.output.list)
    # Allow categorical outcome variables for Jaccard
    data$Y <- factor(sample(LETTERS[1:2], size = nrow(data), replace = TRUE))
    data$Y3 <- factor(sample(LETTERS[1:3], size = nrow(data), replace = TRUE))
    # Allow categorical variable with two levels
    expect_error(category.2 <- Regression(Y ~ X1 + X2, data = data, output = "Jaccard Coefficient"), NA)
    # Check the output equivalent where outcome is numeric binary
    data$Yb <- unclass(data$Y) - 1
    expect_error(num.binary <- Regression(Yb ~ X1 + X2, data = data, output = "Jaccard Coefficient"), NA)
    expect_identical(category.2$importance, num.binary$importance)
    # Check the outcome variable is dichotomized into a numeric binary variable
    expect_warning(dichot <- Regression(Y3 ~ X1 + X2, data = data, output = "Jaccard Coefficient"),
                   "Y3 has been dichotimized into <= A & >= B", fixed = TRUE)
    expect_setequal(dichot$model$Y3, c(0, 1))
    # Check output equivalent to numeric binary outcome
    data$Yb <- as.numeric(data$Y3 %in% LETTERS[2:3])
    expect_error(num.binary <- Regression(Yb ~ X1 + X2, data = data, output = "Jaccard Coefficient"), NA)
    expect_identical(dichot$importance, num.binary$importance)
})
