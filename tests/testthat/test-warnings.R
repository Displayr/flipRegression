context("warnings")

library(flipU)

test_that("Predictor is outcome", {
    y  <- 1:100 + .001
    x <- rnorm(100, y, y)

    expect_error(Regression(y ~ y, robust.se = FALSE), "A variable may only appear once")
    expect_warning(Regression(y ~ x, robust.se = TRUE), NA)
})

test_that("Heteroskedasticity", {
    y  <- 1:100 + .001
    x <- rnorm(100, y, y)
    expect_error(out <- Regression(y ~ x, robust.se = FALSE), NA)
    # Prevent pop-ups
    mockery::stub(print.Regression, "print.htmlwidget", NULL)
    expect_warning(print(out), "Breusch")
    expect_warning(Regression(y ~ x, robust.se = TRUE), NA)
})

test_that("Outliers", {
    set.seed(133452)
    y  <- 1:10 + rnorm(10, .1)
    x <- 1:10
    expect_error(out <- Regression(y ~ x), NA)
    # Prevent pop-ups
    mockery::stub(print.Regression, "print.htmlwidget", NULL)
    expect_warning(print(out), NA)
    x <- c(10, 1:9)
    expect_error(out <- Regression(y ~ x), NA)
    expect_warning(print(out), "Unusual observations detected")
})

test_that("DS-2704: Check user prompts for automated outlier detection in unusual observation case", {
    set.seed(133452)
    y  <- 1:10 + rnorm(10, .1)
    x <- 1:10
    x <- c(10, 1:9)
    expect_error(out <- Regression(y ~ x), NA)
    # Prevent pop-ups
    mockery::stub(print.Regression, "print.htmlwidget", NULL)
    expect_warning(print(out),
                   paste0("Unusual observations detected. ",
                          "Consider re-running the analysis using automated outlier removal"))
    expect_error(out <- Regression(y ~ x, outlier.prop.to.remove = 0.1), NA)
    expect_warning(print(out), NA)
    x <- c(0, x)
    y <- c(10, y)
    expect_error(out <- Regression(y ~ x, outlier.prop.to.remove = 0.1), NA)
    expect_warning(
        print(out),
        paste0("Unusual observations detected. After removing a proportion of the data from the ",
               "analysis, unusual observations exist in the"))
})

test_that("Dichotomized", {
    set.seed(12)
    # Simulate data from logistic model
    x <- sample(0:1, size = 10, replace = TRUE)
    y <- rbinom(10L, size = 10L, prob = 1 / (1 + exp(-1 - 2 * x)))
    expect_warning(Regression(y ~ x, type = "Binary Logit"), "y has been dichotimized")
    y.dichot <- rbinom(10L, size = 1L, prob = 1 / (1 + exp(-1 - 2 * x)))
    expect_warning(Regression(y.dichot ~ x, type = "Binary Logit"), NA)
})

test_that("Missing", {
    y  <- 1:50 + rnorm(50, .1)
    x <- 1:50
    expect_warning(Regression(y ~ x), NA)
    y  <- c(rep(NA, 500), 1:50 + rnorm(50, .1))
    x <- 1:550
    expect_warning(Regression(y ~ x), "the data is missing")
})


for (type in c("Linear", "Poisson", "Quasi-Poisson", "Binary Logit", "NBD",
               "Multinomial Logit", "Ordered Logit"))
    test_that(paste("Categories", type), {
        # Prevent pop-ups
        mockery::stub(print.Regression, "print.htmlwidget", NULL)
        set.seed(213)
        y <- 1:100
        x <- rnorm(100, mean = y, sd = y)
        if (type == "Multinomial Logit")
            y <- factor(cut(y, breaks = c(-Inf, 33, 66, Inf), Labels = c("Low", "Medium", "High")))
        warn <- switch(type,
                       "Ordered Logit" = "NaNs produced",
                       "Binary Logit" = "y has been dichotimized",
                       NA)
        expect_warning(out <- Regression(y ~ x, type = type), warn)
        observed.warnings <- capture_warnings(print(out))
        no.warnings <- length(observed.warnings) == 0L
        unusual.or.breusch.sig <- any(grepl("Unusual observations detected|Breusch", observed.warnings))
        expect_true(no.warnings || unusual.or.breusch.sig)
        set.seed(213)
        warn <- switch(type, "Binary Logit" = "y has been dichotimized", NA)
        y <- sample(12, 100, replace = TRUE)
        x <- rnorm(100, y, y)
        if (type == "Multinomial Logit")
            y <- factor(cut(y, breaks = c(-Inf, 4, 8, Inf), Labels = c("Low", "Medium", "High")))
        expect_warning(out <- Regression(y ~ x, type = type), warn)
        warn <- switch(type,
            Linear = "appears to contain categories",
            `Binary Logit` = ,
            NBD = ,
            `Quasi-Poisson` = ,
            Poisson = "Unusual observations",
            NA
        )
        expect_warning(print(out), warn)
})


test_that("Removed aliased predictors", {
    x  <- 1:100
    y <- z <- rnorm(100)

    expect_warning(
        Regression(x ~ y + z),
        paste0("The following variable(s) are colinear with other variables and ",
               "no coefficients have been estimated: 'z'"),
               fixed = TRUE
    )
})

test_that("Removed aliased predictors (ordered logit)", {
    x  <- rep(1:20, each = 5)
    y <- z <- rnorm(100)
    Z <- structure(z, label = "Fancy Z")
    expect_warning(model <- polr(ordered(x) ~ y + z, Hess = FALSE),
                   "design appears to be rank-deficient", fixed = TRUE)

    expect_warning(Regression(x ~ y + z, type = "Ordered Logit"),
                   paste0("The following variable(s) are colinear with other variables and ",
                          "no coefficients have been estimated: 'z'"),
                   fixed = TRUE)
    expect_warning(Regression(x ~ y + Z, type = "Ordered Logit", show.labels = TRUE),
                   paste0("The following variable(s) are colinear with other variables and ",
                          "no coefficients have been estimated: 'Fancy Z'"),
                   fixed = TRUE)
    y <- z <- factor(sample(letters[1:3], 100, replace = TRUE))
    expect_warning(Regression(x ~ y + z, type = "Ordered Logit"),
                   paste0("The following variable(s) are colinear with other variables and ",
                          "no coefficients have been estimated: 'zb', 'zc'"),
                   fixed = TRUE)
    Z <- structure(z, label = "Fancy Z")
    expect_warning(Regression(x ~ y + Z, type = "Ordered Logit", show.labels = TRUE),
                   paste0("The following variable(s) are colinear with other variables and ",
                          "no coefficients have been estimated: 'Fancy Z: b', 'Fancy Z: c"),
                   fixed = TRUE)
})

data(bank, package = "flipExampleData")
test_that("No VIF warning with dummy variables", {
    z <- Regression(Overall ~ Fees + ATM + Branch, data = bank, missing = "Dummy variable adjustment")
    # Prevent pop-ups
    mockery::stub(print.Regression, "print.htmlwidget", NULL)
    expect_warning(print(z), paste0("Unusual observations detected"), fixed = TRUE)
})

test_that("DS-2826: Warnings for VIFs are improved and appropriate", {
    set.seed(123)
    sigma.mat <- matrix(
        c(1, 0.4, 0.9,
          0.4, 1, 0.5,
          0.9, 0.5, 1),
        nrow = 3,
        byrow = TRUE
    )
    X <- MASS::mvrnorm(n = 100, mu = c(1, 5, 6), Sigma = sigma.mat)
    beta <- c(5, 1, 2, 3)
    Y <- cbind(rep(1, nrow(X)), X) %*% beta + rnorm(100, sd = 1)
    dat <- data.frame(Y, X)
    model <- Regression(Y ~ ., data = dat)
    expect_warning(
        flipRegression:::checkVIFAndWarn(model),
        paste0("A high Variance Inflation Factor is observed in the coefficients X3 = 5.7; X1 = 5.2. ",
               "A value of 4 or more indicates the confidence interval for the coefficient is twice as ",
               "wide as they would be for uncorrelated predictors. A value of 10 or more indicates high ",
               "multicollinearity. Consider conducting a relative importance analysis by selecting the ",
               "output to be Relative Importance Analysis or Shapley Regression."),
        fixed = TRUE
    )
    # Consider glm and expect suggestion to not include Shapley
    dat.poisson <- dat
    dat.poisson$Y <- round(dat.poisson$Y)
    poisson.model <- Regression(Y ~ ., type = "Poisson", data = dat.poisson)
    expect_warning(
        flipRegression:::checkVIFAndWarn(poisson.model),
        paste0("A high Variance Inflation Factor is observed in the coefficients X3 = 5.7; X1 = 5.2. ",
               "A value of 4 or more indicates the confidence interval for the coefficient is twice as ",
               "wide as they would be for uncorrelated predictors. A value of 10 or more indicates high ",
               "multicollinearity. Consider conducting a relative importance analysis by selecting the ",
               "output to be Relative Importance Analysis."),
        fixed = TRUE
    )
    # Prevent pop-ups
    mockery::stub(print.Regression, "print.htmlwidget", NULL)
    # Expect VIF cacluation to be skipped an no warning for RIA
    for (ria in c("Relative Importance Analysis", "Shapley Regression"))
    {
        ria.model <- Regression(Y ~ X1 + X2 + X3, output = ria, data = dat)
        warning.msg <- capture_warnings(print(ria.model))
        expect_true(length(warning.msg) == 1)
        expect_true(grepl("^Unusual observations detected", warning.msg))
        expect_false(grepl("Variation Inflation Factor", warning.msg))
    }
    # Add metadata to variable labels
    createMetaData <- function(variable, question, label)
    {
        attr(variable, "question") <- question
        attr(variable, "label") <- label
        variable
    }
    dat.named <- data.frame(mapply(createMetaData,
                                   dat,
                                   c("Outcome", rep("Predictor", 3)),
                                   c("response", LETTERS[1:3]),
                                   SIMPLIFY = FALSE))
    model.with.labels <- Regression(Y ~ ., data = dat.named, show.labels = TRUE)
    expect_warning(
        flipRegression:::checkVIFAndWarn(model.with.labels),
        paste0("A high Variance Inflation Factor is observed in the coefficients C = 5.7; A = 5.2. ",
               "A value of 4 or more indicates the confidence interval for the coefficient is twice as ",
               "wide as they would be for uncorrelated predictors. A value of 10 or more indicates high ",
               "multicollinearity. Consider conducting a relative importance analysis by selecting the ",
               "output to be Relative Importance Analysis or Shapley Regression."),
        fixed = TRUE
    )
    # Expect no warning when VIF is low
    set.seed(12321)
    X <- MASS::mvrnorm(n = 100, mu = c(1, 5, 6), Sigma = matrix(c(1, 0.4, 0.2,
                                                                  0.4, 1, 0.5,
                                                                  0.2, 0.5, 1),
                                                                nrow = 3, byrow = TRUE))
    beta <- c(5, 1, 2, 3)
    Y <- cbind(rep(1, nrow(X)), X) %*% beta + rnorm(100, sd = 1)
    dat <- data.frame(Y, X)
    model.with.low.vif <- Regression(Y ~ ., data = dat)
    expect_warning(flipRegression:::checkVIFAndWarn(model.with.low.vif), NA)
    # Check Generalised VIF using a factor
    X4 <- cut(dat$X2, breaks = c(0, 4, 6, Inf), labels = LETTERS[1:3])
    beta <- c(beta, 2)
    Y <- cbind(rep(1, nrow(X)), as.matrix(dat[-1]), as.numeric(X4)) %*% beta + rnorm(100, sd = 1)
    original.dat <- dat
    dat <- data.frame(Y, X)
    dat$X4 <- X4
    model.with.vgif <- Regression(Y ~ ., data = dat)
    expect_warning(
        flipRegression:::checkVIFAndWarn(model.with.vgif),
        paste0("A high squared Generalized Variance Inflation Factor is observed in the coefficient for X2 = 4.9. ",
               "A value of 4 or more indicates the confidence interval for the coefficient is ",
               "twice as wide as they would be for uncorrelated predictors. A value of 10 or more ",
               "indicates high multicollinearity. Consider conducting a relative importance analysis by ",
               "selecting the output to be Relative Importance Analysis or Shapley Regression."),
        fixed = TRUE
    )
    # Check the warnings with Dummy variable adjustment
    # Make 40% missing in X1 and make 90% in common with X2 missing
    dat <- original.dat
    n.dat <- nrow(dat)
    x1.missing <- rep(c(TRUE, FALSE), c(40, 60))
    x2.missing <- rep(c(TRUE, FALSE), c(36, 64))
    dat$X1[x1.missing] <- NA
    dat$X2[x2.missing] <- NA
    dummy.model <- Regression(Y ~ ., data = dat, missing = "Dummy variable adjustment")
    expect_warning(flipRegression:::checkVIFAndWarn(dummy.model),
                   paste0("A high Variance Inflation Factor for the added dummy variables are observed in the ",
                          "coefficients X1 = 6.5; X2 = 6.5. A value of 4 or more indicates the confidence ",
                          "interval for the coefficient is twice as wide as they would be for uncorrelated ",
                          "predictors. A value of 10 or more indicates high multicollinearity. Consider ",
                          "conducting a relative importance analysis by selecting the output to be Relative ",
                          "Importance Analysis or Shapley Regression."),
                   fixed = TRUE)
})

test_that("DS-2826: Check VIFs are handled properly", {
    # Check and catch aliased predictors.
    set.seed(123)
    x  <- rnorm(10)
    y <- z <- rnorm(10)
    # Aliased predictors in regressions are computed and print without issue
    expect_warning(
        aliased.mod <- Regression(x ~ y + z),
        "The following variable(s) are colinear with other variables and no coefficients have been estimated: 'z'",
        fixed = TRUE
    )
    # Prevent pop-ups
    mockery::stub(print.Regression, "print.htmlwidget", NULL)
    expect_error(print(aliased.mod), NA)
    # Check the alias check is used as well as the predictor count checks (see car::vif)
    u <- w <- rnorm(10)
    expect_warning(
        aliased.mod <- Regression(x ~ u + w + y + z),
        "The following variable(s) are colinear with other variables and no coefficients have been estimated: 'w', 'z'",
        fixed = TRUE
    )
    expect_error(print(aliased.mod), NA)
    # Check NaN scenarios are handled
    data(phone, package = "flipExampleData")
    phone.weights <- phone$q26
    phone.weights[phone.weights == 99] <- 0
    phone.weights[is.na(phone.weights)] <- 0
    phone$Q5_1 <- as.numeric(phone$Q5_1) - 1
    phone$Q5_2 <- as.numeric(phone$Q5_2) - 1
    phone.subset <- phone$q35 == "male"
    phone.subset[is.na(phone.subset)] <- FALSE

    expect_warning(nan.mod <- Regression(q28 ~ q31 + Q5_1 + Q5_2, data = phone,
                                         subset = phone.subset, weights = phone.weights, interaction = q2))
    nan.caught.warning <- paste0(
        "Possible multicollinearity detected in the data. Consider conducting a relative ",
        "importance analysis by selecting the output to be Relative Importance Analysis or ",
        "Shapley Regression."
    )
    expect_warning(checkVIFAndWarn(nan.mod), nan.caught.warning, fixed = TRUE)
})

test_that("DS-3777 VIF is permissible for polr and svyolr models", {
    set.seed(12321)
    # Low VIF scenario
    X <- MASS::mvrnorm(n = 100, mu = c(1, 5, 6), Sigma = matrix(c(1, 0.4, 0.2,
                                                                  0.4, 1, 0.5,
                                                                  0.2, 0.5, 1),
                                                                nrow = 3, byrow = TRUE))
    cat.var <- sample(letters[1:3], size = 100, replace = TRUE)
    beta <- c(5, 1, 2, 3)
    Y <- cbind(rep(1, nrow(X)), X) %*% beta + rnorm(100, sd = 1)
    y <- factor(cut(Y, breaks = c(-Inf, 25,  35, 40, Inf), labels = LETTERS[1:4]), ordered = TRUE)
    dat <- data.frame(y = y, X = X)
    w <- runif(100L)
    # Regular polr ok
    expect_error(ordered.logit <- Regression(y ~ ., data = dat, type = "Ordered Logit"), NA)
    expect_true(is(ordered.logit$original, "polr"))
    expect_error(vif.table <- vif(ordered.logit), NA)
    expect_true(all(vif.table < 3) && length(vif.table) == 3L)
    # svyolr ok
    expect_error(w.ordered.logit <- Regression(y ~ ., weights = w, data = dat, type = "Ordered Logit"), NA)
    expect_true(is(w.ordered.logit$original, "svyolr"))
    expect_error(w.vif.table <- vif(w.ordered.logit), NA)
    expect_true(all(w.vif.table < 3) && length(w.vif.table) == 3L)
    # Aliased predictor
    dat[["X.2"]] <- dat[["X.1"]]
    expect_warning(ordered.logit <- Regression(y ~ ., data = dat, type = "Ordered Logit"),
                   paste0("The following variable(s) are colinear with other variables and ",
                          "no coefficients have been estimated: 'X.2'"),
                   fixed = TRUE)
    expect_error(car::vif(ordered.logit[["original"]]),
                 "subscript out of bounds")
    expect_error(car::vif(ordered.logit),
                 "Cannot compute VIF when there are aliased predictors in the model")
    # Same for svyolr
    expect_warning(w.ordered.logit <- Regression(y ~ ., weights = w, data = dat, type = "Ordered Logit"),
                   paste0("The following variable(s) are colinear with other variables and ",
                          "no coefficients have been estimated: 'X.2'"),
                   fixed = TRUE)
    expect_error(car::vif(w.ordered.logit),
                 "Cannot compute VIF when there are aliased predictors in the model")
})
