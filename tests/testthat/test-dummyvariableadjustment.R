context("Dummy variable adjustment")

# Simulate correlated predictors
X <- MASS::mvrnorm(n = 200, mu = rep(0, 3), Sigma = matrix(c(1, 0.2, 0.3,
                                                             0.2, 1, 0.2,
                                                             0.3, 0.2, 1), ncol = 3))
beta <- c(0.4, 0.3, 0.25)
r2 <- 0.30

Y <- X %*% beta + rnorm(n = 200)

not.missing.data <- data.frame(Y, X)

missing.data <- data.frame(lapply(not.missing.data, function(x) {
    missing <- sample(c(TRUE, FALSE), size = nrow(X), replace = TRUE, prob = c(1, 4))
    x[missing] <- NA
    x
}))

missing.data <- data.frame(missing.data)

test_that("Coefficient estimates are the same ", {
    # Remove Y values with missing
    dummy.regression <- Regression(Y ~ ., data = missing.data, missing = "Dummy variable adjustment")
    all.coefs <- dummy.regression$coef

    computed.means <- lapply(missing.data, mean, na.rm = TRUE)

    # Check extracted Coefs correct, they will be the last three of the regular coefficients
    dummy.adjusted.coefs <- extractImputedValuesFromDummyAdjustments(all.coefs, computed.means)

    expect_equal(names(dummy.adjusted.coefs), c("X1", "X2", "X3"))

    # dummy adjusted coefficients should be dummy coefficients divided by regular slope coefficient
    # Ignore the first computed mean, thats Y
    expected.coefs <- list(X1 = unname(computed.means[[2]] + all.coefs[5]/all.coefs[2]),
                           X2 = unname(computed.means[[3]] + all.coefs[6]/all.coefs[3]),
                           X3 = unname(computed.means[[4]] + all.coefs[7]/all.coefs[4]))
    expect_identical(dummy.adjusted.coefs, expected.coefs)

    # Check remapped matrix correct
    remapped.data <- flipRegression:::adjustDataMissingDummy(missing.data, dummy.regression$original,
                                                             estimation.data = dummy.regression$estimation.data)

    # Create corerct remapped data
    missing.data.processed <- missing.data
    # Remove missing response cases
    non.missing.outcomes <- which(!is.na(missing.data[[1]]))
    missing.data.processed <- missing.data.processed[non.missing.outcomes, ]
    # Check all predictors missing
    if (any(all.missing.predictors <- apply(missing.data.processed[-1], 1, function(x) all(is.na(x)))))
        missing.data.processed <- missing.data.processed[!all.missing.predictors, ]
    processed.row.names <- row.names(missing.data.processed)
    missing.data.processed <- lapply(names(missing.data.processed), function(x) {
        if (x == "Y")
            return(missing.data.processed[[x]])
        x.col <- missing.data.processed[[x]]
        replace.val <- dummy.adjusted.coefs[[x]]
        x.col[is.na(x.col)] <- replace.val
        x.col
    })
    names(missing.data.processed) <- names(missing.data)
    missing.data.processed <- data.frame(missing.data.processed)
    row.names(missing.data.processed) <- processed.row.names
    missing.data.processed$non.outlier.data_GQ9KqD7YOf <- TRUE

    # Data is the same
    expect_equal(missing.data.processed, remapped.data, check.attributes = FALSE)

    # Expect coefficients to be the same between original dummy adjusted regression and
    # on the remapped dataset.
    expect_equal(lm(Y ~ X1 + X2 + X3, data = remapped.data)$coef, all.coefs[-(5:7)])

    # Aliased dummy variable predictors are handled
    coefficients.with.dummy.aliased <- c(
        `(Intercept)` = 0.5,
        X1 = 0.4,
        X2 = 0.3,
        X1.dummy.var_GQ9KqD7YOf = NA,
        X2.dummy.var_GQ9KqD7YOf = 0.6
    )
    computed.means <- c(X1 = 1, X2 = 2)
    extractImputedValuesFromDummyAdjustments(coefficients.with.dummy.aliased, computed.means) |>
        expect_equal(list(X1 = 1, X2 = 2 + 0.6 / 0.3))
})

test_that("Robust SE compatible with Dummy variable adjustment", {
    # Prevent pop-ups
    mockery::stub(print.Regression, "print.htmlwidget", NULL)
    ill.conditioned.message <- "There is a technical problem with the parameter variance-covariance matrix"
    expect_warning(robust.dummy.regression <- Regression(Y ~ ., data = missing.data,
                                                         missing = "Dummy variable adjustment",
                                                         robust.se = TRUE),
                   NA)
    # Due to the random input data, sometimes the print function will detect outliers (cooks distance)
    # and throw a warning. Other times it will not detect outliers and no warning. Code below captures
    # any warnings and checks if there is a single warning or no warning.
    print.output <- capture_warnings(print(robust.dummy.regression))
    expect_true(length(print.output) == 0 || grepl("Unusual observations detected", print.output))
    # Fall back to the non-influence adjusted HCCM when the influence is not numerically viable.
    missing.data <- data.frame(Y, X)
    missing.data$X1[1] <- NA
    expect_warning(robust.dummy.regression <- Regression(Y ~ ., data = missing.data,
                                                         missing = "Dummy variable adjustment",
                                                         robust.se = TRUE),
                   ill.conditioned.message)
    print.output <- capture_warnings(print(robust.dummy.regression))
    expect_true(length(print.output) == 0 || grepl("Unusual observations detected", print.output))
    # Fall back to the non-influence adjusted HCCM when the influence is not numerically viable.
    # Expect issues with the variance-covariance matrix but it can still be computed.
    missing.data <- data.frame(Y, X)
    missing.data$X1[1] <- NA
    missing.data$X2[2] <- NA
    expect_warning(robust.dummy.regression <- Regression(Y ~ ., data = missing.data,
                                                         missing = "Dummy variable adjustment",
                                                         robust.se = TRUE),
                   ill.conditioned.message)
    print.output <- capture_warnings(print(robust.dummy.regression))
    expect_true(length(print.output) == 0 || grepl("Unusual observations detected", print.output))
    missing.data <- data.frame(Y, X)
    missing.data$X1[1] <- NA
    missing.data$X2[1:2] <- NA
    expect_warning(robust.dummy.regression <- Regression(Y ~ ., data = missing.data,
                                                         missing = "Dummy variable adjustment",
                                                         robust.se = TRUE),
                   ill.conditioned.message)
    print.output <- capture_warnings(print(robust.dummy.regression))
    expect_true(length(print.output) == 0 || grepl("Unusual observations detected", print.output))
    # No warning when data is fine.
    missing.data <- data.frame(Y, X)
    missing.data$X1[c(1, 3)] <- NA
    expect_warning(robust.dummy.regression <- Regression(Y ~ ., data = missing.data,
                                                         missing = "Dummy variable adjustment",
                                                         robust.se = TRUE),
                   NA)
    print.output <- capture_warnings(print(robust.dummy.regression))
    expect_true(length(print.output) == 0 || grepl("Unusual observations detected", print.output))
})


test_that("RIA and Shapley edge case", {
    dat <- not.missing.data
    dat[sample(c(TRUE, FALSE), size = nrow(dat), replace = TRUE, prob = c(1, 4)), -1] <- NA
    expect_error(Regression(Y ~ X1 + X2 + X3, data = dat, type = "Linear", output = "Shapley Regression"),
                 NA)
})

test_that("DS-2952: Dummy variable adjustment and Ordered Logit printable in RIA", {
    data("bank", package = "flipExampleData")
    expect_error(model <- Regression(Overall ~ Fees, data = bank,
                                     missing = "Dummy variable adjustment",
                                     type = "Ordered Logit",
                                     output = "Relative Importance Analysis"),
                 NA)
    # Prevent pop-ups
    mockery::stub(print.Regression, "print.htmlwidget", NULL)
    expect_error(print(model), NA)
})

test_that("DS-2986: Aliased Dummy variables", {
    X <- MASS::mvrnorm(n = 200, mu = rep(0, 6), Sigma = matrix(c(1, 0.2, 0.3, 0.2, 0.3, 0.4,
                                                                 0.2, 1, 0.2, 0.3, 0.4, 0.5,
                                                                 0.3, 0.2, 1, 0.4, 0.5, 0.2,
                                                                 0.2, 0.3, 0.4, 1, 0.2, 0.3,
                                                                 0.3, 0.4, 0.5, 0.2, 1, 0.1,
                                                                 0.4, 0.5, 0.2, 0.3, 0.1, 1), ncol = 6))
    beta <- c(0.4, 0.3, 0.25, 0.3, 0.2, 0.1)
    r2 <- 0.30

    Y <- X %*% beta + rnorm(n = 200)

    not.missing.data <- data.frame(Y, X)
    common.indices <- sample(c(TRUE, FALSE), size = nrow(X), replace = TRUE, prob = c(1, 4))
    one.group.aliased <- data.frame(lapply(1:ncol(not.missing.data), function(x) {
        y <- not.missing.data[[x]]
        if (x %in% 2:3) # Ignore Y and last X
            y[common.indices] <- NA
        y
    }))
    second.common <- sample(c(TRUE, FALSE), size = nrow(X), replace = TRUE, prob = c(1, 4))
    # Ensure simulated values are different
    while(identical(second.common, common.indices))
        second.comoon <- sample(c(TRUE, FALSE), size = nrow(X), replace = TRUE, prob = c(1, 4))
    two.groups.aliased <- data.frame(lapply(1:ncol(not.missing.data), function(x) {
        y <- not.missing.data[[x]]
        if (x %in% 2:3) # Ignore Y and last X
            y[common.indices] <- NA
        else if (x %in% 4:5)
            y[second.common] <- NA
        y
    }))
    third.common <- sample(c(TRUE, FALSE), size = nrow(X), replace = TRUE, prob = c(1, 4))
    # Ensure simulated values are different
    while(identical(third.common, common.indices) || identical(third.common, second.common))
        third.common <- sample(c(TRUE, FALSE), size = nrow(X), replace = TRUE, prob = c(1, 4))
    three.groups.aliased <- data.frame(lapply(1:ncol(not.missing.data), function(x) {
        y <- not.missing.data[[x]]
        if (x %in% 2:3) # Ignore Y and last X
            y[common.indices] <- NA
        else if (x %in% 4:5)
            y[second.common] <- NA
        else if (x %in% 6:7)
            y[third.common] <- NA
        y
    }))
    names(one.group.aliased) <- names(two.groups.aliased) <- names(three.groups.aliased) <- names(not.missing.data)
    expect_warning(Regression(Y ~ X1 + X2 + X3, data = one.group.aliased,
                              missing = "Dummy variable adjustment"),
                   paste0("The predictor variables: (X1; X2) have exactly the same cases with missing values. ",
                          "Consequently, only a single dummy variable was used to adjust the ",
                          "data for these predictors. The dummy variables would be aliased if ",
                          "each predictor in this group had its own dummy variable."),
                   fixed = TRUE)
    labelled.one.group <- one.group.aliased
    attr(labelled.one.group$X1, "label") <- "Apples"
    attr(labelled.one.group$X2, "label") <- "Oranges"
    expect_warning(Regression(Y ~ X1 + X2 + X3, data = labelled.one.group, show.labels = TRUE,
                              missing = "Dummy variable adjustment"),
                   paste0("The predictor variables: (Apples; Oranges) have exactly the same cases with missing values. ",
                          "Consequently, only a single dummy variable was used to adjust the ",
                          "data for these predictors. The dummy variables would be aliased if ",
                          "each predictor in this group had its own dummy variable."),
                   fixed = TRUE)
    expect_warning(Regression(Y ~ X1 + X2 + X3 + X4, data = two.groups.aliased,
                              missing = "Dummy variable adjustment"),
                   paste0("Some groups of predictors have exactly the same cases with missing ",
                          "values and consequently, only a single dummy variable was used to ",
                          "adjust the data for each group. Group 1 (X1; X2) and Group 2 (X3; X4). ",
                          "The dummy variables would be aliased if each predictor in each group ",
                          "had its own dummy variable."),
                   fixed = TRUE)
    labelled.two.groups <- two.groups.aliased
    attr(labelled.two.groups$X1, "label") <- "Apples"
    attr(labelled.two.groups$X3, "label") <- "Oranges"
    expect_warning(Regression(Y ~ X1 + X2 + X3 + X4, data = labelled.two.groups, show.labels = TRUE,
                              missing = "Dummy variable adjustment"),
                   paste0("Some groups of predictors have exactly the same cases with missing ",
                          "values and consequently, only a single dummy variable was used to ",
                          "adjust the data for each group. Group 1 (Apples; X2) and Group 2 ",
                          "(Oranges; X4). The dummy variables would be aliased if each predictor ",
                          "in each group had its own dummy variable."),
                   fixed = TRUE)
    expect_warning(Regression(Y ~ X1 + X2 + X3 + X4 + X5 + X6, data = three.groups.aliased,
                              missing = "Dummy variable adjustment"),
                   paste0("Some groups of predictors have exactly the same cases with missing ",
                          "values and consequently, only a single dummy variable was used to ",
                          "adjust the data for each group. Group 1 (X1; X2), Group 2 (X3; X4) and ",
                          "Group 3 (X5; X6). The dummy variables would be aliased if each ",
                          "predictor in each group had its own dummy variable."),
                   fixed = TRUE)
    labelled.three.groups <- three.groups.aliased
    attr(labelled.three.groups$X1, "label") <- "Apples"
    attr(labelled.three.groups$X3, "label") <- "Oranges"
    attr(labelled.three.groups$X5, "label") <- "Grapes"
    expect_warning(Regression(Y ~ X1 + X2 + X3 + X4 + X5 + X6, data = labelled.three.groups,
                              show.labels = TRUE, missing = "Dummy variable adjustment"),
                   paste0("Some groups of predictors have exactly the same cases with missing ",
                          "values and consequently, only a single dummy variable was used to ",
                          "adjust the data for each group. Group 1 (Apples; X2), Group 2 (Oranges; ",
                          "X4) and Group 3 (Grapes; X6). The dummy variables would be aliased if ",
                          "each predictor in each group had its own dummy variable."),
                   fixed = TRUE)
    mix.aliased <- data.frame(lapply(1:ncol(not.missing.data), function(x) {
        y <- not.missing.data[[x]]
        if (x %in% 2:3) # Create a single group
            y[common.indices] <- NA
        else if (x %in% 4) # Create a single dummy variable not in the group
            y[second.common] <- NA
        y
    }))
    # Test that single lone non-aliased dummy is not captured when group of aliased dummys exist
    names(mix.aliased) <- names(one.group.aliased)
    expect_warning(Regression(Y ~ X1 + X2 + X3, data = mix.aliased, show.labels = TRUE,
                              missing = "Dummy variable adjustment"),
                   paste0("The predictor variables: (X1; X2) have exactly the same cases with missing values. ",
                          "Consequently, only a single dummy variable was used to adjust the ",
                          "data for these predictors. The dummy variables would be aliased if ",
                          "each predictor in this group had its own dummy variable."),
                   fixed = TRUE)
})
