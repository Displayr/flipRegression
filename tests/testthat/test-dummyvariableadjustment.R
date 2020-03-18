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
    dummy.adjusted.coefs <- extractDummyAdjustedCoefs(all.coefs, computed.means)

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
})

test_that("Robust SE compatible with Dummy variable adjustment", {
    ill.conditioned.message <- "There is a technical problem with the parameter variance-covariance matrix"
    expect_warning(robust.dummy.regression <- Regression(Y ~ ., data = missing.data,
                                                         missing = "Dummy variable adjustment",
                                                         robust.se = TRUE),
                   NA)
    expect_warning(print(robust.dummy.regression), "Unusual observations detected")
    # Fall back to the non-influence adjusted HCCM when the influence is not numerically viable.
    missing.data <- data.frame(Y, X)
    missing.data$X1[1] <- NA
    expect_warning(robust.dummy.regression <- Regression(Y ~ ., data = missing.data,
                                                         missing = "Dummy variable adjustment",
                                                         robust.se = TRUE),
                   ill.conditioned.message)
    expect_warning(print(robust.dummy.regression), "Unusual observations detected")
    # Fall back to the non-influence adjusted HCCM when the influence is not numerically viable.
    # Expect issues with the variance-covariance matrix but it can still be computed.
    missing.data <- data.frame(Y, X)
    missing.data$X1[1] <- NA
    missing.data$X2[2] <- NA
    expect_warning(robust.dummy.regression <- Regression(Y ~ ., data = missing.data,
                                                         missing = "Dummy variable adjustment",
                                                         robust.se = TRUE),
                   ill.conditioned.message)
    expect_warning(print(robust.dummy.regression), "Unusual observations detected")
    missing.data <- data.frame(Y, X)
    missing.data$X1[1] <- NA
    missing.data$X2[1:2] <- NA
    expect_warning(robust.dummy.regression <- Regression(Y ~ ., data = missing.data,
                                                         missing = "Dummy variable adjustment",
                                                         robust.se = TRUE),
                   ill.conditioned.message)
    expect_warning(print(robust.dummy.regression), "Unusual observations detected")
    # No warning when data is fine.
    missing.data <- data.frame(Y, X)
    missing.data$X1[c(1, 3)] <- NA
    expect_warning(robust.dummy.regression <- Regression(Y ~ ., data = missing.data,
                                                         missing = "Dummy variable adjustment",
                                                         robust.se = TRUE),
                   NA)
    expect_warning(print(robust.dummy.regression), "Unusual observations detected")
})


test_that("RIA and Shapley edge case", {
    dat <- not.missing.data
    dat[sample(c(TRUE, FALSE), size = nrow(dat), replace = TRUE, prob = c(1, 4)), -1] <- NA
    expect_error(Regression(Y ~ X1 + X2 + X3, data = dat, type = "Linear", output = "Shapley Regression"),
                 NA)
})
