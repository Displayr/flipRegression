context("Automated Outlier Removal")

data(bank, package = "flipExampleData")
bank.formula <- list("Linear" = formula("Overall ~ Fees + Interest + Phone + Branch + Online"))

# coleman data from robustbase

coleman <- structure(list(motherLev = c(6.19,
                                        5.17, 7.04, 7.1, 6.15, 6.41, 6.86, 5.78, 6.51, 5.57, 5.62, 5.34,
                                        5.8, 6.19, 5.62, 6.94, 6.33, 6.01, 7.51, 6.96),
                          Y = c(37.01,
                                26.51, 36.51, 40.7, 37.1, 33.9, 41.8, 33.4, 41.01, 37.2, 23.3,
                                35.2, 34.9, 33.1, 22.7, 39.7, 31.8, 31.7, 43.1, 41.01)),
                     class = "data.frame", row.names = c(NA, -20L))

test_that("Linear method consistent with robustbase", {

    outlier.proportions <- c(0, 0.1, 0.2)
    # Fit robustbase models
    for (outlier.prop in outlier.proportions)
    {
        ltsreg.single.pred.out <- robustbase::ltsReg(Y ~ motherLev, data = coleman, alpha = 1 - outlier.prop)
        regression.single.pred.out <- Regression(Y ~ motherLev, coleman, outlier.proportion = outlier.prop)
        expect_equivalent(ltsreg.single.pred.out$raw.coefficients, regression.single.pred.out$coef)
    }
})

outlier.proportions <- c((0:4)/10, 0.499)

linear.coef <- structure(c(-1.605107, -1.546734, -1.369076, -1.594671, -1.692381,
                           -1.516002, 0.386965, 0.390358, 0.391407, 0.388222, 0.427385,
                           0.408446, 0.283325, 0.28553, 0.281214, 0.279449, 0.25496, 0.267006,
                           0.361998, 0.347979, 0.312275, 0.376358, 0.413116, 0.379259, 0.293546,
                           0.302344, 0.285813, 0.277401, 0.271696, 0.243626, 0.173333, 0.164268,
                           0.173943, 0.177314, 0.160843, 0.17345), .Dim = c(6L, 6L),
                         .Dimnames = list(NULL, c("(Intercept)", "Fees", "Interest", "Phone", "Branch", "Online")))

regression.types <- c("Linear")#, "Binary Logit")
regression.names <- tolower(gsub("-|\\s", "", regression.types))
# regression.types <- c("Linear", "Binary Logit", "Ordered Logit", "Poisson", "Quasi-Poisson",
                      # "NBD", "Multinomial")

miss <- "Exclude cases with missing data"
for (i in seq_along(regression.types))
    test_that(paste0("Coefficient estimates consistent across standard regression: ", regression.types[i]), {
        for (j in seq_along(outlier.proportions))
        {
            regression.out <- suppressWarnings(Regression(bank.formula[[i]], data = bank, type = regression.types[i],
                                                          missing = miss, outlier.proportion = outlier.proportions[j]))
            expected.coefs <- get(paste0(regression.names[i], ".coef"))[j, ]
            expect_equivalent(regression.out$coef, expected.coefs, tolerance = 1e-6)
        }
    })

test_that("Consistent structure with automated outlier removal", {
    basic.linear <- suppressWarnings(Regression(bank.formula[["Linear"]], data = bank))
    # Logical vector of data selected after outlier detection included in outputs and data
    expect_true("non.outlier.data" %in% colnames(basic.linear$estimation.data))
    expect_identical(basic.linear$non.outlier.data, basic.linear$estimation.data$non.outlier.data)
    expect_true(all(basic.linear$non.outlier.data))
    # Check automated output has the anticipated structure
    automated.removal.linear <- suppressWarnings(Regression(bank.formula[["Linear"]], data = bank,
                                                            outlier.proportion = 0.25))
    expect_true("non.outlier.data" %in% colnames(automated.removal.linear$estimation.data))
    expect_identical(automated.removal.linear$non.outlier.data, automated.removal.linear$estimation.data$non.outlier.data)
    expect_false(all(automated.removal.linear$non.outlier.data))
    expect_equal(mean(automated.removal.linear$non.outlier.data), 0.75, tolerance = 1e-2)
    # Outputs not the same and take the expected subsets
    expect_false(identical(automated.removal.linear$coef, basic.linear$coef))
    expect_equal(basic.linear$coef, lm(bank.formula[["Linear"]], data = bank)$coefficients)
    n.data <- nrow(basic.linear$estimation.data)
    computed.subset <- flipRegression:::findNonOutlierObservations(data = basic.linear$estimation.data,
                                                                   model = lm(bank.formula[["Linear"]],
                                                                              data = basic.linear$estimation.data),
                                                                   outlier.proportion = 0.25,
                                                                   type = "Linear")
    expect_equal(computed.subset, automated.removal.linear$non.outlier.data)
    manually.computed.subset <- rank(abs(basic.linear$original$residuals)) <= ceiling(n.data * 0.75)
    expect_equivalent(computed.subset, manually.computed.subset)
    expect_equal(automated.removal.linear$coef, lm(bank.formula[["Linear"]],
                                                   data = basic.linear$estimation.data[computed.subset, ])$coefficients)
})
