context("Automated Outlier Removal")

data(bank, package = "flipExampleData")
small.bank <- na.omit(bank)
small.bank$Overall_Binary <- small.bank$Overall >= 4
bank.formula <- list("Linear" = formula("Overall ~ Fees + Interest + Phone + Branch + Online"),
                     "Binary Logit" = formula("Overall_Binary ~ Phone + Fees"),
                     "Ordered Logit" = formula("Overall ~ Fees + Interest + Phone + Branch + Online"),
                     "Poisson" = formula("Overall ~ Fees + Interest + Phone + Branch + Online"),
                     "Quasi-Poisson" = formula("Overall ~ Fees + Interest + Phone + Branch + Online"),
                     "NBD" = formula("Overall ~ Fees + Interest + Phone + Branch + Online"), "Weighted Ordered Logit" = as.formula("ReducedOverall ~ Fees + Interest + Phone + Branch + Online"))

# Combine highest category for weighted Ordered Logit tests
small.bank$ReducedOverall = small.bank$Overall
small.bank$ReducedOverall[small.bank$ReducedOverall == 7] = 6

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

outlier.proportions <- (0:3)/10

linear.coef <- structure(c(-1.459413, -1.295832, -1.117945, -1.386525, 0.383453,
                           0.382063, 0.375983, 0.39975, 0.287438, 0.279689, 0.25785, 0.272563,
                           0.365195, 0.349545, 0.318893, 0.382782, 0.292158, 0.284359, 0.310889,
                           0.290508, 0.134204, 0.128732, 0.111658, 0.100294), .Dim = c(4L, 6L),
                         .Dimnames = list(NULL, c("(Intercept)", "Fees", "Interest", "Phone", "Branch", "Online")))

linear.coef.with.weight <- structure(c(-1.477443, -1.389282, -1.139199, -1.494176, 0.384225,
                                       0.388478, 0.382247, 0.43022, 0.27378, 0.266267, 0.24448, 0.245963,
                                       0.36295, 0.349076, 0.308568, 0.372647, 0.300361, 0.307878, 0.313785,
                                       0.310015, 0.144519, 0.13632, 0.130055, 0.118276), .Dim = c(4L, 6L),
                                     .Dimnames = list(NULL,
                                                      c("(Intercept)", "Fees", "Interest", "Phone", "Branch", "Online")))
binarylogit.coef <- structure(c(-4.621772, -4.050691, -3.959363, -7.904405, 0.715942,
                                0.608819, 0.755718, 1.51023, 0.782365, 0.841273, 0.805507, 1.755174),
                              .Dim = 4:3, .Dimnames = list(NULL, c("(Intercept)", "Phone", "Fees")))
binarylogit.coef.with.weight <- structure(c(-4.369567, -4.05915, -4.231852, -8.028061, 0.66271,
                                            0.55975, 0.782216, 1.488368, 0.76289, 0.914789, 0.885606, 1.831634),
                                          .Dim = 4:3, .Dimnames = list(NULL, c("(Intercept)", "Phone", "Fees")))


orderedlogit.coef <- structure(c(1.074733, 1.506681, 1.79141, 2.55067, 0.763027, 0.899418,
                                 1.013961, 0.85089, 0.983918, 1.106463, 1.445897, 1.833551, 0.798146,
                                 1.091603, 1.298587, 1.665575, 0.362199, 0.425159, 0.64179, 0.542695,
                                 10.916934, 13.629625, 17.07491, 20.094821, 13.377769, 16.725771,
                                 20.861229, 24.919543, 16.293303, 20.37421, 25.253927, 30.432305,
                                 18.983125, 23.493051, 28.930626, 35.301666, 21.312741, 60.430076,
                                 38.766009, 49.958933), .Dim = c(4L, 10L),
                               .Dimnames = list(NULL, c("Fees", "Interest", "Phone", "Branch", "Online",
                                                        "2|3", "3|4", "4|5", "5|6", "6|7")))
orderedlogit.coef.with.weight <- structure(c(1.093002, 1.508906, 1.960617, 2.696281, 0.734564,
                                             0.784731, 0.847043, 0.839381, 0.990432, 1.093496, 1.522127, 1.936009,
                                             0.835325, 1.059941, 1.364337, 1.728631, 0.398258, 0.494446, 0.634532,
                                             0.580282, 11.136874, 13.403966, 17.392275, 21.083152, 13.633699,
                                             16.432427, 21.283742, 26.096664, 16.590643, 20.110522, 25.756754,
                                             31.742763, 19.273532, 23.392693, 29.813049, 37.52649),
                                           .Dim = c(4L, 9L),
                                           .Dimnames = list(NULL, c("Fees", "Interest", "Phone", "Branch",
                                                                    "Online", "2|3", "3|4", "4|5", "5|6")))

# Test Ordered Logit separately since svyolr is fragile.
miss <- "Exclude cases with missing data"
test_that("Weighted Ordered Logit (svyolr)", {
    expect_error(regression <- Regression(bank.formula[["Ordered Logit"]], data = small.bank, weights = weight,
                                          type = "Ordered Logit", outlier.proportion = 0),
                 NA)
    non.outlier.data <- flipRegression:::findNonOutlierObservations(data = small.bank, outlier.proportion = 0.1,
                                                                    model = regression$original, type = "Ordered Logit",
                                                                    weights = small.bank$weight, seed = 12321)
    expected.error.message <- paste0("Removing outliers has removed all the observations in the outcome variable with",
                                     " level(s): 7. If possible, this issue could be solved by merging the categories ",
                                     "of the outcome variable or reducing the Automated Outlier removal setting.")
    # Expect Overall level 7 to disappear
    expect_equal(table(small.bank$Overall)[6], c(`7` = 1))
    expect_equal(table(small.bank$Overall[non.outlier.data])[6], structure(NA_integer_, .Names = NA_character_))

    expect_error(Regression(bank.formula[["Ordered Logit"]], data = small.bank, weights = weight,
                            type = "Ordered Logit", outlier.proportion = 0.1),
                 expected.error.message, fixed = TRUE)
    # Used combine category data (move level 7 to level 6 for weighted ordered logit) for further tests
    for (j in seq_along(outlier.proportions))
    {
        weighted.regression <- Regression(bank.formula[["Weighted Ordered Logit"]], data = small.bank,
                                          type = "Ordered Logit",weights = weight, missing = miss,
                                          outlier.proportion = outlier.proportions[j])
        expect_equivalent(weighted.regression$coef, orderedlogit.coef.with.weight[j, ], tolerance = 1e-6)
    }
})

regression.types <- c("Linear", "Binary Logit", "Ordered Logit")
regression.names <- tolower(gsub("-|\\s", "", regression.types))
# regression.types <- c("Linear", "Binary Logit", "Ordered Logit", "Poisson", "Quasi-Poisson",
                      # "NBD", "Multinomial")
for (i in seq_along(regression.types))
{
    expected.coefs <- get(paste0(regression.names[i], ".coef"))
    expected.coefs.with.weights <- get(paste0(regression.names[i], ".coef.with.weight"))
    test_that(paste0("Coefficient estimates consistent across standard regression: ", regression.types[i]), {
        for (j in seq_along(outlier.proportions))
        {
            regression <- Regression(bank.formula[[i]], data = small.bank, type = regression.types[i],
                                     missing = miss, outlier.proportion = outlier.proportions[j])
            expect_equivalent(regression$coef, expected.coefs[j, ], tolerance = 1e-6)
            if (regression.types[i] == "Ordered Logit")
                next
            else
            {
                weighted.regression <- Regression(bank.formula[[i]], data = small.bank, type = regression.types[i],
                                                  weights = weight, missing = miss,
                                                  outlier.proportion = outlier.proportions[j])

                expect_equivalent(weighted.regression$coef, expected.coefs.with.weights[j, ], tolerance = 1e-6)
            }
        }
    })
}

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
                                                                   type = "Linear",
                                                                   weights = NULL)
    expect_equal(computed.subset, automated.removal.linear$non.outlier.data)
    manually.computed.subset <- rank(abs(basic.linear$original$residuals)) <= ceiling(n.data * 0.75)
    expect_equivalent(computed.subset, manually.computed.subset)
    expect_equal(automated.removal.linear$coef, lm(bank.formula[["Linear"]],
                                                   data = basic.linear$estimation.data[computed.subset, ])$coefficients)
})
