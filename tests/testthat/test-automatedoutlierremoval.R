context("Automated Outlier Removal")

data(bank, package = "flipExampleData")
small.bank <- na.omit(bank)
# Combine highest category for weighted Ordered Logit tests
small.bank <- transform(small.bank,
                        Overall_Binary = factor(as.numeric(Overall >= 4)),
                        ReducedOverall = pmin(Overall, 6))
bank.formula <- list(Linear = Overall ~ Fees + Interest + Phone + Branch + Online,
                     `Binary Logit` = Overall_Binary ~ Phone + Fees,
                     `Ordered Logit` = Overall ~ Fees + Interest + Phone + Branch + Online,
                     Poisson = Overall ~ Fees + Interest + Phone + Branch + Online,
                     `Quasi-Poisson` = Overall ~ Fees + Interest + Phone + Branch + Online,
                     NBD = Overall ~ Fees + Interest + Phone + Branch + Online,
                     `Weighted Ordered Logit` = ReducedOverall ~ Fees + Interest + Phone + Branch + Online,
                     `Multinomial Logit` = Overall ~ Fees + Interest + Phone + Branch + Online)

# coleman data from robustbase

coleman <- data.frame(
    motherLev = c(6.19, 5.17, 7.04, 7.1, 6.15, 6.41, 6.86, 5.78, 6.51, 5.57,
                  5.62, 5.34, 5.8, 6.19, 5.62, 6.94, 6.33, 6.01, 7.51, 6.96),
    Y = c(37.01, 26.51, 36.51, 40.7, 37.1, 33.9, 41.8, 33.4, 41.01, 37.2, 23.3,
          35.2, 34.9, 33.1, 22.7, 39.7, 31.8, 31.7, 43.1, 41.01)
)

test_that("Linear method consistent with robustbase", {

    outlier.proportions <- c(0, 0.1, 0.2)
    # Fit robustbase models
    for (outlier.prop in outlier.proportions)
    {
        ltsreg.single.pred.out <- robustbase::ltsReg(Y ~ motherLev, data = coleman, alpha = 1 - outlier.prop)
        regression.single.pred.out <- Regression(Y ~ motherLev, coleman, outlier.prop.to.remove = outlier.prop)
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
                                          type = "Ordered Logit", outlier.prop.to.remove = 0),
                 NA)
    non.outlier.data <- findNonOutlierObservations(model = regression$original,
                                                   outlier.prop.to.remove = 0.1,
                                                   seed = 12321)
    expected.error.message <- paste0("Removing outliers has removed all the observations in the outcome variable with",
                                     " level(s): 7. If possible, this issue could be solved by merging the categories ",
                                     "of the outcome variable or reducing the Automated Outlier removal setting.")
    # Expect Overall level 7 to disappear
    expect_equal(table(small.bank$Overall)[6], c(`7` = 1))
    expect_equal(table(small.bank$Overall[non.outlier.data])[6], structure(NA_integer_, .Names = NA_character_))

    expect_error(Regression(bank.formula[["Ordered Logit"]], data = small.bank, weights = weight,
                            type = "Ordered Logit", outlier.prop.to.remove = 0.1),
                 expected.error.message, fixed = TRUE)
    # Used combine category data (move level 7 to level 6 for weighted ordered logit) for further tests
    for (j in seq_along(outlier.proportions))
    {
        weighted.regression <- Regression(bank.formula[["Weighted Ordered Logit"]], data = small.bank,
                                          type = "Ordered Logit", weights = weight, missing = miss,
                                          outlier.prop.to.remove = outlier.proportions[j])
        expect_equal(mean(weighted.regression$estimation.data$non.outlier.data_GQ9KqD7YOf),
                     1 - outlier.proportions[j], tolerance = 1e-2)
        expect_equivalent(weighted.regression$coef, orderedlogit.coef.with.weight[j, ], tolerance = 1e-6)
    }
})

poisson.coef <- structure(c(-0.033102, 0.054263, 0.08203, -0.006858, 0.09939,
                            0.102068, 0.08594, 0.098248, 0.072796, 0.067073, 0.069967, 0.067454,
                            0.095385, 0.087733, 0.081678, 0.098152, 0.07608, 0.07944, 0.081667,
                            0.077377, 0.033132, 0.021127, 0.02856, 0.030338),
                          .Dim = c(4L,6L),
                          .Dimnames = list(NULL, c("(Intercept)", "Fees", "Interest", "Phone", "Branch", "Online")))

poisson.coef.with.weight <- structure(c(-0.038808, 0.030415, 0.052372, -0.031036, 0.099405,
                                       0.099918, 0.093745, 0.105088, 0.069434, 0.064036, 0.067405, 0.066951,
                                       0.094989, 0.086389, 0.079522, 0.094368, 0.077888, 0.086115, 0.081464,
                                       0.076753, 0.036209, 0.026473, 0.03302, 0.034317),
                                     .Dim = c(4L, 6L),
                                     .Dimnames = list(NULL, c("(Intercept)", "Fees", "Interest", "Phone", "Branch", "Online")))

quasipoisson.coef <- structure(c(-0.033102, 0.054263, 0.08203, -0.006858, 0.09939,
                                 0.102068, 0.08594, 0.098248, 0.072796, 0.067073, 0.069967, 0.067454,
                                 0.095385, 0.087733, 0.081678, 0.098152, 0.07608, 0.07944, 0.081667,
                                 0.077377, 0.033132, 0.021127, 0.02856, 0.030338),
                               .Dim = c(4L,6L),
                               .Dimnames = list(NULL, c("(Intercept)", "Fees", "Interest", "Phone", "Branch", "Online")))

quasipoisson.coef.with.weight <- structure(c(-0.038808, 0.030415, 0.052372, -0.031036, 0.099405,
                                             0.099918, 0.093745, 0.105088, 0.069434, 0.064036, 0.067405, 0.066951,
                                             0.094989, 0.086389, 0.079522, 0.094368, 0.077888, 0.086115, 0.081464,
                                             0.076753, 0.036209, 0.026473, 0.03302, 0.034317),
                                           .Dim = c(4L, 6L),
                                           .Dimnames = list(NULL, c("(Intercept)", "Fees", "Interest", "Phone", "Branch", "Online")))

nbd.coef <- structure(c(-0.033102, 0.054263, 0.08203, -0.006858, 0.09939,
                        0.102068, 0.08594, 0.098248, 0.072796, 0.067073, 0.069967, 0.067454,
                        0.095385, 0.087733, 0.081678, 0.098152, 0.07608, 0.07944, 0.081667,
                        0.077377, 0.033132, 0.021127, 0.02856, 0.030338), .Dim = c(4L, 6L),
                      .Dimnames = list(NULL, c("(Intercept)", "Fees", "Interest", "Phone", "Branch", "Online")))

nbd.coef.with.weight <- structure(c(-0.038809, 0.021439, 0.083738, 0.005239, 0.099405,
                                    0.103246, 0.104868, 0.103977, 0.069434, 0.061136, 0.057542, 0.065668,
                                    0.094989, 0.088558, 0.084856, 0.094597, 0.077888, 0.080988, 0.078403,
                                    0.077274, 0.036209, 0.03073, 0.023232, 0.02616), .Dim = c(4L, 6L),
                                  .Dimnames = list(NULL, c("(Intercept)", "Fees", "Interest", "Phone", "Branch", "Online")))

regression.types <- c("Linear", "Binary Logit", "Ordered Logit", "Poisson", "Quasi-Poisson", "NBD")
regression.names <- tolower(gsub("-|\\s", "", regression.types))

miss <- "Exclude cases with missing data"

tt <- "Multinomial Logit"
expected.multinomial.warning <- "Automated outlier removal and re-fitting a 'Multinomial Logit' model is not supported"
test_that("Multinomial Logit", {
    expect_error(regression <- Regression(bank.formula[[tt]], data = small.bank, type = tt,
                                          missing = miss, outlier.prop.to.remove = 0),
                 NA)
    # Prevent pop-ups
    mockery::stub(print.Regression, "print.htmlwidget", NULL)
    expect_error(print(regression), NA)
    expect_error(regression.with.weight <- Regression(bank.formula[[tt]], data = small.bank, type = tt,
                                                      missing = miss, outlier.prop.to.remove = 0, weights = weight),
                 NA)
    # Prevent pop-ups
    mockery::stub(print.Regression, "print.htmlwidget", NULL)
    expect_error(print(regression.with.weight), NA)
    # Expect errors for outlier proportion requested
    expect_warning(Regression(bank.formula[[tt]], data = small.bank, type = tt,
                              missing = miss, outlier.prop.to.remove = 0.1),
                   expected.multinomial.warning)
    expect_warning(Regression(bank.formula[[tt]], data = small.bank, type = tt,
                              missing = miss, outlier.prop.to.remove = 0.1, weights = weight),
                   expected.multinomial.warning)
})

for (i in seq_along(regression.types))
{
    expected.coefs <- get(paste0(regression.names[i], ".coef"))
    expected.coefs.with.weights <- get(paste0(regression.names[i], ".coef.with.weight"))
    expected.warning <- if (regression.types[i] == "NBD") "Model may not have converged" else NA
    test_that(paste0("Coefficient estimates consistent across standard regression: ", regression.types[i]), {
        for (j in seq_along(outlier.proportions))
        {
            expect_warning(regression <- Regression(bank.formula[[i]], data = small.bank, type = regression.types[i],
                                                    missing = miss, outlier.prop.to.remove = outlier.proportions[j]),
                           expected.warning)
            expect_equal(mean(regression$estimation.data$non.outlier.data_GQ9KqD7YOf),
                         1 - outlier.proportions[j], tolerance = 1e-2)
            expect_equivalent(regression$coef, expected.coefs[j, ], tolerance = 1e-6)
            if (regression.types[i] == "Ordered Logit")
                next
            else
            {
                expect_warning(weighted.regression <- Regression(bank.formula[[i]], data = small.bank, type = regression.types[i],
                                                                 weights = weight, missing = miss,
                                                                 outlier.prop.to.remove = outlier.proportions[j]),
                               expected.warning)
                expect_equal(mean(weighted.regression$estimation.data$non.outlier.data_GQ9KqD7YOf),
                             1 - outlier.proportions[j], tolerance = 1e-2)
                expect_equivalent(weighted.regression$coef, expected.coefs.with.weights[j, ], tolerance = 1e-6)
            }
        }
    })
}

test_that("Consistent structure with automated outlier removal", {
    proportion <- 0.25
    basic.linear <- Regression(bank.formula[["Linear"]], data = small.bank)
    # Logical vector of data selected after outlier detection included in outputs and data
    expect_true("non.outlier.data_GQ9KqD7YOf" %in% colnames(basic.linear$estimation.data))
    expect_identical(basic.linear$non.outlier.data,
                     basic.linear$estimation.data$non.outlier.data_GQ9KqD7YOf)
    expect_true(all(basic.linear$non.outlier.data_GQ9KqD7YOf))
    # Check automated output has the anticipated structure
    automated.removal.linear <- Regression(bank.formula[["Linear"]],
                                           data = small.bank,
                                           outlier.prop.to.remove = proportion)
    expect_true("non.outlier.data_GQ9KqD7YOf" %in% colnames(automated.removal.linear$estimation.data))
    expect_identical(automated.removal.linear$non.outlier.data,
                     automated.removal.linear$estimation.data$non.outlier.data_GQ9KqD7YOf)
    expect_false(all(automated.removal.linear$non.outlier.data))
    expect_equal(mean(automated.removal.linear$non.outlier.data), 1 - proportion, tolerance = 1e-2)
    # Outputs not the same and take the expected subsets
    expect_false(identical(automated.removal.linear$coef, basic.linear$coef))
    expect_equal(basic.linear$coef, lm(bank.formula[["Linear"]], data = small.bank)$coefficients)
    n.data <- nrow(basic.linear$estimation.data)
    computed.subset <- findNonOutlierObservations(model = lm(bank.formula[["Linear"]],
                                                             data = basic.linear$estimation.data),
                                                  outlier.prop.to.remove = proportion)
    expect_equal(computed.subset, automated.removal.linear$non.outlier.data)
    manually.computed.subset <- rank(abs(basic.linear$original$residuals)) <= ceiling(n.data * (1 - proportion))
    expect_equivalent(computed.subset, manually.computed.subset)
    filtered.data <- basic.linear$estimation.data[computed.subset, ]
    expect_equal(automated.removal.linear[["coef"]],
                 lm(bank.formula[["Linear"]], data = filtered.data)[["coefficients"]])
})

test_that("Relative Importance and Shapley Output", {
    for (tt in regression.types)
    {
        expected.warning <- if (tt == "NBD") "Model may not have converged" else NA
        # Determine subset for 10% outlier removal.
        expect_warning(regression.non.outlier.data <- Regression(bank.formula[[tt]], data = small.bank, type = tt,
                                                                 outlier.prop.to.remove = 0.1)$non.outlier.data,
                       expected.warning)
        expect_equal(mean(regression.non.outlier.data), 0.9, tolerance = 1e-2)
        # Check importance output is the same on auto removed and subsetted data
        expect_warning(importance.with.removal <- Regression(bank.formula[[tt]], data = small.bank, type = tt,
                                                             outlier.prop.to.remove = 0.1,
                                                             output = "Relative Importance Analysis")$importance,
                         expected.warning)
        expect_warning(importance.on.subset <- Regression(bank.formula[[tt]], type = tt,
                                                          data = small.bank[regression.non.outlier.data, ],
                                                          outlier.prop.to.remove = 0,
                                                          output = "Relative Importance Analysis")$importance,
                         expected.warning)
        expect_equal(importance.with.removal, importance.on.subset)
        # Same for weighted make adjustments for Ordered Logit to avoid error
        ttw <- if (tt == "Ordered Logit") "Weighted Ordered Logit" else tt
        expect_warning(weighted.regression.non.outlier.data <- Regression(bank.formula[[ttw]], data = small.bank,
                                                                          type = tt, weights = weight,
                                                                          outlier.prop.to.remove = 0.1)$non.outlier.data,
                         expected.warning)
        expect_equal(mean(weighted.regression.non.outlier.data), 0.9, tolerance = 1e-2)
        expect_warning(weighted.importance.with.removal <- Regression(bank.formula[[ttw]], data = small.bank, type = tt,
                                                                        weights = weight,
                                                                        outlier.prop.to.remove = 0.1,
                                                                        output = "Relative Importance Analysis")$importance,
                         expected.warning)
        expect_warning(weighted.importance.on.subset <- Regression(bank.formula[[ttw]], type = tt,
                                                                     data = small.bank[weighted.regression.non.outlier.data, ],
                                                                     outlier.prop.to.remove = 0,
                                                                     weights = weight,
                                                                     output = "Relative Importance Analysis")$importance,
                         expected.warning)
        expect_equal(weighted.importance.with.removal, weighted.importance.on.subset)
        # Check Shapley output is the same on auto removed and subsetted data
        if (tt == "Linear")
        {
            shapley.with.removal <- Regression(bank.formula[[tt]], data = small.bank, type = tt,
                                               outlier.prop.to.remove = 0.1,
                                               output = "Shapley Regression")$importance
            shapley.on.subset <- Regression(bank.formula[[tt]], type = tt,
                                            data = small.bank[regression.non.outlier.data, ],
                                            outlier.prop.to.remove = 0,
                                            output = "Shapley Regression")$importance
            expect_equal(shapley.with.removal[["non.outlier.n"]], 198L)
            expect_equal(shapley.with.removal[names(shapley.with.removal) != "non.outlier.n"],
                         shapley.on.subset)
            weighted.shapley.with.removal <- Regression(bank.formula[[tt]], data = small.bank, type = tt,
                                                        weights = weight,
                                                        outlier.prop.to.remove = 0.1,
                                                        output = "Shapley Regression")$importance
            weighted.shapley.on.subset <- Regression(bank.formula[[tt]], type = tt,
                                                     data = small.bank[weighted.regression.non.outlier.data, ],
                                                     outlier.prop.to.remove = 0,
                                                     weights = weight,
                                                     output = "Shapley Regression")$importance
            expect_equal(weighted.shapley.with.removal[["non.outlier.n"]], 198L)
            expect_equal(weighted.shapley.with.removal[names(weighted.shapley.with.removal) != "non.outlier.n"],
                         weighted.shapley.on.subset)
        }
    }
})

test_that("Correct n.estimation used with outlier removal and checks", {
    z.0 <- Regression(Overall ~ Fees + ATM, data = bank, outlier.prop.to.remove = 0)
    expect_equal(z.0$sample.description,
                 paste0("n = 686 cases used in estimation of a total sample size of 896; ",
                        "cases containing missing values have been excluded;"))
    # Prevent pop-ups
    mockery::stub(print.Regression, "print.htmlwidget", NULL)
    expect_warning(print(z.0),
                   paste0("Unusual observations detected. Consider re-running the analysis using ",
                          "automated outlier removal with a non-zero setting to automatically remove unusual ",
                          "observations that can affect the final Regression model. The largest hat value is ",
                          "0.0252, which is higher than the threshhold of 0.0117 = 2 * (k + 1) / n"),
                   fixed = TRUE)
    z.10 <- Regression(Overall ~ Fees + ATM, data = bank, outlier.prop.to.remove = 0.1)
    expect_equal(z.10$sample.description,
                 paste0("n = 618 cases used in estimation of a total sample size of 896; ",
                        "cases containing missing values have been excluded;"))
    expect_warning(print(z.10),
                   paste0("Unusual observations detected. After removing a proportion of the data from the ",
                          "analysis, unusual observations exist in the data. Recommend inspecting the model ",
                          "diagnostics and possibly increasing the automatic outlier removal if necessary. ",
                          "The largest hat value is 0.0286, which is higher than the threshhold of 0.0129 ",
                          "= 2 * (k + 1) / n"),
                   fixed = TRUE)
    # Expect outlier removal not to conflict with missing data description
    complete.cases.bank <- bank[complete.cases(bank), ]
    z.0 <- Regression(Overall ~ Fees + ATM, data = complete.cases.bank, outlier.prop.to.remove = 0)
    expect_equal(z.0$sample.description, "n = 244 cases used in estimation;")
    z.10 <- Regression(Overall ~ Fees + ATM, data = complete.cases.bank, outlier.prop.to.remove = 0.1)
    expect_equal(z.10$sample.description, "n = 220 cases used in estimation of a total sample size of 244;")
    complete.bank.subset <- sample(c(TRUE, FALSE), size = nrow(complete.cases.bank), replace = TRUE)
    attr(complete.bank.subset, "name") <- "Some Fancy Subset"
    z.0 <- Regression(Overall ~ Fees + ATM, data = complete.cases.bank, outlier.prop.to.remove = 0,
                      subset = complete.bank.subset)
    expect_match(z.0$sample.description, "^n = \\d+ cases used in estimation \\(Some Fancy Subset\\);$")
    z.10 <- Regression(Overall ~ Fees + ATM, data = complete.cases.bank, outlier.prop.to.remove = 0.1,
                       subset = complete.bank.subset)
    expect_match(z.10$sample.description,
                 paste0("^n = \\d+ cases used in estimation of a total sample size of \\d+ \\(Some Fancy Subset\\);$"),
                 perl = TRUE)
    # Ensure subset with clashing name doesn't cause issues
    attr(complete.bank.subset, "name") <- "(Men); Women, (Children);"
    z.0 <- Regression(Overall ~ Fees + ATM, data = complete.cases.bank, outlier.prop.to.remove = 0,
                       subset = complete.bank.subset)
    expect_match(z.0$sample.description,
                 paste0("^n = \\d+ cases used in estimation \\(\\(Men\\); Women, \\(Children\\);\\);$"),
                 perl = TRUE)
    z.10 <- Regression(Overall ~ Fees + ATM, data = complete.cases.bank, outlier.prop.to.remove = 0.1,
                       subset = complete.bank.subset)
    expect_match(z.10$sample.description,
                 paste0("^n = \\d+ cases used in estimation of a total sample size of \\d+ \\(\\(Men\\); Women, \\(Children\\);\\);$"),
                 perl = TRUE)
    # Make some missing
    missing.index <- sample(c(TRUE, FALSE), size = nrow(complete.cases.bank), replace = TRUE, prob = c(1, 5))
    complete.cases.bank$Fees[missing.index] <- NA
    non.missing <- sum(!is.na(complete.cases.bank$Fees))
    z.0 <- Regression(Overall ~ Fees + ATM, data = complete.cases.bank, outlier.prop.to.remove = 0)
    expect_match(z.0$sample.description, paste0("^n = ", non.missing," cases used in estimation of a total sample ",
                                                "size of 244; cases containing missing values have been excluded;$"),
                 perl = TRUE)
    z.10 <- Regression(Overall ~ Fees + ATM, data = complete.cases.bank, outlier.prop.to.remove = 0.1)
    expect_match(z.0$sample.description, paste0("^n = ", non.missing," cases used in estimation of a total sample ",
                                                "size of 244; cases containing missing values have been excluded;$"),
                 perl = TRUE)
})

test_that("DS-3534 Outlier removal in Jaccard and Correlation", {
    coefs.wo.outliers <- list(c(40, -0.14),
                              c(20, 0),
                              c(20, -0.02))
    x.ranges <- list(c(71, 76, 79, 79, 95, 108, 120, 120, 121, 141, 147),
                     c(145, 160, 160, 168, 168, 225, 258),
                     c(276, 276, 276, 301, 304, 318, 350, 351, 360, 360, 400, 440, 460, 472))
    group <- factor(rep(letters[1:3], vapply(x.ranges, length, 1L)))
    dat <- mapply(function(coefs, x.domain) {
        y <- coefs[1] + coefs[2] * x.domain + rnorm(length(x.domain), sd = 1)
        data.frame(y = y, x = x.domain)
    },
    coefs.wo.outliers,
    x.ranges,
    SIMPLIFY = FALSE)
    dat <- do.call(rbind, dat)
    dat[["group"]] <- group
    # Add outliers
    outliers <- data.frame(x = c(70, 250, 300),
                           y = c(10, 15, 0),
                           group = factor(letters[1:3]))
    dat.w.outliers <- rbind(dat, outliers)
    # Check outliers get removed
    split.dat.w.outliers <- split(dat.w.outliers, dat.w.outliers[["group"]])
    outliers.removed <- lapply(split.dat.w.outliers, function(input.dat)
        removeDataThatAreOutliersFromLinearRegression(formula = y ~ x, data = input.dat, weights = NULL,
                                                      outlier.prop.to.remove = 0.13))
    expect_equal(outliers.removed, split(dat, dat[["group"]]))
})
