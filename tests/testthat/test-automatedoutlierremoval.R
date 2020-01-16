context("Automated Outlier Removal")

# coleman data from robustbase

coleman <- structure(list(salaryP = c(3.83, 2.89, 2.86, 2.92, 3.06, 2.07,
                                      2.52, 2.45, 3.13, 2.44, 2.09, 2.52, 2.22, 2.67, 2.71, 3.14, 3.54,
                                      2.52, 2.68, 2.37),
                          fatherWc = c(28.87, 20.1, 69.05, 65.4, 29.59,
                                       44.82, 77.37, 24.67, 65.01, 9.99, 12.2, 22.55, 14.3, 31.79, 11.6,
                                       68.47, 42.64, 16.7, 86.27, 76.73),
                          sstatus = c(7.2, -11.71, 12.32,
                                      14.28, 6.31, 6.16, 12.7, -0.17, 9.85, -0.05, -12.86, 0.92, 4.77,
                                      -0.96, -16.04, 10.62, 2.66, -10.99, 15.03, 12.77),
                          teacherSc = c(26.6,
                                        24.4, 25.7, 25.7, 25.4, 21.6, 24.9, 25.01, 26.6, 28.01, 23.51,
                                        23.6, 24.51, 25.8, 25.2, 25.01, 25.01, 24.8, 25.51, 24.51),
                          motherLev = c(6.19,
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
