context("Standard errors")
data(colas, package = "flipExampleData")
z <- unclass(colas$q4a)
flipFormat::Labels(z) <- "Like Coca-Cola"
colas$like.coke <- z - 3
# Recoding scheme
# '18 to 24' = 21;
# '25 to 29' = 27;
# '30 to 34' = 32;
# '35 to 39' = 37;
# '40 to 44' = 42;
# '45 to 49' = 47;
# '50 to 54' = 52;
# '55 to 64' = 60;
# '65 or more' = 77
old.values <- levels(colas$d1)
new.values <- c(21, 27, 32, 37, 42, 47, 52, 60, 77)
colas$agenumeric <- new.values[match(colas$d1, old.values)]

test_that("Stata Linearized Standard Errors with weights", {
    z <- Regression(like.coke ~ d1, weights = colas$agenumeric, data = colas, robust.se = FALSE)
    # se
    z.coef.table <- z[["summary"]][["coefficients"]]
    expect_equal(z.coef.table[1:3, "Std. Error"],
                 c(`(Intercept)` = .1186729, `d125 to 29` = .2204833, `d130 to 34` = .1738284),
                 tolerance = 0.0000001)
    # p
    expect_equal(z.coef.table[, "Pr(>|t|)"],
                 c(`(Intercept)` = 0.000, `d125 to 29` = 0.122, `d130 to 34` = 0.663,
                   `d135 to 39` = 0.589, `d140 to 44` = 0.486, `d145 to 49` = 0.432,
                   `d150 to 54` = 0.453, `d155 to 64` = 0.072,  `d165 or more` = 0.405),
                 tolerance = 0.0005)
})

test_that("Stata Robust Standard Errors", {
    z <- Regression(like.coke ~ d1, data = colas, robust.se = "hc1", show.labels = TRUE)
    z.coef.table <- z[["summary"]][["coefficients"]]
    # se
    expect_equal(z.coef.table[1:3, "Robust SE"],
                 c(`(Intercept)` = .1201564, `Age: 25 to 29` = .2232395, `Age: 30 to 34` = .1760013),
                 tolerance = 0.0000005)
    # p
    expect_equal(z.coef.table[1:4, "Pr(>|t|)"],
                 c(`(Intercept)` = 0.000, `Age: 25 to 29` = 0.127, `Age: 30 to 34` = 0.667, `Age: 35 to 39` = 0.593),
                 tolerance = 0.0005)
})

test_that("HCCM calculations", {
    # Use the PublicSchool data from the sandwich package
    # Compre the generated values against the values in the sandwich package
    data(PublicSchools, package = "sandwich")
    quad.model <- lm(Expenditure ~ Income + I(Income^2), data = PublicSchools)
    technical.problem <- "There is a technical problem with the parameter variance-covariance matrix"
    adjustments <- paste0("hc", 0:4)
    for (type in adjustments) {
        # Expect car::hcmm to error that the matrix is singular
        expect_error(car::hccm(quad.model, type = "hc0"), "hccm estimator is singular with rank")
        # Otherwise output should match sandwich::vcovHC and warn the user about the technical problem
        expect_warning(output <- vcov2(quad.model, robust.se = type), technical.problem)
        expect_equal(output, sandwich::vcovHC(quad.model, type = toupper(type)))
    }
    # No robust adjustment
    expect_equal(vcov2(quad.model, robust.se = FALSE), vcov(quad.model))
    # Check a model with no issues, taken from sandwich::vcovHC help page
    set.seed(20230321)
    x <- sin(1:100)
    y <- 1 + x + rnorm(100)
    weights <- runif(100)
    dat <- data.frame(x, y, weights)
    model <- lm(y ~ x, data = dat)
    design <- survey::svydesign(ids = ~1, weights = ~weights, data = dat)
    weighted.model <- survey::svyglm(y ~ x, design)
    expect_equal(vcov2(weighted.model), FixVarianceCovarianceMatrix(vcov(weighted.model)))
    # Check other robust adjustments
    for (type in adjustments) {
        expect_equal(vcov2(model, robust.se = type), sandwich::vcovHC(model, type = toupper(type)))
        # Adjustments not used for weighted models
        expect_equal(vcov2(weighted.model, robust.se = type),
                     car::hccm(weighted.model, type = type))
    }
    # Check the case when a dummy variable adjustment is used.
    dat <- data.frame(
        nps = 100 * c(1, 0, 1, 1, -1, 1, 1, 1, 1, 1, 1, 1, -1, 1, -1, 1, 1, 0, 1, 1),
        driver = c(NA, 4, 5, 4, 4, 5, 5, 3, 5, 3, 5, 5, 1, 5, 2, 2, 5, 5, 5, 5)
    )
    # Expect only the warning about issues if robust error is checked
    expect_warning(Regression(nps ~ driver, data = dat, robust.se = FALSE,
                              missing = "Dummy variable adjustment"),
                   NA)
    for (type in paste0("hc", 0:4))
       expect_warning(Regression(nps ~ driver, data = dat, robust.se = type,
                                 missing = "Dummy variable adjustment"),
                      "There is a technical problem with the parameter variance-covariance")
})
