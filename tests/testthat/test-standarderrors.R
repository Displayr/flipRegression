context("Standard errors")
data(colas, package = "flipExampleData")
z <- unclass(colas$q4a)
#z[z == 4] <- 9
#z[z == 5] <- 4
#z[z == 9] <- 5
flipFormat::Labels(z) <- "Like Coca-Cola"
colas$like.coke <- z - 3
colas$balanced <- c(rep(1:3, rep(100,3)), rep(NA, 27))
colas$agenumeric <- car::recode(colas$d1, as.factor = FALSE, recodes = "'18 to 24' = 21; '25 to 29' = 27; '30 to 34' = 32; '35 to 39' = 37; '40 to 44' = 42; '45 to 49' = 47; '50 to 54' = 52; '55 to 64' = 60; '65 or more' = 77")

test_that("Stata Linearized Standard Errors with weights", {
    z <- Regression(like.coke~d1, weights = colas$agenumeric, data = colas, robust.se = FALSE)
    # se
    expect_equal(as.numeric(z$summary$coefficients[1:3, 2]), c( .1186729, .2204833, .1738284), tolerance = 0.0000001)
    # p
    expect_equal(as.numeric(z$summary$coefficients[, 4]), c( 0.000, 0.122, 0.663 , 0.589, 0.486, 0.432, 0.453, 0.072,  0.405), tolerance = 0.0005)
})

test_that("Stata Robust Standard Errors", {
    z <- Regression(like.coke~d1, data = colas, robust.se = "hc1", show.labels = TRUE)
    # xtabs(~d1 + like.coke, data = colas)
    # data.frame(colas$d1, colas$like.coke)
    # table(colas$d1)
    # lm(like.coke ~ d1, data = colas)
    # #Regression(like.coke~d1, data = colas, robust.se = FALSE)
    # se
    expect_equal(as.numeric(z$summary$coefficients[1:3, 2]), c( .1201564, .2232395, .1760013 ), tolerance = 0.0000005)
    # p
    expect_equal(as.numeric(z$summary$coefficients[1:4, 4]), c( 0.000, 0.127 , 0.667 , 0.593), tolerance = 0.0005)
})

test_that("HCCM calculations", {
    # Use the PublicSchool data from the sandwich package
    # Compre the generated values against the values in the sandwich package
    data(PublicSchools, package = "sandwich")
    quad.model <- lm(Expenditure ~ Income + I(Income^2), data = PublicSchools)
    technical.problem <- "There is a technical problem with the parameter variance-covariance matrix"
    adjustments <- paste0("hc", 0:4)
    for (type in adjustments) {
        expect_error(car::hccm(quad.model, type = "hc0"), "object 'bads' not found")
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
    ## model fit and HC3 covariance
    model <- lm(y ~ x, data = dat)
    design <- survey::svydesign(ids = ~1, weights = ~weights, data = dat)
    weighted.model <- survey::svyglm(y ~ x, design)
    expect_equal(vcov2(weighted.model), FixVarianceCovarianceMatrix(vcov(weighted.model)))
    # Check other robust adjustments
    for (type in adjustments)
        expect_equal(vcov2(model, robust.se = type), sandwich::vcovHC(model, type = toupper(type)))
})
