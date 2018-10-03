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
    z = Regression(like.coke~d1, weights = colas$agenumeric, data = colas, robust.se = FALSE)
    # se
    expect_equal(as.numeric(z$summary$coefficients[1:3, 2]), c( .1186729, .2204833, .1738284), tolerance = 0.0000001)
    # p
    expect_equal(as.numeric(z$summary$coefficients[, 4]), c( 0.000, 0.122, 0.663 , 0.589, 0.486, 0.432, 0.453, 0.072,  0.405), tolerance = 0.0005)
})

test_that("Stata Robust Standard Errors", {
    z = suppressWarnings(Regression(like.coke~d1, data = colas, robust.se = "hc1", show.labels = TRUE))
    #Regression(like.coke~d1, data = colas, robust.se = "hc1", show.labels = TRYE)
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

