context("Stepwise")
data(bank, package = "flipExampleData")
wgt <- bank$ID
attr(wgt, "label") <- "ID"
bank$FeesAndInterest <- bank$Fees * bank$Interest
linear.model <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM + FeesAndInterest,
                data = bank, type = "Linear")

for (type in c("Linear", "Poisson", "Binary Logit",  "NBD", "Multinomial Logit", "Ordered Logit"))
    test_that(paste("Stepwise: ", type), {
        z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM + FeesAndInterest,
                        data = bank, type = type))
        expect_error(suppressWarnings(Stepwise(z)), NA)
    })

test_that(paste("Stepwise: ", "Quasi-Poisson"), {
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM + FeesAndInterest,
                    data = bank, type = "Quasi-Poisson"))
    expect_that(suppressWarnings(Stepwise(z)), throws_error())
})

for (type in c("Linear", "Poisson", "Quasi-Poisson", "Binary Logit",  "NBD", "Multinomial Logit", "Ordered Logit"))
    test_that(paste("Stepwise weighted: ", type), {
        z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM + FeesAndInterest,
                        data = bank, type = type, weights = wgt))
        expect_error(suppressWarnings(Stepwise(z)), NA)
    })

test_that("Stepwise: detailed", {
    expect_error(suppressWarnings(Stepwise(linear.model, output = "Detailed")), NA)
})

test_that("Stepwise: all", {
    expect_error(suppressWarnings(Stepwise(linear.model, output = "All")), NA)
})

test_that("Stepwise: forward", {
    expect_error(suppressWarnings(Stepwise(linear.model, direction = "Forward")), NA)
})

test_that("Stepwise: always include fees and interest", {
    expect_error(suppressWarnings(Stepwise(linear.model, direction = "Forward", always.include = c("Fees", "Interest"))), NA)
})

test_that("Stepwise: 2 steps", {
    expect_error(suppressWarnings(Stepwise(linear.model, direction = "Forward", steps = 2)), NA)
})
