context("Multiple imputation")

data("bank", package = "flipExampleData")
zbank <- bank[1:200,]
set.seed(23442)
zbank$rnd <- runif(nrow(zbank))
zbank$rnd1 <- runif(nrow(zbank))
zbank$rnd2 <- runif(nrow(zbank))
zbank$rnd3 <- runif(nrow(zbank))
zbank$rnd4 <- runif(nrow(zbank))
attr(bank$Overall, "label") <- "Overall satisfaction"
attr(bank$Fees, "label") <- "Fees paid"
attr(bank$Online, "label") <- "Online banking"

data("cola", package = "flipExampleData")
cola <- cola[1:150,]
cola$Q3[1:100] <- NA
cola$Q3 <- unclass(cola$Q3)

test_that("Multiple imputation with auxiliary variables ", {
    # No auxiliary variables, data has an argument
    z1 = Regression(Overall ~ Branch + Interest, data = zbank, missing = "Multiple imputation")$coef[2]
    # No auxiliary variables, data inferred from environment
    attach(zbank)
    z2 = Regression(Overall ~ Branch + Interest, missing = "Multiple imputation")$coef[2]
    detach(zbank)
    expect_equal(z1, z2)
    # Auxiliary variables, data has an argument
    zaux <- data.frame(a = zbank$Phone, b = zbank$Online)
    z3 = Regression(Overall ~ Branch + Interest,auxiliary.data = zaux, data = zbank, missing = "Multiple imputation")$coef[2]
    # Auxiliary variables, data inferred from environment
    attach(zbank)
    z4 = Regression(Overall ~ Branch + Interest, auxiliary.data = zaux, missing = "Multiple imputation")$coef[2]
    detach(zbank)
    expect_false(isTRUE(all.equal(z1, z4)))
    # Auxiliary variables, incorrect number of rows.
    zaux <- data.frame(a = zbank$Phone, b = zbank$Online)[1:10,]
    expect_error(Regression(Overall ~ Branch + Interest,auxiliary.data = zaux, data = zbank, missing = "Multiple imputation"))

})


test_that("Multiple imputation run using Regression", {
    est <- flipData::EstimationData(Overall ~ Fees + Interest + Phone + Branch + Online + ATM + rnd + rnd1 + rnd2 + rnd3 + rnd4, zbank, missing = "Multiple imputation", m = 10)
    models <- lapply(est$estimation.data, FUN = function(x) Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM + rnd + rnd1 + rnd2 + rnd3 + rnd4, data = x))
    coefs <- MultipleImputationCoefficientTable(models)
    expect_equal(coefs[12,5], 0.211, 0.001)
})

test_that("Multiple imputation ", {
    z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM + rnd + rnd1 + rnd2 + rnd3 + rnd4, data = zbank, missing = "Multiple imputation")
    expect_equal(z$coefficient.table[12,5], 0.283, 0.001)
})

# Using examples from https://www.ssc.wisc.edu/sscc/pubs/mi/missmech.do

#  generate data
set.seed(88)
n <- 1000
X <- rnorm(n)
y <- X + rnorm(n)

# drop values of x
# probability of x being missing 10%
# data is MCAR
x <- X
x[runif(n) < .1] <- NA

Regression(y ~ x, data = data.frame(y = y, x = x))
Regression(y ~ x, data = data.frame(y = y, x = x), missing = "Multiple imputation", m = 10)

# drop values of x
# probability of x being missing depends on y (observed)
# data is MAR
x <- X
x[runif(n) < y] <- NA
Regression(y ~ x, data = data.frame(y = y, x = x))
Regression(y ~ x, data = data.frame(y = y, x = x), missing = "Multiple imputation")


# drop values of x
# probability of x being missing now depends on x (sometimes unobserved)
# data is MNAR
x <- X
x[X < runif(n)] <- NA
z <- runif(n)
suppressWarnings(Regression(y ~ x + z, data = data.frame(y = y, x = x, z = z)))
Regression(y ~ x + z, data = data.frame(y = y, x = x, z = z), missing = "Multiple imputation")
Regression(y ~ x + z, data = data.frame(y = y, x = x, z = z), missing = "Use partial data (pairwise correlations)")


# Case study

distance <- Distance <- c(5, 5, 10, 10, 15, 15, 20, 20)
price <- Price <- c(1.6, 2.4, 1.1, 1.9, .6, 1.4, .1, .9)

PredictionPlot(Regression(price ~ distance))

## M ~ X
distance <- Distance
distance[7:8] <- NA

# Complete case
Regression(price ~ distance, data = data.frame(distance, price))

Regression(price ~ distance, data = data.frame(distance, price), missing = "Use partial data (pairwise correlations)")
Regression(price ~ distance, data = data.frame(distance, price)[1:6, ], missing = "Use partial data (pairwise correlations)")

# Regression-based imputation
Regression(price ~ distance, data = data.frame(distance, price), missing = "Multiple imputation")

# Hot decking
distance[7:8] <- 15
Regression(price ~ distance, data = data.frame(distance, price))

# Mean imputation
distance[7:8] <- mean(distance[1:6])
Regression(price ~ distance, data = data.frame(distance, price))

predict(Regression(distance ~ price))

# M ~ Y
distance <- Distance
distance[c(5,7)] <- NA
Regression(price ~ distance, data = data.frame(distance, price))
Regression(price ~ distance, data = data.frame(distance, price), missing = "Use partial data (pairwise correlations)")
Regression(price ~ distance, data = data.frame(distance, price), missing = "Multiple imputation")



