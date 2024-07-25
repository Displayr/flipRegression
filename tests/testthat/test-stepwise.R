context("Stepwise")
data(bank, package = "flipExampleData")
wgt <- bank$ID
attr(wgt, "label") <- "ID"
bank$FeesAndInterest <- bank$Fees * bank$Interest
linear.model <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM + FeesAndInterest,
                data = bank, type = "Linear"))

for (type in c("Linear", "Poisson", "Binary Logit",  "NBD", "Multinomial Logit", "Ordered Logit"))
    test_that(paste("Stepwise: ", type), {
        z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM + FeesAndInterest,
                        data = bank, type = type))
        expect_error(suppressWarnings(Stepwise(z)), NA)
    })

test_that(paste("Stepwise: ", "Quasi-Poisson"), {
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM + FeesAndInterest,
                    data = bank, type = "Quasi-Poisson"))
    expect_error(suppressWarnings(Stepwise(z)))
})

for (type in c("Linear", "Poisson", "Quasi-Poisson", "Binary Logit",  "NBD", "Multinomial Logit", "Ordered Logit"))
    for (direction in c("Forward", "Backward"))
            test_that(paste(direction, "Stepwise weighted:", type), {
                z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM + FeesAndInterest,
                        data = bank, type = type, weights = wgt))
                expect_error(suppressWarnings(Stepwise(z, direction = direction)), NA)
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
    expect_error(suppressWarnings(Stepwise(linear.model, direction = "Forward",
                                           always.include = c("Fees", "Interest"))), NA)
})

test_that("Stepwise: 2 steps", {
    expect_error(suppressWarnings(Stepwise(linear.model,
                                           direction = "Forward", steps = 2)), NA)
})

test_that("Regression + Stepwise with . in formula",
{
    n <- 200
    dat <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
    dat$y <- 3*dat[["x1"]] - 2*dat[["x2"]] + .5*rnorm(n)
    fit <- Regression(y~., data = dat)
    expect_true(all(names(fit$model) %in% names(dat)))

    out <- suppressWarnings(Stepwise(fit))
    expect_false("x3" %in% names(out$model$model))
})

test_that("Forward stepwise with robust standard errors (DS-2978)", {
    linear.model <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM + FeesAndInterest,
                                                data = bank, type = "Linear",
                                                robust.se = TRUE))
    expect_error(Stepwise(linear.model, direction = "Forward"), NA)
})

## https://github.com/ryantibs/best-subset/bestsubset
enlist <- function (...)
{
    result <- list(...)
    if ((nargs() == 1) & is.character(n <- result[[1]])) {
        result <- as.list(seq(n))
        names(result) <- n
        for (i in n) result[[i]] <- get(i)
    }
    else {
        n <- sys.call()
        n <- as.character(n)[-1]
        if (!is.null(n2 <- names(result))) {
            which <- n2 != ""
            n[which] <- n2[which]
        }
        names(result) <- n
    }
    result
}

## Simulation setup based on "Best subset selection via a modern
## optimization lens" by Dimitris Bertsimas, Angela King, and Rahul
## Mazumder, Annals of Statistics, 44(2), 813-852, 2016.
## Code source: \url{https://github.com/ryantibs/best-subset/bestsubset}
## The data model is: Y \sim N(Xbeta, sigma^2 I).  The predictor
## variables have covariance matrix Sigma, with (i,j)th entry
## rho^abs(i-j). The error variance sigma^2 is set according to the
## desired signal-to-noise ratio. The first 4 options for the nonzero
## pattern of the underlying regression coefficients beta follow the
## simulation setup in Bertsimas, King, and Mazumder (2016), and the
## 5th is a weak sparsity option:
##    • 1: beta has s components of 1, occurring at (roughly)
##      equally-spaced indices in between 1 and p
##    • 2: beta has its first s components equal to 1
##    • 3: beta has its first s components taking nonzero values,
##      where the decay in a linear fashion from 10 to 0.5
##    • 4: beta has its first 6 components taking the nonzero values
##      -10,-6, -2,2,6,10
##    • 5: beta has its first s components equal to 1, and the rest
##      decaying to zero at an exponential rate
sim.xy <- function (n, p, nval, rho = 0, s = 5, beta.type = 1, snr = 1)
{
    x = matrix(rnorm(n * p), n, p)
    xval = matrix(rnorm(nval * p), nval, p)
    if (rho != 0) {
        inds = 1:p
        Sigma = rho^abs(outer(inds, inds, "-"))
        obj = svd(Sigma)
        Sigma.half = obj$u %*% (sqrt(diag(obj$d))) %*% t(obj$v)
        x = x %*% Sigma.half
        xval = xval %*% Sigma.half
    }
    else Sigma = diag(1, p)
    s = min(s, p)
    beta = rep(0, p)
    if (beta.type == 1) {
        beta[round(seq(1, p, length = s))] = 1
    }
    else if (beta.type == 2) {
        beta[1:s] = 1
    }
    else if (beta.type == 3) {
        beta[1:s] = seq(10, 0.5, length = s)
    }
    else if (beta.type == 4) {
        beta[1:6] = c(-10, -6, -2, 2, 6, 10)
    }
    else {
        beta[1:s] = 1
        beta[(s + 1):p] = 0.5^(1:(p - s))
    }
    vmu = as.numeric(t(beta) %*% Sigma %*% beta)
    sigma = sqrt(vmu/snr)
    y = as.numeric(x %*% beta + rnorm(n) * sigma)
    yval = as.numeric(xval %*% beta + rnorm(nval) * sigma)
    enlist(x, y, xval, yval, Sigma, beta, sigma)
}

expect_stepwise_sim_contains_true_nonzero_coefficients <- function(seed,
                                                                   stepwise.direction,
                                                                   n.respondents,
                                                                   n.predictors,
                                                                   n.non.zero.coef,
                                                                   signal.to.noise.ratio,
                                                                   n.max.false.pos.nonzero = 0,
                                                                   n.max.false.neg.nonzero = 0,
                                                                   weights = NULL)
{
    set.seed(seed)
    dat <- sim.xy(n = n.respondents, p = n.predictors, nval = 0, rho = 0,
                  s = n.non.zero.coef, beta.type = 1,
                  snr = signal.to.noise.ratio)
    df <- as.data.frame(cbind(y = dat$y, dat$x))
    pred.names <- colnames(df)[-1]

    included.expected <- pred.names[dat$beta > 0]
    excluded.expected <- setdiff(pred.names, included.expected)

    ## Stepwise does not work with "." on RHS of formula
    form <- as.formula(paste0("y~", paste(pred.names, collapse = "+")))
    reg.out <- if (!is.null(weights)) Regression(form, data = df, weights = weights) else Regression(form, data = df)
    stepwise.out <- Stepwise(reg.out, direction = stepwise.direction)
    incl.out <- setdiff(pred.names, stepwise.out$excluded)
    expect_true(sum(!included.expected %in% incl.out) <= n.max.false.neg.nonzero)
    expect_true(sum(incl.out %in% excluded.expected) <= n.max.false.pos.nonzero)
}

test_that("EH-623 and EH-629: Simulation study of stepwise reg. using bestsubset pkg",
{
    set.seed(1)
    weights <- list(NULL, runif(300), runif(1000))
    for (w in c(1, 3))
        for (direction in c("Forward", "Backward"))
            for (seed in 2:4)
                expect_stepwise_sim_contains_true_nonzero_coefficients(
                               seed, stepwise.direction = direction,
                               n.respondents = 1000, n.predictors = 10,
                               n.non.zero.coef = 3, signal.to.noise.ratio = 4,
                               n.max.false.pos.nonzero = 2, n.max.false.neg.nonzero = 0,
                               weights = weights[[w]])

    for (w in c(1, 3))
        for (direction in c("Forward", "Backward"))
            for (seed in 10:12)
                expect_stepwise_sim_contains_true_nonzero_coefficients(
                                   seed, stepwise.direction = direction,
                                   n.respondents = 1000, n.predictors = 10,
                                   n.non.zero.coef = 3, signal.to.noise.ratio = 2,
                                   n.max.false.pos.nonzero = 3, n.max.false.neg.nonzero = 0,
                                   weights = weights[[w]])

    for (w in 1:2)
        for (direction in c("Forward", "Backward"))
            for (seed in 100:102)
                expect_stepwise_sim_contains_true_nonzero_coefficients(
                                   seed, stepwise.direction = direction,
                                   n.respondents = 300, n.predictors = 20,
                                   n.non.zero.coef = 4, signal.to.noise.ratio = 1,
                                   n.max.false.pos.nonzero = 5, n.max.false.neg.nonzero = 0,
                                   weights = weights[[w]])
})
