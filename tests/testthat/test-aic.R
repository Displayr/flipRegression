context("Test computation of AIC")

data(api, package = "survey")

test_that("EH-530: Weighted AIC calculations correct", {
    # Use the simple random sample from the population data
    # Academic performance explained by % english language learners,
    # % free/reduced lunch, and % mobility
    target.formula <- api00 ~ ell + meals + mobility

    wt <- rep(1, nrow(apisrs))
    unit.design <- survey::svydesign(id = ~1, weights = wt, data = apisrs)

    unit.weighted.svyglm <- survey::svyglm(target.formula, design = unit.design)
    # Compute the unweighted model and its AIC
    reg.model <- lm(target.formula, data = apisrs)
    default.aic <- AIC(reg.model)
    # Should align with TIC too
    require("VGAM", quietly = TRUE)
    vglm.model <- vglm(target.formula, data = apisrs, family = uninormal)
    default.tic <- TIC(vglm.model)
    # Compute the svyglm with unit weights
    unit.weighted.lm <- survey::svyglm(target.formula, design = unit.design)

    computed.unit.lm.aic <- extractSvyLmAIC(unit.weighted.lm)
    computed.aic <- computed.unit.lm.aic[["AIC"]]

    survey.aic <- AIC(unit.weighted.svyglm)[["AIC"]]

    # Unit weighted model AIC should be within 1% tolerance of the unweighted model and TIC
    expect_true(abs((default.aic - computed.aic) / computed.aic) < 0.01)
    expect_true(abs((default.tic - computed.aic) / computed.aic) < 0.01)
    # survey package is way off the mark (as of survey version 4.1-1)
    expect_false(abs((default.aic - survey.aic) / survey.aic) < 0.01)
    expect_gt(abs((default.aic - survey.aic) / survey.aic), 138)

    # Check calculation is precise against the theory
    X <- model.matrix(target.formula, data = apisrs)
    y <- apisrs[["api00"]]
    linear.predictor <- X %*% coef(reg.model)
    sigma2.mle <- sum((y - linear.predictor)^2) / nrow(apisrs)
    n.hat <- nrow(apisrs)
    minus.2.ell <- n.hat * (1 + log(2 * pi) + log(sigma2.mle))
    n.variables <- length(flipU::AllVariablesNames(target.formula)) - 1L
    n.parameters <- n.variables + 2L # add intercept and sigma2
    eff.p <- 2 * n.parameters
    expected.aic <- minus.2.ell + eff.p
    expect_equal(default.aic, expected.aic)

    # Compute the weighted equivalent from the sampling weights (~pw)
    weighted.design <- svydesign(id = ~1, weights = ~pw, data = apiclus2)
    weighted.lm <- svyglm(target.formula, design = weighted.design)

    computed.weighted.lm.aic <- extractSvyLmAIC(weighted.lm)
    computed.aic <- computed.weighted.lm.aic[["AIC"]]

    # Extract weights used in survey
    w <- weighted.lm[["prior.weights"]]
    n.hat <- sum(w)
    y <- apiclus2[["api00"]]
    mu.hat <- weighted.lm[["linear.predictors"]]
    eps <- y - mu.hat
    sigma2.hat <- sum(eps^2 * w) / n.hat
    minus.2.ell.hat <- n.hat * log(sigma2.hat) + n.hat + n.hat * log(2 * pi)
    v.beta.zero <- weighted.lm[["naive.cov"]] * sigma2.hat
    v.beta <- vcov(weighted.lm)
    # Compute the delta matrix for the regression coefficients
    delta.beta.matrix <- solve(v.beta.zero, v.beta)
    # Compute the sigma2 component of the delta matrix
    ## Information matrix for sigma2 in unweighted case
    i.sigma2 <- 1 / (2 * sigma2.hat^2)
    ## Estimate the covariance of sigma2 under sampling weights
    ## Use the score equation estimator
    u.sigma2.i <- -1 / (2 * sigma2.hat) + eps^2 / (2 * sigma2.hat^2)
    var.sigma2 <-  1 / mean(w * u.sigma2.i^2)
    delta.sigma2 <- i.sigma2 * var.sigma2
    # Compute the overall design effects
    delta.beta <- diag(delta.beta.matrix)
    delta.bar <- mean(c(delta.beta, delta.sigma2))
    eff.p <- sum(delta.beta, delta.sigma2)
    # Design effect matrix (delta) computation,
    expected.aic <- minus.2.ell.hat + 2 * eff.p

    # Check that the computed AIC is the same as the expected AIC
    # in both the internal function and the resulting output from the Regression parent function
    expect_equal(computed.aic, expected.aic)
    reg.model <- Regression(target.formula, type = "Linear", weights = pw, data = apiclus2)
    extracted.aic <- extractAIC(reg.model)
    expect_equal(extracted.aic[["AIC"]], expected.aic)
    expect_equal(extracted.aic[["df"]], eff.p)
})
