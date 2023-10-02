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

    # Compute the svyglm with unit weights
    unit.weighted.lm <- survey::svyglm(target.formula, design = unit.design)

    computed.unit.lm.aic <- extractSvyLmAIC(unit.weighted.lm)
    computed.aic <- computed.unit.lm.aic[["AIC"]]

    survey.aic <- AIC(unit.weighted.svyglm)[["AIC"]]

    # Unit weighted model AIC should be within 1% tolerance of the unweighted model
    expect_true(abs((default.aic - computed.aic) / computed.aic) < 0.01)
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
    # Design effect matrix (delta) computation,
    ## compute the regression coefficient components first
    V0 <- weighted.lm[["naive.cov"]] * sigma2.hat
    V <- vcov(weighted.lm)
    delta.mu <- solve(V0, V)
    ## Compute sigma2 component
    i.sigma2 <- n.hat / (2 * sigma2.hat^2)
    u.sigma2 <- -1 / (2 * sigma2.hat) + eps^2 / (2 * sigma2.hat^2)
    h.sigma2 <- sum(w * u.sigma2^2)
    delta.sigma2 <- h.sigma2 / i.sigma2
    delta.bar <- mean(c(diag(delta.mu), delta.sigma2))
    eff.p <- sum(diag(delta.mu)) + delta.sigma2
    expected.aic <- minus.2.ell.hat + 2 * eff.p

    # Check that the computed AIC is the same as the expected AIC
    # in both the internal function and the resulting output from the Regression parent function
    expect_equal(computed.aic, expected.aic)
    reg.model <- Regression(target.formula, type = "Linear", weights = pw, data = apiclus2)
    extracted.aic <- extractAIC(reg.model)
    expect_equal(extracted.aic[["AIC"]], expected.aic)
    expect_equal(extracted.aic[["df"]], eff.p)
})
