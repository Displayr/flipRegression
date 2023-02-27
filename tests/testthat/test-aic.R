context("Test computation of AIC")

data(api, package = "survey")

test_that("EH-530: Weighted AIC calculation correct with unit weights", {
    # Use the simple random sample from the population data
    weighted.design <- survey::svydesign(id = ~1, weights = ~pw, data = apisrs)
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

    # Compute the weighted equivalent from the sampling weights (~pw)
    weighted.lm <- svyglm(target.formula, design = weighted.design)

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
})
