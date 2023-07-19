context("Multinomial Logit coefficient estimates")

data(iris)

iris.mn.logit <- Regression(
    Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
    data = iris,
    type = "Multinomial Logit"
)

require(nnet, quietly = TRUE)

iris.mn.logit.nnet <- multinom(
    Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
    data = iris,
    maxit = 10000,
    MaxNWts = 1e9
)

summary.iris.mn.logit <- summary(iris.mn.logit.nnet)

test_that("Uses nnet backend", {
    fitted.model <- iris.mn.logit[["original"]]
    expect_s3_class(fitted.model, "multinom")
    expect_equal(class(fitted.model), class(iris.mn.logit.nnet))
})

test_that("Coefficient estimates", {
    mnlogit.summary <- iris.mn.logit[["summary"]]
    expect_false(is.null(mnlogit.summary))
    expect_equal(
        mnlogit.summary[["coefficients"]],
        summary.iris.mn.logit[["coefficients"]]
    )
    expected.coefs <- array(
        c(18.4082084651887, -24.2300611858864, -6.08225088968087, -8.54730530784213,
          -9.39662663851482, -16.0771653430915, 16.1703774073959, 25.5996359547922,
          -2.05811336552616, 16.2274757165043),
        dim = c(2L, 5L),
        dimnames = list(
            c("versicolor", "virginica"),
            c("(Intercept)", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
        )
    )
    expect_equal(
        mnlogit.summary[["coefficients"]],
        expected.coefs,
        tolerance = 1e-4 # Rounding differences on architectures or LAPACK libraries
    )
    expected.std.err <- array(
        c(22.6042212565885, 23.6150863481396, 38.5975513681996, 38.6152601702841,
          40.3728977517317, 40.5355723732569, 109.039307826591, 109.17776569286,
          60.4512356663927, 60.7681151494788),
        dim = c(2L, 5L),
        dimnames = list(
            c("versicolor", "virginica"),
            c("(Intercept)", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
        )
    )
    expect_equal(
        mnlogit.summary[["standard.errors"]],
        summary.iris.mn.logit[["standard.errors"]],
        tolerance = 1e-4 # Rounding differences on architectures or LAPACK libraries
    )
    expect_equal(
        mnlogit.summary[["standard.errors"]],
        expected.std.err,
        tolerance = 1e-4 # Rounding differences on architectures or LAPACK libraries
    )
    expect_equal(
        mnlogit.summary[["z.values"]],
        summary.iris.mn.logit[["z.values"]]
    )
    z.statistics <- with(mnlogit.summary, coefficients / standard.errors)
    expect_equal(
        iris.mn.logit[["p.values"]],
        pnorm(abs(z.statistics), lower.tail = FALSE) * 2
    )
    expect_equal(
        mnlogit.summary[["AIC"]],
        summary.iris.mn.logit[["AIC"]],
    )
    expect_equal(
        mnlogit.summary[["AIC"]],
        31.8987263967134
    )
})
