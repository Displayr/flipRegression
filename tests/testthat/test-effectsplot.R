context("Effects plots")

data(adult.2000, package = "flipExampleData")

test_that("Stata Linearized Standard Errors with weights", {

    for (type in c("Linear", "Poisson", "Quasi-Poisson", "NBD", "Ordered Logit", "Binary Logit", "Multinomial Logit"))
    {
        rgr <- suppressWarnings(Regression(education_num ~ marital + sex + hrs_per_week,
                          data = adult.2000, type = type,
                          output = "Effects Plot",
                          effects.format = list(max.label = 5, y.axis = "HELLO")))
        expect_error(suppressWarnings(print(rgr)), NA)
    }
})

