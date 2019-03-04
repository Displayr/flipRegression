context("Effects plots")

data(adult.2000, package = "flipExampleData")
adult.2000 <- adult.2000[adult.2000$education_num >= 10, ]

test_that("Effects plot", {

    for (type in c("Linear", "Poisson", "Quasi-Poisson", "NBD", "Ordered Logit", "Binary Logit", "Multinomial Logit"))
        {
        rgr <- suppressWarnings(Regression(education_num ~ marital + sex + hrs_per_week + income + workclass,
                          data = adult.2000, type = type,
                          output = "Effects Plot",
                          effects.format = list(max.label = 5, y.axis = "HELLO")))
        expect_error(suppressWarnings(print(rgr)), NA)
    }
})

test_that("Effects plot with colinear variables; DS-2304",
{
    adult.2000$race2 <- adult.2000$race
    rgr <- suppressWarnings(Regression(education_num ~ marital + sex + race + race2,
                          data = adult.2000, type = "Linear",
                          output = "Effects Plot",
                          effects.format = list(max.label = 5, y.axis = "HELLO")))
    expect_error(suppressWarnings(print(rgr)), NA)
})
