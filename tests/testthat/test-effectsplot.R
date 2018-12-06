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

