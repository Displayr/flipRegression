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

test_that("Effects plot with colinear continuous variables; DS-2304",
{
    data(bank, package = "flipExampleData")
    bank$Fees2 <- bank$Fees
    model <- suppressWarnings(Regression(data = bank,
                 formula = Overall ~ Fees + Interest + Phone + Branch + Online + ATM + Fees2,
                 output = "Effects Plot", type = "Linear"))
    expect_error(suppressWarnings(print(model)), NA)
})

test_that("Effects plot with colinear categorical variables; DS-2304",
{
    adult.2000$race2 <- adult.2000$race
    rgr <- suppressWarnings(Regression(education_num ~ marital + sex + race + race2,
                          data = adult.2000, type = "Linear",
                          output = "Effects Plot",
                          effects.format = list(max.label = 5, y.axis = "HELLO")))
    expect_error(suppressWarnings(print(rgr)), NA)
})


test_that("Effects plot with weighted and filtered data; ", {
    n.cases <- nrow(adult.2000)
    adult.weight <- runif(n.cases)
    adult.subset <- sample(c(TRUE, FALSE), size = n.cases, replace = TRUE)
    mapply(function(variable.name, variable) assign(variable.name, variable, envir = .GlobalEnv),
           names(adult.2000),
           adult.2000)
    for (type in c("Linear", "Poisson", "Quasi-Poisson", "NBD", "Binary Logit", "Multinomial Logit"))
    {
        for (sub in c(FALSE, TRUE))
        {
            rgr <- suppressWarnings(Regression(education_num ~ marital + sex + hrs_per_week + income + workclass,
                                               type = type, weights = adult.weight,
                                               subset = if (sub) adult.subset,
                                               output = "Effects Plot",
                                               effects.format = list(max.label = 5, y.axis = "HELLO")))
            expect_error(suppressWarnings(print(rgr)), NA)
        }
    }
    rm(list = names(adult.2000), envir = .GlobalEnv)
})
