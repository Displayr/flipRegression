data(adult.2000, package = "flipExampleData")


for (type in c("Linear", "Multinomial Logit", "Binary Logit"))
    for (output in c("Summary", "ANOVA", "Relative Importance Analysis"))
        test_that(paste("export data :", type, output),
                {
                    if (type != "Multinomial Logit" || output != "Relative Importance Analysis")
                    {
                        z <- suppressWarnings(Regression(relationship ~ age + sex, data = adult.2000, type = type, output = output))
                        expect_equal(length(dim(attr(z, "ChartData"))), 2L)
                    }
                })



test_that("ChartData uses last iteration results; DS-2424",
{
    z <- suppressWarnings(Regression(relationship ~ age + sex, data = adult.2000, type = "Linear"))
    expect_equal(head(attr(z, "ChartData"))[, 1], z$coef)
})
