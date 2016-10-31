context("contrasts")

expect_that("contrasts", {
            data(phone, package = "flipExampleData")
            phone$q4 <- ordered(phone$q4)
            z <- Regression(q25 ~ q4, data = phone, contrasts = c("contr.treatment", "contr.poly"))
            z1 <- lm(q25 ~ q4, data = phone)
            expect_equal(names(z$coef), names(z1$coef))
            z <- Regression(q25 ~ q4, data = phone)
            z1 <- lm(q25 ~ q4, data = phone)
            expect_false(all(names(z$coef) == names(z1$coef)))
    })

