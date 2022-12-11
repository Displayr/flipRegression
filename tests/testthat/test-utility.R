context("Utilities")

data(bank, package = "flipExampleData")
small.bank <- na.omit(bank)
small.bank <- transform(small.bank,
                        Overall_Binary = factor(as.numeric(Overall >= 4)))
small.wts <- runif(nrow(small.bank))
bank.formula <- Overall ~ Fees
regression.types <- c("Linear", "Binary Logit", "Ordered Logit",
                      "NBD", "Poisson", "Quasi-Poisson", "Multinomial Logit")
n.types <- length(regression.types)
weights <- replicate(n.types, NULL, simplify = FALSE)
weights <- c(weights, replicate(n.types, small.wts, simplify = FALSE))
regression.types <- rep(regression.types, 2L)

createFittedModel <- function(type, weights) {
    if (type == "Binary Logit") bank.formula <- Overall_Binary ~ Fees
    if (type == "Ordered Logit")  small.bank[["Overall"]] <- Ordered(small.bank[["Overall"]])
    warn.msg <- if (type == "NBD") "Model may not have converged" else NA
    expect_warning(model <- fitModel(.formula = bank.formula,
                                     .estimation.data = small.bank,
                                     .weights = weights,
                                     type = type,
                                     robust.se = FALSE,
                                     subset = NULL),
                  warn.msg)
    model[["model"]]
}

createFakeRegression <- function(model) {
    structure(list(original = model), class = "Regression")
}

raw.models <- mapply(createFittedModel, regression.types, weights)
full.models <- lapply(raw.models, createFakeRegression)

test_that("getModelType and isWeightedModel operate correctly", {
    for (i in seq_along(regression.types)) {
        expect_equal(getModelType(raw.models[[i]]), regression.types[i])
        expect_equal(getModelType(full.models[[i]]), regression.types[i])
        expect_equal(isWeightedModel(raw.models[[i]]), !is.null(weights[[i]]))
        expect_equal(isWeightedModel(full.models[[i]]), !is.null(weights[[i]]))
    }
})
