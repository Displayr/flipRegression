context("variables")

library(flipData)
y <- factor(rep(1:2, 5))
x <- runif(10)
z <- Regression(y ~ x, type = "Binary Logit")
y.degenerate <- factor(rep(1L, 10L))

test_that("Probabilities",{
    expect_equal(Observed(z), y)
})


test_that("Probabilities",{
    expect_equal(sum(Probabilities(z)[, 1]), as.numeric(table(y)[2]))

    pp <- Probabilities(z, data.frame(x=(1:5)/10))
    expect_equal(dim(pp), c(5, 2))
    expect_equal(rowSums(pp), rep(1, 5), check.attributes = FALSE)
    # Expect warning at both model fitting and Probabilities requested when poor Binary Logit model used
    expect_warning(z.degenerate <- Regression(y.degenerate ~ x, type = "Binary Logit"),
                   "needs to contain two or more categories")
    expect_warning(Probabilities(z.degenerate),
                   paste0("The Outcome variable only has a single category for this Binary Logit Regression model. ",
                          "The computed probabilities here are very likely to be uninformative and the outcome ",
                          "variable of the original Binary Logit model inspected. It should have the second category ",
                          "added and the Binary Logit model recomputed. The computed probabilities are all near zero ",
                          "as it is attempting to compute the probability of observing a category that wasn't ",
                          "included in the original data"),
                   fixed = TRUE)
})

test_that("Observed with .",{
    dat <- data.frame(y = rnorm(100), z = rbinom(100, 1, .5), x = rnorm(100))
    fit <- Regression(y~., data = dat)
    expect_equal(Observed(fit), dat[["y"]])
})

data(bank, package = "flipExampleData")

test_that("na.exclude behaviour of Regression methods",
{
    fit <- suppressWarnings(
        Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                   data = bank, missing = "Exclude cases with missing data")
    )
    resid <- residuals(fit)
    resid.expect <- flipRegression:::fillInMissingRowNames(rownames(bank),
                                                           residuals(fit$original))
    expect_equal(resid, resid.expect)
    expect_equal(names(resid), rownames(bank))
    expect_length(resid, nrow(bank))

    fitted <- fitted(fit)
    fitted.expect <- flipRegression:::fillInMissingRowNames(rownames(bank),
                                                           fitted(fit$original))
    expect_equal(fitted, fitted.expect)
    expect_equal(names(fitted), rownames(bank))
    expect_length(fitted, nrow(bank))
})

test_that("labels adding back for infIndexPlot; DS-3339",
{
    expect_error(infIndexPlot(z), NA)
})

test_that("DS-4360 Estimation Data template created correctly", {
    data(bank, package = "flipExampleData")
    bank.formula <- Overall ~ Fees + Interest
    all.variable.names <- flipU::AllVariablesNames(bank.formula, bank)
    sbank <- subset(bank, select = all.variable.names) |> na.omit()
    interest.levels <- c("Very dissatisfied", "unsatisfied", "a little unsatisfied",
                         "Neutral", "A little satisfied", "Satisfied", "Very satisfied")
    sbank <- transform(sbank, Interest = factor(Interest, labels = interest.levels))
    model.types <- list("Linear", "Binary Logit", "Ordered Logit", "NBD", "Quasi-Poisson",
                        "Poisson", "Multinomial Logit")
    # Helper function to test same data across all model types
    fitModelForTest <- function(type, data = sbank, ...) {
        is.binary.logit <- type == "Binary Logit"
        if (is.binary.logit) {
            data <- transform(data, OverallBinary = factor(as.numeric(Overall >= 4)))
        }
        reg.formula <- if (is.binary.logit) OverallBinary ~ Fees + Interest else Overall ~ Fees + Interest
        if (type == "Ordered Logit") data[["Overall"]] <- Ordered(data[["Overall"]])
        warn.msg <- if (type == "NBD") "Model may not have converged" else NA
        expect_warning(model <- Regression(formula = reg.formula,
                                           data = data,
                                           type = type,
                                           ...),
                       warn.msg)
        model
    }
    # Function to check each regression model
    checkEstimationDataTemplate <- function(regression.model, expected.template) {
        expect_true("estimation.data.template" %in% names(regression.model))
        expect_equal(regression.model[["estimation.data.template"]], expected.template)
    }
    # Basic tests for all model types, no dummy variable adjustment
    basic.expected.template <- structure(
        list(
            Overall = list(
                type = "numeric",
                default.value = 1
            ),
            Fees = list(
                type = "numeric",
                default.value = 1
            ),
            Interest = list(
                type = "factor",
                levels = interest.levels,
                observed.levels = interest.levels,
                has.unobserved.levels = FALSE,
                ordered = FALSE,
                default.value = interest.levels[1]
            )
        ),
        outcome.name = "Overall"
    )
    categorical.overall <- list(
        type = "factor",
        levels = as.character(1:7),
        observed.levels = as.character(1:7),
        has.unobserved.levels = FALSE,
        ordered = FALSE,
        default.value = "1"
    )
    for (type in model.types) {
        fit <- fitModelForTest(type = type)
        expected.template <- basic.expected.template
        outcome.name <- if (type == "Binary Logit") "OverallBinary" else "Overall"
        if (endsWith(type, "Logit")) {
            expected.template[["Overall"]] <- categorical.overall
            expected.template[["Overall"]][["ordered"]]  <- type == "Ordered Logit"
            if (startsWith(type, "Binary")) {
                names(expected.template) <- c("OverallBinary", "Fees", "Interest")
                expected.template[[outcome.name]][["levels"]] <- c("0", "1")
                expected.template[[outcome.name]][["observed.levels"]] <- c("0", "1")
                expected.template[[outcome.name]][["default.value"]] <- "0"
                attr(expected.template, "outcome.name") <- outcome.name
            }
        }
        #waldo::compare(fit$estimation.data.template, expected.template)
        checkEstimationDataTemplate(fit, expected.template)
    }
    # Dummy variable adjustment
    sbank.with.missing <- sbank
    ## Numeric adjustment and Factor adjustment
    is.na(sbank.with.missing[["Fees"]]) <- sbank.with.missing[["Fees"]] == 5L
    set.seed(12321)
    is.na(sbank.with.missing[["Interest"]]) <- sample.int(nrow(sbank), size = 10L)
    for (type in model.types) {
        fit.with.missing <- fitModelForTest(type = type,
                                            data = sbank.with.missing,
                                            missing = "Dummy variable adjustment")
        outcome.name <- if (startsWith(type, "Binary")) "OverallBinary" else "Overall"
        expected.template <- basic.expected.template
        # Dummy variables are always numeric (binary 0/1) and default to 0.
        expected.template[["Fees.dummy.var_GQ9KqD7YOf"]] <- list(type = "numeric", default.value = 0)
        expected.template[["Interest.dummy.var_GQ9KqD7YOf"]] <- list(type = "numeric", default.value = 0)
        # Numeric adjustment gets the mean of the observed values
        expected.template[["Fees"]][["imputed.value"]] <- mean(sbank.with.missing[["Fees"]], na.rm = TRUE)
        # Factor adjustment uses the baseline (first) level
        expected.template[["Interest"]][["imputed.value"]] <- interest.levels[1]
        if (endsWith(type, "Logit")) {
            expected.template[["Overall"]] <- categorical.overall
            expected.template[["Overall"]][["ordered"]]  <- type == "Ordered Logit"
            if (startsWith(type, "Binary")) {
                names(expected.template)[names(expected.template) == "Overall"] <- "OverallBinary"
                expected.template[[outcome.name]][["levels"]] <- c("0", "1")
                expected.template[[outcome.name]][["observed.levels"]] <- c("0", "1")
                expected.template[[outcome.name]][["default.value"]] <- "0"
            }
            attr(expected.template, "outcome.name") <- outcome.name
        }
        checkEstimationDataTemplate(fit.with.missing, expected.template)
    }
    ## Error handling
    fit.with.missing[["estimation.data.template"]] <- NULL
    err.msg <- paste0("appendDummyAdjustmentsToTemplate only works with ",
                      "Regression models that have an estimation.data.template.")
    expect_error(appendDummyAdjustmentsToTemplate(fit.with.missing), err.msg,
                 fixed = TRUE)
    fit.with.missing <- unclass(fit.with.missing)
    err.msg <- "appendDummyAdjustmentsToTemplate only works with Regression models."
    expect_error(appendDummyAdjustmentsToTemplate(fit.with.missing), err.msg,
                 fixed = TRUE)
})
