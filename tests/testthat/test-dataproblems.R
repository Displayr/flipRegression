context("Data problems")

test_that("Removing unused factors prior to regression", {

    data(phone, package = "flipExampleData")
    levs <- attr(phone$q3, "value.labels")

    z <- phone$q3
    z[is.na(z)] <- 100
    z <- as.numeric(z)
    z <- factor(z)

    lv <- c("-9", "0", names(levs[7:1]), "100")
    levels(z) <- lv
    z[z == "100"] <- NA
    phone$q3 <- z

    expect_error(suppressWarnings(Regression(q3 ~ q2, data = phone, missing = "Multiple imputation")), NA)
    expect_error(suppressWarnings(Regression(q2 ~ q3, data = phone, missing = "Multiple imputation")), NA)

})

test_that("FDR corrected p-values", {
    p.raw <- c(1e-3, 1e-2, 0.15, 0.3, 0.5, NA)
    p1 <- p.adjust(p.raw, "fdr")
    p2 <- PValueAdjustFDR(p.raw)
    expect_equal(p1[2], p2[2])
    expect_equal(sum(p1[1:5] != p2[1:5]), 4)
})


data(adult.2000, package = "flipExampleData")
set.seed(1234)
adult.2000$race[runif(2000) > 0.9] <- NA
adult.2000$age[runif(2000) > 0.9] <- -Inf
adult.2000$hrs_per_week[runif(2000) > 0.9] <- Inf

test_that("Infinity in data", {
    expected.error <- paste0("Variable(s) hrs_per_week, age contain infinite values. ",
                             "Either recode the infinities to finite values or set them as missing data.")
    expect_error(suppressWarnings(Regression(hrs_per_week ~ sex + race + age,
                                             type = "Linear",
                                             data = adult.2000,
                                             missing = "Exclude cases with missing data")),
                 expected.error,
                 fixed = TRUE)

})

test_that("DS-2876: Jaccard coefficients not suitable", {
    # Outcome not suitable
    set.seed(12321)
    Y <- rnorm(100)
    X <- lapply(1:3, function(x) rbinom(10, size = 1, prob = 0.5))
    badX <- lapply(1:3, function(x) rnorm(10))
    data <- data.frame(Y, X, badX)
    names(data) <- c("Y", paste0("X", 1:3), paste0("badX", 1:3))
    data$Yok <- rbinom(nrow(data), size = 1, prob = 0.5)
    error.prefix <- paste0("Both the outcome and predictor variable need to be binary variables ",
                           "(only take the values zero or one). The ")
    plural.error.prefix <- paste0("Both the outcome and predictor variables need to be binary variables ",
                                  "(only take the values zero or one). The ")
    outcome.warning <- paste0("outcome variable ", sQuote("Y"), " is not a binary variable")
    error.suffix <- paste0(", please change the variable to a binary variable if you wish to ",
                           "create output with the Jaccard coefficients.")
    plural.error.suffix <- paste0(", please change the variables to binary variables if you wish to ",
                                  "create output with the Jaccard coefficients.")
    expect_warning(Regression(Y ~ X1, data = data, output = "Jaccard Coefficient"),
                   "^Y has been dichotimized")
    expect_error(processDataSuitableForJaccard(Yok ~ badX1,
                                               data = subset(data, select = c("Yok", "badX1"))),
                 paste0(error.prefix, "predictor variable ", sQuote("badX1"),
                        " is not a binary variable", error.suffix),
                 fixed = TRUE)
    expect_error(processDataSuitableForJaccard(Yok ~ X1 + badX1,
                                               data = subset(data, select = c("Yok", "X1", "badX1"))),
                 paste0(plural.error.prefix, "predictor variable ", sQuote("badX1"),
                        " is not a binary variable", error.suffix),
                 fixed = TRUE)
    expect_error(processDataSuitableForJaccard(Yok ~ X1 + badX1 + badX2,
                                               data = subset(data, select = c("Yok", "X1", "badX1", "badX2"))),
                 paste0(plural.error.prefix, "predictor variables ", sQuote("badX1"),
                        " and ", sQuote("badX2"), " are not binary variables", plural.error.suffix), fixed = TRUE)
    expect_error(processDataSuitableForJaccard(Yok ~ X1 + badX1 + badX2 + badX3,
                                               data = subset(data, select = c("Yok", "X1", "badX1", "badX2", "badX3"))),
                 paste0(plural.error.prefix, "predictor variables ", sQuote("badX1"), ", ",
                        sQuote("badX2"), " and ", sQuote("badX3"), " are not binary variables", plural.error.suffix),
                 fixed = TRUE)
    # Outcome and predictors is ok.
    data$Y <- rbinom(nrow(data), size = 1, prob = 0.5)
    expected.output.list <- list(data = subset(data, select = c("Y", "X1")),
                                 formula = Y ~ X1,
                                 labels = "X1")
    expect_equal(processDataSuitableForJaccard(Y ~ X1, data = subset(data, select = c("Y", "X1"))),
                 expected.output.list)
    # Good outcome but bad predictors throw errors
    expect_error(processDataSuitableForJaccard(Y ~ badX1, data = subset(data, select = c("Y", "badX1"))),
                 paste0(error.prefix, "predictor variable ", sQuote("badX1"),
                        " is not a binary variable", error.suffix), fixed = TRUE)
    expect_error(processDataSuitableForJaccard(Y ~ X1 + badX1, data = subset(data, select = c("Y", "X1", "badX1"))),
                 paste0(plural.error.prefix, "predictor variable ", sQuote("badX1"),
                        " is not a binary variable", error.suffix), fixed = TRUE)
    expect_error(processDataSuitableForJaccard(Y ~ X1 + badX1 + badX2,
                                               data = subset(data, select = c("Y", "badX1", "badX2"))),
                 paste0(plural.error.prefix, "predictor variables ", sQuote("badX1"),
                        " and ", sQuote("badX2"), " are not binary variables", plural.error.suffix), fixed = TRUE)
    expect_error(processDataSuitableForJaccard(Y ~ X1 + badX1 + badX2 + badX3,
                                               data = subset(data, select = c("Y", "badX1", "badX2", "badX3"))),
                 paste0(plural.error.prefix, "predictor variables ", sQuote("badX1"), ", ",
                        sQuote("badX2"), " and ", sQuote("badX3"), " are not binary variables", plural.error.suffix),
                 fixed = TRUE)
    # Handles interaction terms.
    data$int <- factor(rep(LETTERS[1:4], 25))
    input.dat <- subset(data, select = c("Y", "X1", "int"))
    expected.output.list <- list(data = input.dat,
                                 formula = Y ~ X1,
                                 labels = "X1")
    expect_equal(processDataSuitableForJaccard(Y ~ X1,
                                               interaction.name = "int",
                                               data = subset(data, select = c("Y", "X1", "int"))),
                 expected.output.list)
    # Check fancy label are returned if requested
    attr(input.dat$X1, "label") <- "X1 from Q2"
    expected.output.list$data <- input.dat
    expect_equal(processDataSuitableForJaccard(Y ~ X1,
                                               interaction.name = "int",
                                               data = input.dat),
                 expected.output.list)
    expected.output.list$labels <- "X1 from Q2"
    expect_equal(processDataSuitableForJaccard(Y ~ X1, interaction.name = "int",
                                               data = input.dat, show.labels = TRUE),
                 expected.output.list)

    # Check warnings thrown for data with no variation.
    data$Y1 <- 1
    error.prefix <- "Both the outcome and predictor variables should be binary variables. However, the "
    single.constant <- paste0(", it is constant with no variation (all values in the ",
                              "variable are zero or all values in the variable are one). ")
    many.constant <- paste0(", the variables are constant with no variation (all values in the ",
                            "variable are zero or all values in the variable are one). ")
    single.error.suffix <- "Consider if this variable is appropriate and remove if necessary."
    many.error.suffix <- "Consider if these variables are appropriate and remove if necessary."
    expect_error(Regression(Y1 ~ X1 + X2, data = data, output = "Jaccard Coefficient"),
                 paste0("The Outcome variable needs to be a binary variable. It is not. ",
                        "It is constant with no variation (all values in the variable are the same). ",
                        "Please replace the outcome variable with a binary variable."),
                 fixed = TRUE)
    data$X10 <- 1
    data$X21 <- 1
    expect_error(processDataSuitableForJaccard(Y ~ X10 + X2, data = subset(data, select = c("Y", "X10", "X2"))),
                 paste0(error.prefix, "predictor variable ", sQuote("X10"), " is not a binary variable",
                        single.constant, single.error.suffix), fixed = TRUE)
    expect_error(processDataSuitableForJaccard(Y1 ~ X10 + X2 + X21,
                                               data = subset(data, select = c("Y1", "X10", "X2", "X21"))),
                 paste0(error.prefix, "outcome variable ", sQuote("Y1"), " is not a binary variable",
                        " and predictor variables ", sQuote("X10"), " and ", sQuote("X21"), " are not binary variables",
                        many.constant, many.error.suffix), fixed = TRUE)
    # Check binary categorical variable handled and converted to numeric binary
    binary.cat <- factor(sample(LETTERS[1:2], size = nrow(data), replace = TRUE))
    data$binary.cat <- binary.cat
    binary.splits <- lapply(1:nlevels(data$binary.cat),
                            function(x) as.numeric(data$binary.cat == levels(binary.cat)[x]))
    output.dat <- data.frame(cbind(data[names(data) %in% c("Y", "X1")], binary.splits))
    names(output.dat) <- c("Y", "X1", "binary.cat.1", "binary.cat.2")
    attr(output.dat$binary.cat.1, "label") <- "binary.cat: A"
    attr(output.dat$binary.cat.2, "label") <- "binary.cat: B"
    input.dat <- subset(data, select = c("Y", "X1", "binary.cat"))
    expected.output.list <- list(data = output.dat,
                                 formula = Y ~ X1 + binary.cat.1 + binary.cat.2,
                                 labels = c("X1", "binary.cat.1", "binary.cat.2"))
    expect_equal(processDataSuitableForJaccard(Y ~ X1 + binary.cat,
                                               data = input.dat),
                 expected.output.list)
    # Check categorical variables with more than two levels handled and converted to numeric binary
    cat <- factor(sample(LETTERS[1:3], size = nrow(data), replace = TRUE))
    data$cat <- cat
    binary.splits <- lapply(1:nlevels(data$cat), function(x) as.numeric(data$cat == levels(cat)[x]))
    output.dat <- data.frame(cbind(data[names(data) %in% c("Y", "X1")], binary.splits))
    names(output.dat) <- c("Y", "X1", "cat.1", "cat.2", "cat.3")
    attr(output.dat$cat.1, "label") <- "cat: A"
    attr(output.dat$cat.2, "label") <- "cat: B"
    attr(output.dat$cat.3, "label") <- "cat: C"
    input.dat <- subset(data, select = c("Y", "X1", "cat"))
    expected.output.list <- list(data = output.dat,
                                 formula = Y ~ X1 + cat.1 + cat.2 + cat.3,
                                 labels = c("X1", "cat.1", "cat.2", "cat.3"))

    expect_equal(processDataSuitableForJaccard(Y ~ X1 + cat,
                                               data = input.dat),
                 expected.output.list)
    # Check ordinal factors are split into binary numeric variables
    ordered <- factor(sample(LETTERS[1:3], size = nrow(data), replace = TRUE), ordered = TRUE)
    data$ordered <- ordered
    binary.splits <- lapply(1:nlevels(data$ordered), function(x) as.numeric(data$ordered == levels(ordered)[x]))
    output.dat <- data.frame(cbind(data[names(data) %in% c("Y", "X1")], binary.splits))
    names(output.dat) <- c("Y", "X1", "ordered.1", "ordered.2", "ordered.3")
    attr(output.dat$ordered.1, "label") <- "ordered: A"
    attr(output.dat$ordered.2, "label") <- "ordered: B"
    attr(output.dat$ordered.3, "label") <- "ordered: C"
    input.dat <- subset(data, select = c("Y", "X1", "ordered"))
    expected.output.list <- list(data = output.dat,
                                 formula = Y ~ X1 + ordered.1 + ordered.2 + ordered.3,
                                 labels = c("X1", "ordered.1", "ordered.2", "ordered.3"))
    expect_equal(processDataSuitableForJaccard(Y ~ X2 + ordered,
                                               data = input.dat),
                expected.output.list)
    # Check order of binary transformed categorical variables is preserved
    input.dat <- subset(data, select = c("Y", "int", "X1", "cat", "X2", "ordered", "X3"))
    expect_error(output <- processDataSuitableForJaccard(data = input.dat,
                                                         formula = Y ~ int + X1 + cat + X2 + ordered + X3),
                 NA)
    expect_equal(names(output$data), c("Y", paste0("int.", 1:4), "X1",
                                       paste0("cat.", 1:3), "X2",
                                       paste0("ordered.", 1:3), "X3"))
    # Same test with input data in different order to the formula
    input.data <- subset(data, select = c("Y", paste0("X", 1:3), "int", "cat", "ordered"))
    expect_error(output <- Regression(Y ~ int + X1 + cat + X2 + ordered + X3, data = input.data,
                                      output = "Jaccard Coefficient"),
                 NA)
    names(output$estimation.data)
    expect_equal(names(output$estimation.data), c("Y", paste0("int.", 1:4), "X1",
                                                  paste0("cat.", 1:3), "X2",
                                                  paste0("ordered.", 1:3), "X3", "non.outlier.data_GQ9KqD7YOf"))
})

test_that("DS-2990: Test identification of aliased predictors and variables with no variation", {
    set.seed(1)
    sigma.mat <- matrix(c(1, 0.5, 0.3,
                          0.5, 1, 0.4,
                          0.3, 0.4, 1), byrow = TRUE, ncol = 3)
    X <- MASS::mvrnorm(n = 100, mu = rep(0, 3), Sigma = sigma.mat)
    beta <- 1:3
    Y <- X %*% beta + rnorm(100)
    dat <- data.frame(Y = Y, X = X)
    # add aliased predictors, single numeric and categorical
    dat$X.4 <- dat$X.3
    dat$X.5 <- 2 * dat$X.3
    dat$X.6 <- dat$X.3 / 2 - dat$X.2 + dat$X.1 / 2
    dat$cat1 <- factor(rep(c(1, 2, 3, 4), c(10, 10, 20, 60)), labels = LETTERS[1:4])
    dat$cat2 <- factor(rep(c(1, 2, 3, 4, 5), c(10, 10, 20, 30, 30)), labels = LETTERS[1:5])
    ##  Check function that identifies predictors into groups of aliased predictors ##
    # Expect null when no aliased predictors
    expect_null(determineAliased(Y ~ X.1 + X.2 + X.3, data = dat, "Y"))
    # Expect named list of class types that are aliased
    expect_equal(determineAliased(Y ~ X.1 + X.2 + X.3 + X.4, data = dat, "Y"),
                 single.group <- structure(list(c("X.3" = "numeric", "X.4" = "numeric")),
                                           all.aliased.variables = c("X.3", "X.4")))
    expect_equal(determineAliased(Y ~ X.1 + X.2 + X.3 + X.4 + cat1, data = dat, "Y"),
                 single.group)
    aliased.factors <- paste0("cat", c(rep(1:2, 3), 2), rep(LETTERS[2:5], c(2, 2, 2, 1)))
    expect_equal(determineAliased(Y ~ X.1 + X.2 + X.3 + X.4 + cat1 + cat2, data = dat, "Y"),
                 mixed.group <- structure(list(c("X.3" = "numeric", "X.4" = "numeric"),
                                               c("cat1" = "factor", "cat2" = "factor")),
                                          all.aliased.variables = c("X.3", "X.4", aliased.factors)))
    expect_equal(determineAliased(Y ~ X.1 + X.2 + X.3 + X.6, data = dat, "Y"),
                 long.two <- structure(list(c("X.1" = "numeric", "X.2" = "numeric",
                                              "X.3" = "numeric", "X.6" = "numeric")),
                                       all.aliased.variables = c("X.1", "X.2", "X.3", "X.6")))
    expect_equal(determineAliased(Y ~ X.1 + X.2 + X.3 + X.5 + X.6, data = dat, "Y"),
                 long.two <- structure(list(c("X.3" = "numeric", "X.5" = "numeric"),
                                       c("X.1" = "numeric", "X.2" = "numeric", "X.3" = "numeric", "X.6" = "numeric")),
                                       all.aliased.variables = paste0("X.", c(3, 5, 1, 2, 3, 6))))
    ## Check error message thrown is appropriate
    regular.labels <- c(paste0("X.", 1:6), paste0("cat", 1:2))
    fancy.labels <- c("Apples", "Oranges", "Grapes", "Banana", "Peaches", "Kiwi", "Lions", "Tigers")
    names(fancy.labels) <- names(regular.labels) <- regular.labels
    # Single group, regular labels with names,
    expect_error(throwAliasedExceptionImportanceAnalysis(single.group, regular.labels, "Shapley Regression"),
                 paste0("Some predictors are linearly dependent on other predictors and cannot be estimated if they ",
                        "are included in the model together. The following group of predictors: ('X.3', 'X.4') have a ",
                        "linearly dependent relationship and at least one of them needs to be removed to conduct a ",
                        "Shapley Regression."), fixed = TRUE)
    # Test warning given when a group is specified
    expect_error(throwAliasedExceptionImportanceAnalysis(single.group, regular.labels,
                                                         output = "Shapley Regression",
                                                         group.name = "Displayr"),
                 paste0("Within the Displayr category, some predictors are linearly dependent on other predictors and ",
                          "cannot be estimated if they are included in the model together. The following group of ",
                          "predictors: ('X.3', 'X.4') have a linearly dependent relationship and at least one of them ",
                          "needs to be removed to conduct a Shapley Regression."), fixed = TRUE)
    expect_error(throwAliasedExceptionImportanceAnalysis(single.group, fancy.labels, "Relative Importance Analysis"),
                 paste0("Some predictors are linearly dependent on other predictors and cannot be estimated if they ",
                        "are included in the model together. The following group of predictors: ('Grapes', 'Banana') ",
                        "have a linearly dependent relationship and at least one of them needs to be removed to ",
                        "conduct a Relative Importance Analysis."), fixed = TRUE)
    # Two numeric predictors aliased.
    expect_error(throwAliasedExceptionImportanceAnalysis(long.two, fancy.labels, "Shapley Regression"),
                 paste0("Some predictors are linearly dependent on other predictors and cannot be estimated if ",
                        "they are included in the model together. In each of the following groups of predictors ",
                        "there is a linearly dependent relationship and at least one predictor needs to be ",
                        "removed to conduct a Shapley Regression. Group 1: ('Grapes', 'Peaches'), Group 2: ",
                        "('Apples', 'Oranges', 'Grapes', 'Kiwi')."), fixed = TRUE)
    # Mix categorical/numeric
    expect_error(throwAliasedExceptionImportanceAnalysis(mixed.group, regular.labels, "Shapley Regression"),
                 paste0("Some predictors are linearly dependent on other predictors and cannot be estimated if they ",
                        "are included in the model together. In each of the following groups of predictors there is a ",
                        "linearly dependent relationship and at least one predictor needs to be removed to conduct a ",
                        "Shapley Regression. Group 1: ('X.3', 'X.4'), Group 2: ('cat1', 'cat2'). The linearly ",
                        "dependent relationship with the categorical variables  occurs in the dummy coding of at ",
                        "least one of the contrast levels for each categorical variable."), fixed = TRUE)
    # Test outcome with no variation
    expect_error(throwAliasedExceptionImportanceAnalysis(mixed.group, regular.labels, "Shapley Regression"),
                 paste0("Some predictors are linearly dependent on other predictors and cannot be estimated if they ",
                        "are included in the model together. In each of the following groups of predictors there is a ",
                        "linearly dependent relationship and at least one predictor needs to be removed to conduct a ",
                        "Shapley Regression. Group 1: ('X.3', 'X.4'), Group 2: ('cat1', 'cat2'). The linearly ",
                        "dependent relationship with the categorical variables  occurs in the dummy coding of at ",
                        "least one of the contrast levels for each categorical variable."), fixed = TRUE)
    dat$const <- 1
    dat.with.att <- dat
    attr(dat.with.att$const, "label") <- "Fancy constant"
    expect_error(validateVariablesHaveVariation(const ~ X.1 + X.2, dat.with.att, outcome.name = "const",
                                                show.labels = FALSE, output = "Shapley Regression"),
                 paste0("The outcome variable, 'const', is constant and has no variation. The outcome needs to have ",
                        "at least two unique values to conduct a Shapley Regression"),
                 fixed = TRUE)
    expect_error(validateVariablesHaveVariation(const ~ X.1 + X.2, dat.with.att, outcome.name = "const",
                                                show.labels = FALSE, output = "Shapley Regression",
                                                group.name = "Displayr"),
                 paste0("Within the Displayr category, the outcome variable, 'const', is constant ",
                        "and has no variation. The outcome needs to have at least two unique values ",
                        "to conduct a Shapley Regression"),
                 fixed = TRUE)
    expect_error(validateVariablesHaveVariation(const ~ X.1 + X.2, dat.with.att, outcome.name = "const"
                                                , show.labels = TRUE, output = "Relative Importance Analysis"),
                 paste0("The outcome variable, 'Fancy constant', is constant and has no variation. ",
                        "The outcome needs to have at least two unique values to conduct a ",
                        "Relative Importance Analysis"),
                 fixed = TRUE)
    expect_error(validateVariablesHaveVariation(Y ~ X.1 + const + X.2, dat.with.att, outcome.name = "Y",
                                                show.labels = FALSE, output = "Relative Importance Analysis"),
                 paste0("Each predictor needs to have at least two unique values to conduct a ",
                        "Relative Importance Analysis. The predictor 'const' is constant and has no variation."),
                 fixed = TRUE)
    expect_error(validateVariablesHaveVariation(Y ~ X.1 + const + X.2, dat.with.att, outcome.name = "Y",
                                                show.labels = TRUE, output = "Shapley Regression"),
                 paste0("Each predictor needs to have at least two unique values to conduct a Shapley Regression. ",
                        "The predictor 'Fancy constant' is constant and has no variation."),
                 fixed = TRUE)
    dat$const.fact <- factor(rep("B", nrow(dat)), levels = LETTERS[1:3])
    dat.with.att <- dat
    attr(dat.with.att$const.fact, "label") <- "This is a bad factor"
    expect_error(validateVariablesHaveVariation(Y ~ X.1 + const.fact + X.2, dat.with.att, outcome.name = "Y",
                                                show.labels = TRUE, output = "Shapley Regression"),
                 paste0("Each predictor needs to have at least two unique values to conduct a Shapley Regression. ",
                        "The predictor 'This is a bad factor' is constant and has no variation."),
                 fixed = TRUE)
    expect_error(validateVariablesHaveVariation(Y ~ X.1 + const.fact + const + X.2, dat.with.att, outcome.name = "Y",
                                                show.labels = FALSE, output = "Shapley Regression"),
                 paste0("Each predictor needs to have at least two unique values to conduct a Shapley Regression. ",
                        "The following predictors are constant and have no variation: 'const.fact', 'const'"),
                 fixed = TRUE)
})

test_that("DS-2884: Ordered Logit with non-syntactic variable names",
{
    set.seed(303)
    `Cola Tracking.sav` <- list()
    `Cola Tracking.sav`$Variables$d1 <- as.factor(sample(4, 100, replace = TRUE))
    `Cola Tracking.sav`$Variables$d2 <- runif(100)
    `Cola Tracking.sav`$Variables$d3 <- as.factor(sample(3, 100, replace = TRUE))
    form <- `Cola Tracking.sav`$Variables$d1 ~
        `Cola Tracking.sav`$Variables$d2 +
        `Cola Tracking.sav`$Variables$d3
    expect_error(out <- Regression(type = "Ordered Logit", formula = form),
                 NA)
    # Prevent pops
    mockery::stub(print.Regression, "print.htmlwidget", NULL)
    expect_error(print(out), NA)

    ColaTracking.sav <- list()
    ColaTracking.sav$Variables$d1 <- as.factor(sample(4, 100, replace = TRUE))
    ColaTracking.sav$Variables$d2 <- runif(100)
    ColaTracking.sav$Variables$d3 <- as.factor(sample(3, 100, replace = TRUE))
    form <- ColaTracking.sav$Variables$d1 ~
        ColaTracking.sav$Variables$d2 +
        ColaTracking.sav$Variables$d3
    expect_error(out <- Regression(type = "Ordered Logit", formula = form),
                 NA)
    expect_error(print(out), NA)

})

test_that("DS-2990: Helper functions detecting dataset references in formula", {
    # Check function that identifies dataset references in variable names is working
    all.var.names <- c("`My data`$Variables$`My Y`", "X")
    # Test outcome variable first
    expect_error(name.output <- checkVariablesForDataSetNames(all.var.names), NA)
    # Dataset reference identified and correct logical vector
    expect_equal(name.output, c("`My data`$Variables$`My Y`" = TRUE, X = FALSE))
    # Check predictors working ok too
    all.var.names <- c("`My Y`", "`My data`$Variables$X", "`Other data`$Variables$`Fancy X`", "X3", "`Another X`")
    expect_error(name.output <- checkVariablesForDataSetNames(all.var.names), NA)
    expect_equal(name.output, c("`My Y`" = FALSE,
                                "`My data`$Variables$X" = TRUE,
                                "`Other data`$Variables$`Fancy X`" = TRUE,
                                X3 = FALSE, "`Another X`" = FALSE))
    # Check handles the case where variables are ambiguous when dataset references are removed.
    all.var.names <- c("Y", "`My data`$Variables$X", "`Other data`$Variables$X", "X3", "X")
    expect_error(name.output <- checkVariablesForDataSetNames(all.var.names), NA)
    expect_equivalent(name.output, c(FALSE, TRUE, TRUE, FALSE, FALSE))
    # Checks formula and data have their references to other datasets removed in their names.
    sigma.mat <- matrix(c(1, 0.5, 0.3,
                          0.5, 1, 0.4,
                          0.3, 0.4, 1), byrow = TRUE, ncol = 3)
    X <- MASS::mvrnorm(n = 100, mu = rep(0, 3), Sigma = sigma.mat)
    dat <- data.frame(X = X)
    dat$Y <- runif(nrow(dat))
    dat.with.ref <- dat
    # change the name of the outcome
    names(dat.with.ref)[length(dat)] <- "`My data`$Variables$Y"
    all.var.names <- c("`My data`$Variables$Y", "X.1", "X.2", "X.3")
    bad.names <- c("`My data`$Variables$Y", paste0("X.", 1:3))
    good.names <- c("Y", paste0("X.", 1:3))
    names(good.names) <- bad.names
    expected.check <- logical(4)
    names(expected.check) <- bad.names
    expected.check[1L] <- TRUE
    expect_equal(reference.vector <- checkVariablesForDataSetNames(all.var.names),
                 expected.check)
    expected.mapping <- good.names
    names(expected.mapping) <- bad.names
    expect_equal(removeDataSetReferences(reference.vector),
                 expected.mapping)
    input.formula <- `My data`$Variables$Y ~ X.1 + X.2 + X.3
    expect_error(output.list <- relabelFormulaAndData(expected.mapping, input.formula, dat.with.ref),
                 NA)
    expect_setequal(names(output.list$data), c("Y", paste0("X.", 1:3)))
    checkDataSame <- function(outlist, data)
    {
        # Do a manual rename of the columns
        names(data) <- sub("^[[:print:]]*[$](Variables|Questions)[$]", "", names(data))
        vapply(names(outlist$data), function(x) identical(outlist$data[[x]], data[[x]]), logical(1))
    }
    expect_true(all(checkDataSame(output.list, dat)))
    expect_equal(output.list$formula, Y ~ X.1 + X.2 + X.3)
    # Same but mess up both the predictor and outcomes
    dat.with.ref <- dat
    # change the name of the outcome and one of the predictors
    names(dat.with.ref)[length(dat)] <- "`My data`$Variables$Y"
    names(dat.with.ref)[2] <- "`Other data`$Variables$X.2"
    input.formula <- `My data`$Variables$Y ~ X.1 + `Other data`$Variables$X.2 + X.3
    bad.names <- c("`My data`$Variables$Y", "X.1", "`Other data`$Variables$X.2", "X.3")
    good.names <- c("Y", paste0("X.", 1:3))
    names(good.names) <- bad.names
    expected.check <- logical(4)
    names(expected.check) <- bad.names
    expected.check[c(1, 3)] <- TRUE
    expect_equal(reference.vector <- checkVariablesForDataSetNames(bad.names),
                 expected.check)
    expected.mapping <- good.names
    names(expected.mapping) <- bad.names
    expect_equal(removeDataSetReferences(reference.vector),
                 expected.mapping)
    expect_error(output.list <- relabelFormulaAndData(expected.mapping, input.formula, dat.with.ref),
                 NA)
    expect_setequal(names(output.list$data), c("Y", paste0("X.", 1:3)))
    expect_true(all(checkDataSame(output.list, dat)))
    expect_equal(output.list$formula, Y ~ X.1 + X.2 + X.3)
    # Now the same but give a variable conflict
    dat.with.ref <- dat
    names(dat.with.ref)[length(dat)] <- "`My data`$Variables$Y"
    names(dat.with.ref)[2] <- "`Other data`$Variables$X.1"
    names(dat.with.ref)[1] <- "`My data`$Variables$X.1"
    input.formula <- `My data`$Variables$Y ~ `My data`$Variables$X.1 + `Other data`$Variables$X.1 + X.3
    bad.names <- c("`My data`$Variables$Y", "`My data`$Variables$X.1", "`Other data`$Variables$X.1", "X.3")
    good.names <- c("Y", rep("X.1", 2), "X.3")
    names(good.names) <- bad.names
    expected.check <- logical(4)
    names(expected.check) <- bad.names
    expected.check[1:3] <- TRUE
    expect_equal(reference.vector <- checkVariablesForDataSetNames(bad.names),
                 expected.check)
    expected.mapping <- good.names
    expected.mapping[3] <- "X.1"
    names(expected.mapping) <- bad.names
    expect_equal(removeDataSetReferences(reference.vector),
                 expected.mapping)
    expected.dataset.names <- c(rep("My data", 2), "Other data")
    names(expected.dataset.names) <- bad.names[reference.vector]
    expect_equal(lookupDataSetNames(bad.names[reference.vector], dat.with.ref),
                 expected.dataset.names)
    expected.mapping[reference.vector] <- paste0(expected.mapping[reference.vector],
                                                 " from ",
                                                 expected.dataset.names)
    expect_error(output.list <- relabelFormulaAndData(expected.mapping, input.formula, dat.with.ref),
                 NA)
    expect_setequal(names(output.list$data),
                    c("X.1_from_My_data", "X.1_from_Other_data", "X.3", "Y_from_My_data"))
    # Check mapping was done appropriately, if so, the linear models produced on both versions of the data
    # should have identical coefficients
    long.lm <- lm(`\`My data\`$Variables$Y` ~ ., data = dat.with.ref)
    cleaned.lm <- lm(Y_from_My_data ~ ., data = output.list$data)
    expect_equal(unname(long.lm$coef), unname(cleaned.lm$coef))
    # Same as above with a weights column
    dat.with.ref <- dat
    names(dat.with.ref)[length(dat)] <- "`My data`$Variables$Y"
    names(dat.with.ref)[2] <- "`Other data`$Variables$X.1"
    names(dat.with.ref)[1] <- "`My data`$Variables$X.1"
    dat.with.ref$weights <- runif(nrow(dat))
    input.formula <- `My data`$Variables$Y ~ `My data`$Variables$X.1 + `Other data`$Variables$X.1 + X.3
    bad.names <- c("`My data`$Variables$Y", "`My data`$Variables$X.1", "`Other data`$Variables$X.1", "X.3")
    good.names <- c("Y", rep("X.1", 2), "X.3")
    names(good.names) <- bad.names
    expected.check <- logical(4)
    names(expected.check) <- bad.names
    expected.check[1:3] <- TRUE
    expect_equal(reference.vector <- checkVariablesForDataSetNames(bad.names),
                 expected.check)
    expected.mapping <- good.names
    names(expected.mapping) <- bad.names
    expect_equal(removeDataSetReferences(reference.vector),
                 expected.mapping)
    expected.dataset.names <- c(rep("My data", 2), "Other data")
    names(expected.dataset.names) <- bad.names[reference.vector]
    expect_equal(lookupDataSetNames(bad.names[reference.vector], dat.with.ref),
                 expected.dataset.names)
    expected.mapping[reference.vector] <- paste0(expected.mapping[reference.vector],
                                                 " from ",
                                                 expected.dataset.names)
    expect_error(output.list <- relabelFormulaAndData(expected.mapping, input.formula, dat.with.ref),
                 NA)
    expect_setequal(names(output.list$data),
                    c("X.1_from_My_data", "X.1_from_Other_data", "X.3", "Y_from_My_data", "weights"))
    long.lm <- lm(`\`My data\`$Variables$Y` ~ ., data = dat.with.ref)
    cleaned.lm <- lm(Y_from_My_data ~ ., data = output.list$data)
    expect_equal(unname(long.lm$coef), unname(cleaned.lm$coef))
})

test_that("Non-syntactic names in formula", {
    non.syntactic.names <- c("`Q1#A`", "X.1", "X.2", "X.3")
    expected.mapping <- c("X.Q1.A.", X.1 = "X.1", X.2 = "X.2", X.3 = "X.3")
    names(expected.mapping)[1] <- "`Q1#A`"
    expect_equal(checkForNonSyntacticNames(non.syntactic.names),
                 expected.mapping)
    expect_null(checkForNonSyntacticNames(c("Y", paste0("X.", 1:3))))
})

test_that("Non syntactic variable names and dataset references", {
    non.syntactic.names <- c("`Q1#A`", "`X@1`", "`X^2`", "X.3")
    dat <- MASS::mvrnorm(n = 50, mu = 1:4, Sigma = crossprod(matrix(c(1, 0.2, 0.3, 0.4,
                                                                      0.2, 1, 0.25, 0.1,
                                                                      0.3, 0.25, 1, 0.05,
                                                                      0.4, 0.1, 0.05, 1),
                                                                    byrow = TRUE, nrow = 4)))
    dataref <- "dat2$Variables$"
    non.syntactic.names <- paste0(dataref, non.syntactic.names)
    bad.formula <- as.formula(paste0(non.syntactic.names[1], " ~ ",
                                     paste0(non.syntactic.names[-1], collapse = " + ")))
    dat <- as.data.frame(dat)
    names(dat) <- non.syntactic.names
    fancy.labels <- paste0(c("My ", rep("Preds ", 3)),
                           gsub(paste0("`|dat2\\$Variables\\$"), "", non.syntactic.names), " fancy label!")
    for (i in seq_along(names(dat)))
        attr(dat[[i]], "label") <- fancy.labels[i]
    expect_error(model.using.names <- Regression(bad.formula,
                                                 data = dat,
                                                 outlier.prop.to.remove = 0.4),
                 NA)
    expect_error(model.using.labels <- Regression(bad.formula,
                                                  data = dat,
                                                  outlier.prop.to.remove = 0.4,
                                                  show.labels = TRUE),
                 NA)
    # Prevent pops
    mockery::stub(print.Regression, "print.htmlwidget", NULL)
    expect_error(print(model.using.names), NA)
    expect_error(print(model.using.labels), NA)
})

test_that("DS-3379: MASS::polr starting value not suitable", {
    dat <- readRDS("dat.with.bad.starting.value.rds")
    single.warn <- capture_warnings(Regression(y ~ ., data = dat,
                                               type = "Ordered Logit",
                                               missing = "Dummy variable adjustment"))
    expect_equal(single.warn, "glm.fit: fitted probabilities numerically 0 or 1 occurred")
})

test_that("DS-3618: Dummy adjusted models with outlier removal ", {
    set.seed(3618)
    dat <- MASS::mvrnorm(n = 50, mu = 1:5,
                         Sigma = crossprod(matrix(c(1,    0.2,  0.3,  0.4, 0.3,
                                                    0.2,    1, 0.25,  0.1, 0.2,
                                                    0.3, 0.25,    1, 0.05, 0.25,
                                                    0.4,  0.1, 0.05,    1,  0.3,
                                                    0.3,  0.2, 0.25,  0.3,    1),
                                                  byrow = TRUE, nrow = 5)))
    Y <- rnorm(nrow(dat))
    Y[1:2] <- c(-1, 1) * 100 #Outlier
    dat <- data.frame(Y = Y, dat)
    # Force some redundant structure in missing values
    is.na(dat[[2]]) <- 1:3
    is.na(dat[[3]]) <- 1:2
    is.na(dat[[4]]) <- 3:5
    is.na(dat[[5]]) <- 3
    dummy.adjusted.dat <- flipData::EstimationData(Y ~ ., data = dat,
                                                  missing = "Dummy variable adjustment")
    original.model.formula <- Y ~ X1 + X2 + X3 + X4 + X5
    model.formula.with.int <- Y ~ X1 + X2 + X3 + X4 + X5 +
        X1:group + X2:group + X3:group + X4:group + X5:group
    formulae <- list(formula = original.model.formula,
                     formula.with.interaction = model.formula.with.int)
    form.with.int <- original.model.formula
    dummy.formula <- updateDummyVariableFormulae(original.model.formula, form.with.int,
                                                 data = dummy.adjusted.dat[["estimation.data"]])
    dummy.formula <- dummy.formula[["formula"]]

    first.dummy.model <- fitModel(dummy.formula, dummy.adjusted.dat[["estimation.data"]],
                                  .weights = NULL, type = "Linear", robust.se = FALSE,
                                  subset = NULL)
    dummy.adjusted.dat[["formulae"]] <- formulae
    model <- first.dummy.model[["model"]]
    ed <- first.dummy.model[["estimation.data"]]
    first.formula <- first.dummy.model[["formula"]]
    refit.model <- refitModelWithoutOutliers(model = model, formula = first.formula,
                                             .estimation.data = ed, .weights = NULL,
                                             type = "Linear", robust.se = FALSE, outlier.prop.to.remove = 0.05,
                                             dummy.processed.data = dummy.adjusted.dat)
    expect_false(all(refit.model[[".estimation.data"]][["non.outlier.data_GQ9KqD7YOf"]]))
    new.model <- refit.model[["model"]]
    first.model.fit <- first.dummy.model[["model"]]
    # Except the 2nd and 3rd obs to be identified as outliers
    expect_equal(refit.model[["non.outlier.data"]], as.logical(rep(c(1, 0, 1), c(1, 2, 50 - 3))))
    expect_setequal(names(coef(first.model.fit)),
                    c("(Intercept)", paste0("X", 1:5), paste0("X", 1:4, ".dummy.var_GQ9KqD7YOf")))
    # Expect the 2nd and 4th dummy variable to be removed after outliers removed since thy are singular.
    expect_setequal(setdiff(names(coef(first.model.fit)), names(coef(new.model))),
                    paste0("X", c(2, 4), ".dummy.var_GQ9KqD7YOf"))
})

test_that("DS-4188: No non-zero data after filtering and handling missing data in dummy adjustment", {
    set.seed(4188)
    Y <- rep(1:5, each = 10)
    X1 <- 1L * (runif(50L) > Y / 6)
    is.na(X1) <- sample.int(50L, size = 10L)
    dat <- data.frame(Y, X1, X2 = NA)
    # Create variable with only one non-zero value after subsetting
    non.zero.x1 <- which.min(X1 != 0L)
    dat[["X2"]][non.zero.x1] <- 0L
    dat.filter <- logical(50L)
    dat.filter[c(non.zero.x1, sample.int(50L, size = 25L))] <- TRUE
    expected.warn <- paste0("The following variable(s) are colinear with other variables and no ",
                            "coefficients have been estimated: 'X2'")
    expect_warning(reg <- Regression(Y ~ X1 + X2, data = dat, missing = "Dummy variable adjustment",
                                     subset = dat.filter),
                   expected.warn, fixed = TRUE)
    dat[["Y.ord"]] <- as.ordered(dat[["Y"]])
    expected.warn <- paste0("The following variable(s) are colinear with other variables and no ",
                            "coefficients have been estimated: 'X2'")
    expect_warning(reg <- Regression(Y.ord ~ X1 + X2, data = dat, missing = "Dummy variable adjustment",
                                     subset = dat.filter, type = "Ordered Logit"),
                   expected.warn, fixed = TRUE)
    expected.error <- paste0("Each predictor needs to have at least two unique values to conduct a ",
                             "Relative Importance Analysis. The predictor 'X2' is constant and has ",
                             "no variation.")
    # Additional earlier warning is generated about colinear variable being removed before RIA validates
    # the input matrix
    expect_warning(
        expect_error(Regression(Y.ord ~ X1 + X2, data = dat,
                                missing = "Dummy variable adjustment",
                                subset = dat.filter, type = "Ordered Logit",
                                output = "Relative Importance Analysis"),
                   expected.error, fixed = TRUE),
        expected.warn, fixed = TRUE)
})

test_that("DS-4889: Unable to compute anova on weighted models with interaction", {
    mockery::stub(computeInteractionCrosstab, "anova", function(object, ...) stop("Error"))
    some.weights <- runif(nrow(mtcars))
    test.dat <- mtcars
    test.dat[["gear"]] <- as.factor(test.dat[["gear"]])
    result <- FitRegression(
        .formula = mpg ~ cyl + gear,
        .estimation.data = test.dat,
        .weights = some.weights,
        type = "Linear",
        robust.se = FALSE,
        outlier.prop.to.remove = NULL
    )
    result[["terms"]] <- result$original$terms
    result[["missing"]] <- "Exclude cases with missing data"
    result[["show.labels"]] <- TRUE
    result[["weights"]] <- some.weights
    result[["subset"]] <- TRUE
    result[["type"]] <- "Linear"
    result[["estimation.data"]] <- test.dat
    result[["correction"]] <- "None"
    interaction.formula <- mpg ~ cyl + gear + cyl:gear
    expect_error(
        computeInteractionCrosstab(
            result = result,
            interaction.name = "gear",
            interaction.label = "Some gears",
            formula.with.interaction = interaction.formula,
            importance = NULL,
            importance.absolute = NULL,
            internal.loop = FALSE
        ),
        "The F-test could not be computed for this interaction.",
        fixed = TRUE
    )
    assign("productName", "Displayr", envir = .GlobalEnv)
    expect_error(
        computeInteractionCrosstab(
            result = result,
            interaction.name = "gear",
            interaction.label = "Some gears",
            formula.with.interaction = interaction.formula,
            importance = NULL,
            importance.absolute = NULL,
            internal.loop = FALSE
        ),
        "The F-test could not be computed for this interaction. Please contact support@displayr.com"
    )
    mockery::stub(computeInteractionCrosstab, "anova",
         function(object, ...) stop("Non-numeric argument to mathematical function"))
    expect_error(observed.warning <- capture_warnings(
            computeInteractionCrosstab(
                result = result,
                interaction.name = "gear",
                interaction.label = "Some gears",
                formula.with.interaction = interaction.formula,
                importance = NULL,
                importance.absolute = NULL,
                internal.loop = FALSE
            ),
        ),
        NA
    )
    expect_equal(
        observed.warning,
        paste0("The F-Test could not be computed for this Crosstab interaction. ",
               "This is probably because too many cells in the interaction are ",
               "empty or contain a single observation. Try combining categories ",
               "in the model which contain small numbers of observations to increase ",
               "the sample sizes for cells in the Crosstab interaction."),
        fixed = TRUE
    )
    rm("productName", envir = .GlobalEnv)
})

test_that("Removing missing entirely missing variables", {
    some.data <- data.frame(
        y = runif(5),
        x1 = runif(5),
        x2 = rep(NA, 5),
        x3 = factor(sample(letters[1:2], size = 5, replace = TRUE))
    )
    formula <- y ~ x1 + x2
    missing.variables <- c(y = FALSE, x1 = FALSE, x2 = TRUE)
    out <- removeMissingVariables(data = some.data, formula = formula, missing.variables = missing.variables) |>
        expect_warning(
            paste0(
                "Data has variable(s) that are entirely missing values (all observed values of the variable are ",
                "missing). These variable(s) have been removed from the analysis: x2."
            ),
            fixed = TRUE
        )
    out |> expect_equal(
        list(
            data = some.data[, !colnames(some.data) %in% c("x2", "x3")],
            formula = y ~ x1,
            formula.with.interaction = y ~ x1
        )
    )
    # Check interaction
    out.interaction <- removeMissingVariables(data = some.data, formula = formula, interaction = "x3",
                                              missing.variables = missing.variables) |>
        expect_warning(
            paste0(
                "Data has variable(s) that are entirely missing values (all observed values of the variable are ",
                "missing). These variable(s) have been removed from the analysis: x2."
            ),
            fixed = TRUE
        )
    out.interaction |> expect_equal(
        list(
            data = some.data[, !colnames(some.data) %in% c("x2")],
            formula = y ~ x1,
            formula.with.interaction = y ~ x1 + x3 + x1:x3
        )
    )
    # Check error if missing outcome
    some.data <- data.frame(y = NA, x1 = runif(5))
    missing.variables <- c(y = TRUE, x1 = FALSE)
    removeMissingVariables(data = some.data, formula = y ~ x1, missing.variables = missing.variables) |>
        expect_error(
            "Outcome variable is entirely missing (all observed values of the variable are missing).",
            fixed = TRUE
        )

    some.data <- data.frame(
        y = runif(5),
        x1 = NA,
        x11 = runif(5),
        x3 = runif(5),
        x2 = factor(sample(letters[1:2], size = 5, replace = TRUE))
    )
    formula <- y ~ x1 + x11 + x3
    interaction <- "x2"
    missing.variables <- c(y = FALSE, x1 = TRUE, x11 = FALSE, x3 = FALSE)
    removeMissingVariables(data = some.data, formula = y ~ x1 + x11 + x3, interaction = "x2",
                           missing.variables = missing.variables) |>
        expect_warning(
            paste0(
                "Data has variable(s) that are entirely missing values (all observed values of the variable are ",
                "missing). These variable(s) have been removed from the analysis: x1."
            ),
            fixed = TRUE
        ) |>
        expect_equal(
            list(
                data = some.data[, !colnames(some.data) %in% c("x1")],
                formula = y ~ x11 + x3,
                formula.with.interaction = y ~ x11 + x3 + x2 + x11:x2 + x3:x2
            )
        )
})
