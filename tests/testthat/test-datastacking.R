# Test the new stackable data feature

# Input and processed Multi outcome variable sets
binary.multi.outcome <- structure(list(Apple = c(0, 1), Microsoft = c(1, 1), NET = c(1, 1)),
                                  questiontype = "PickAny", question = "Brand Binary",
                                  row.names = 1:2, class = "data.frame")
binary.multi.outcome.cleaned <- structure(list(Apple = c(0, 1), Microsoft = c(1, 1)),
                                          questiontype = "PickAny", question = "Brand Binary",
                                          row.names = 1:2, class = "data.frame")
nominal.multi.outcome <- structure(list(Apple = structure(c(1, 2), .Label = c("Low", "High"), class = "factor"),
                                        Microsoft = structure(c(2, 2), .Label = c("Low", "High"), class = "factor")),
                                   questiontype = "PickOneMulti", question = "Brand Nominal",
                                   row.names = 1:2, class = "data.frame")
ordinal.multi.outcome <- structure(list(Apple = structure(c(1, 2), .Label = c("Low", "High"), class = c("ordered", "factor")),
                                        Microsoft = structure(c(2, 2), .Label = c("Low", "High"), class = c("ordered", "factor"))),
                                   questiontype = "PickOneMulti", question = "Brand Ordinal",
                                   row.names = 1:2, class = "data.frame")
numeric.multi.outcome <- structure(list(Apple = c(6, 8), Microsoft = c(10, 8), SUM = c(16, 16)),
                                   questiontype = "NumberMulti", question = "Brand Numeric",
                                   row.names = 1:2, class = "data.frame")
numeric.multi.outcome.cleaned <- structure(list(Apple = c(6, 8), Microsoft = c(10, 8)),
                                           questiontype = "NumberMulti", question = "Brand Numeric",
                                           row.names = 1:2, class = "data.frame")
larger.numeric.multi.outcome <- structure(list(Apple = c(6, 8), Amazon = c(33, 66),
                                               Google = c(12, 24), Microsoft = c(10, 8), SUM = c(61, 106)),
                                          questiontype = "NumberMulti", question = "Brand Numeric",
                                          row.names = 1:2, class = "data.frame")

# Input and processed Grid variable sets
numeric.grid <- structure(list(`Fun, Apple` = c(0, 12), `Fun, Microsoft` = c(11, 0),
                               `Fun, SUM` = c(11, 12),
                               `Innovative, Apple` = c(15, 9), `Innovative, Microsoft` = c(9, 19),
                               `Innovative, SUM` = c(24, 28), `SUM, Apple` = c(15, 21), `SUM, Microsoft` = c(20, 19),
                               `SUM, SUM` = c(35, 40)),
                          class = "data.frame", row.names = 1:2,
                          questiontype = "NumberGrid", question = "Qualities Numeric")
transposed.numeric.grid <- structure(list(`Apple, Fun` = c(0, 12), `Apple, Innovative` = c(15, 9),
                                          `Apple, SUM` = c(15, 21),
                                          `Microsoft, Fun` = c(11, 0), `Microsoft, Innovative` = c(9, 19),
                                          `Microsoft, SUM` = c(20, 19),
                                          `SUM, Fun` = c(11, 12), `SUM, Innovative` = c(24, 28),
                                          `SUM, SUM` = c(35, 40)),
                                     class = "data.frame", row.names = 1:2,
                                     questiontype = "NumberGrid", question = "Qualities Numeric")
ambiguous.numeric.grid <- structure(list(`Fun, Apple` = c(0, 12), `Fun, Microsoft` = c(11, 0),
                                         `Fun, SUM` = c(11, 12),
                                         `Apple, Apple` = c(15, 9), `Apple, Microsoft` = c(9, 19),
                                         `Apple, SUM` = c(24, 28), `SUM, Apple` = c(15, 21), `SUM, Microsoft` = c(20, 19),
                                         `SUM, SUM` = c(35, 40)),
                                    class = "data.frame", row.names = 1:2,
                                    questiontype = "NumberGrid", question = "Qualities Numeric")
transposed.ambiguous.numeric.grid <- structure(list(`Apple, Fun` = c(0, 12), `Microsoft, Fun` = c(11, 0),
                                                    `SUM, Fun` = c(11, 12),
                                                    `Apple, Apple` = c(15, 9), `Microsoft, Apple` = c(9, 19),
                                                    `SUM, Apple` = c(24, 28), `Apple, SUM` = c(15, 21), `Microsoft, SUM` = c(20, 19),
                                                    `SUM, SUM` = c(35, 40)),
                                               class = "data.frame", row.names = 1:2,
                                               questiontype = "NumberGrid", question = "Qualities Numeric")
numeric.grid.cleaned <- structure(list(`Fun, Apple` = c(0, 12), `Fun, Microsoft` = c(11, 0),
                                       `Innovative, Apple` = c(15, 9), `Innovative, Microsoft` = c(9, 19)),
                                  class = "data.frame", row.names = 1:2,
                                  questiontype = "NumberGrid", question = "Qualities Numeric")
misaligned.larger.numeric.grid <- structure(list(`Fun, Apple` = c(0, 12), `Fun, Microsoft` = c(11, 0),
                                                 `Fun, Google` = c(7, 25), `Fun, Amazon` = c(18, 2),
                                                 `Innovative, Apple` = c(15, 9), `Innovative, Microsoft` = c(9, 19),
                                                 `Innovative, Google` = c(13, 91), `Innovative, Amazon` = c(50, 44)),
                                            class = "data.frame", row.names = 1:2,
                                            questiontype = "NumberGrid", question = "Qualities Numeric")
larger.numeric.grid <- structure(list(`Fun, Apple` = c(0, 12), `Fun, Amazon` = c(18, 2),
                                      `Fun, Google` = c(7, 25), `Fun, Microsoft` = c(11, 0),
                                      `Innovative, Apple` = c(15, 9), `Innovative, Amazon` = c(50, 44),
                                      `Innovative, Google` = c(13, 91), `Innovative, Microsoft` = c(9, 19)),
                                 class = "data.frame", row.names = 1:2,
                                 questiontype = "NumberGrid", question = "Qualities Numeric")

binary.grid <- structure(list(`Fun, Apple` = c(0, 1), `Fun, Microsoft` = c(1, 0),
                               `Fun, NET` = c(1, 1),
                               `Innovative, Apple` = c(1, 1), `Innovative, Microsoft` = c(1, 1),
                               `Innovative, NET` = c(1, 1), `NET, Apple` = c(1, 2), `NET, Microsoft` = c(1, 1),
                               `NET, NET` = c(1, 1)),
                          class = "data.frame", row.names = 1:2,
                          questiontype = "PickAnyGrid", question = "Qualities Binary")
binary.grid.cleaned <- structure(list(`Fun, Apple` = c(0, 1), `Fun, Microsoft` = c(1, 0),
                                       `Innovative, Apple` = c(1, 1), `Innovative, Microsoft` = c(1, 1)),
                                  class = "data.frame", row.names = 1:2,
                                  questiontype = "PickAnyGrid", question = "Qualities Binary")

numeric.grid.commas <- structure(list(`Fun, Apple, Oranges & Grapes` = c(0, 12), `Fun, Microsoft` = c(11, 0),
                                      `Innovative, Apple, Oranges & Grapes` = c(15, 9), `Innovative, Microsoft` = c(9, 19)),
                                 class = "data.frame", row.names = 1:2,
                                 questiontype = "NumberGrid", question = "Qualities Numeric")

# Expected Stacked data

binary.binary.stacked <-structure(list(Y = structure(c(0, 1, 1, 1), label = "Brand Binary"),
                                       X1 = structure(c(0, 1, 1, 0), label = "Qualities Binary: Fun"),
                                       X2 = structure(c(1, 1, 1, 1), label = "Qualities Binary: Innovative")),
                                  class = "data.frame", row.names = c("1.Apple", "2.Apple", "1.Microsoft", "2.Microsoft"))
binary.numeric.stacked <- structure(list(Y = structure(c(0, 1, 1, 1), label = "Brand Binary"),
                                         X1 = structure(c(0, 12, 11, 0), label = "Qualities Numeric: Fun"),
                                         X2 = structure(c(15, 9, 9, 19), label = "Qualities Numeric: Innovative")),
                                    class = "data.frame", row.names = c("1.Apple", "2.Apple", "1.Microsoft", "2.Microsoft"))
nominal.binary.stacked <- structure(list(Y = structure(c(1L, 2L, 2L, 2L), .Label = c("Low", "High"),
                                                       class = "factor", label = "Brand Nominal"),
                                         X1 = structure(c(0, 1, 1, 0), label = "Qualities Binary: Fun"),
                                         X2 = structure(c(1, 1, 1, 1), label = "Qualities Binary: Innovative")),
                                    class = "data.frame", row.names = c("1.Apple", "2.Apple", "1.Microsoft", "2.Microsoft"))
nominal.numeric.stacked <- structure(list(Y = structure(c(1L, 2L, 2L, 2L), .Label = c("Low", "High"),
                                                        class = "factor", label = "Brand Nominal"),
                                          X1 = structure(c(0, 12, 11, 0), label = "Qualities Numeric: Fun"),
                                          X2 = structure(c(15, 9, 9, 19), label = "Qualities Numeric: Innovative")),
                                     class = "data.frame", row.names = c("1.Apple", "2.Apple", "1.Microsoft", "2.Microsoft"))
ordinal.binary.stacked <- structure(list(Y = structure(c(1L, 2L, 2L, 2L), .Label = c("Low", "High"),
                                                       class = c("ordered", "factor"), label = "Brand Ordinal"),
                                         X1 = structure(c(0, 1, 1, 0), label = "Qualities Binary: Fun"),
                                         X2 = structure(c(1, 1, 1, 1), label = "Qualities Binary: Innovative")),
                                    class = "data.frame", row.names = c("1.Apple", "2.Apple", "1.Microsoft", "2.Microsoft"))
ordinal.numeric.stacked <- structure(list(Y = structure(c(1L, 2L, 2L, 2L), .Label = c("Low", "High"),
                                                        class = c("ordered", "factor"), label = "Brand Ordinal"),
                                          X1 = structure(c(0, 12, 11, 0), label = "Qualities Numeric: Fun"),
                                          X2 = structure(c(15, 9, 9, 19), label = "Qualities Numeric: Innovative")),
                                     class = "data.frame", row.names = c("1.Apple", "2.Apple", "1.Microsoft", "2.Microsoft"))
numeric.binary.stacked <- structure(list(Y = structure(c(6, 8, 10, 8), label = "Brand Numeric"),
                                         X1 = structure(c(0, 1, 1, 0), label = "Qualities Binary: Fun"),
                                         X2 = structure(c(1, 1, 1, 1), label = "Qualities Binary: Innovative")),
                                    class = "data.frame", row.names = c("1.Apple", "2.Apple", "1.Microsoft", "2.Microsoft"))
numeric.numeric.stacked <- structure(list(Y = structure(c(6, 8, 10, 8), label = "Brand Numeric"),
                                          X1 = structure(c(0, 12, 11, 0), label = "Qualities Numeric: Fun"),
                                          X2 = structure(c(15, 9, 9, 19), label = "Qualities Numeric: Innovative")),
                                     class = "data.frame", row.names = c("1.Apple", "2.Apple", "1.Microsoft", "2.Microsoft"))

test_that("Test input error messages", {
    # Test formula error when none provided outside stacking context
    error.msg <- paste0(dQuote("formula"), " argument is missing and is required unless stackable data is provided ",
                               "via the ", dQuote("stacked.data.check"), " and ", dQuote("unstacked.data"),
                               " arguments. Please provide a formula or stackable data and re-run the Regression")
    expect_error(Regression(), error.msg)
    # Test default incorrect input format
    error.msg <- paste0("'unstacked.data' needs to be a list with two elements, ",
                        "'Y' containing a data.frame with the outcome variables and ",
                        "'X' containing a data.frame with the predictor variables.")
    expect_error(Regression(stacked.data.check = TRUE, unstacked.data = NULL), error.msg)
    error.msg <- paste0("Outcome variable to be stacked needs to be a data.frame. " ,
                        "Please assign a data.frame to the \"Y\" element of the 'unstacked.data' argument.")
    expect_error(Regression(stacked.data.check = TRUE, unstacked.data = list(X = 1:5, Y = 1:5)), error.msg)
    error.msg <- paste0("Predictor variables to be stacked needs to be a data.frame. " ,
                        "Please assign a data.frame to the \"X\" element of the 'unstacked.data' argument.")
    expect_error(Regression(stacked.data.check = TRUE, unstacked.data = list(Y = data.frame(y1 = 1:5, y2 = 1:5),
                                                                             X = 1:5)), error.msg)
    error.msg <- paste0("Size of variables doesn't agree, the provided outcome variables have 2 observations ",
                        "while the provided predictor variables have 3 observations. ",
                        "Please input variables that have the same size.")
    # Test different sizes inputs
    expect_error(Regression(stacked.data.check = TRUE, unstacked.data = list(Y = data.frame(1:2),
                                                                                              X = data.frame(1:3))),
                 error.msg)
    error.msg <- paste0("Size of variables doesn't agree, the provided outcome variables ",
                        "have 2 observations while the provided predictor variables ",
                        "have 1 observations. Please input variables that have the same size.")
    expect_error(Regression(stacked.data.check = TRUE,
                            unstacked.data = list(Y = binary.multi.outcome,
                                                  X = binary.grid.cleaned[1, ])),
                 error.msg, fixed = TRUE)
    # Test completely mismatched variables (no outcome labels match predictor labels)
    binary.mismatch.outcome <- binary.multi.outcome
    names(binary.mismatch.outcome)[1:2] <- c("Google", "Amazon")
    error.msg <- paste0("It is not possible to stack these variables since none of the outcome variable labels match ",
                        "the variable labels in the predictor variables. The outcome variables ",
                        sQuote("Brand Binary"), " have labels: ", paste0(sQuote(c("Google", "Amazon")), collapse = ", "),
                        " which don't appear in the labels of the grid of predictor variables.")
    expect_error(Regression(stacked.data.check = TRUE,
                            unstacked.data = list(Y = binary.mismatch.outcome,
                                                  X = binary.grid.cleaned)),
                 error.msg, fixed = TRUE)
    binary.mismatch.outcome <- binary.multi.outcome
    names(binary.mismatch.outcome)[1:2] <- c("Fun", "Microsoft")
    error.msg <- paste0("Ambiguous labels in the grid predictors need to be reconciled before stacking can occur. ",
                        "The outcome variable ", sQuote("Brand Binary"), " has labels: ",
                        paste0(sQuote(c("Fun", "Microsoft")), collapse = ", "), " and these labels appear in both ",
                        "dimensions of the grid predictor variables. Please rename the labels ",
                        "in either the outcome variables or grid predictor variables to stack the variables ",
                        "and proceed.")
    expect_error(Regression(stacked.data.check = TRUE,
                            unstacked.data = list(Y = binary.mismatch.outcome,
                                                  X = binary.grid.cleaned)),
                 error.msg, fixed = TRUE)
    # Check outcome input appropriate
    outcome.without.attributes <- numeric.multi.outcome[1:ncol(numeric.multi.outcome.cleaned)]
    expect_warning(Regression(stacked.data.check = TRUE,
                              unstacked.data = list(Y = outcome.without.attributes,
                                                    X = binary.grid.cleaned)),
                   "The following variable(s) are colinear", fixed = TRUE)
    # Check Grid input appropriate
    predictor.without.attributes <- numeric.grid.cleaned[1:ncol(numeric.grid.cleaned)]
    expect_error(Regression(stacked.data.check = TRUE,
                            unstacked.data = list(Y = numeric.multi.outcome,
                                                  X = predictor.without.attributes)),
                 NA)
    # Check error is thrown if grid data.frame has variable names with commas
    ambig.names <- paste0(sQuote(c("Fun, Apple, Oranges & Grapes", "Innovative, Apple, Oranges & Grapes")),
                          collapse = ", ")
    error.msg <- paste0("The variable labels in the predictor grid should be comma separated to determine the columns ",
                        "that belong to the appropriate outcome variable. This means that the variable labels cannot ",
                        "use commas. Please remove the commas in the names in the predictor grid to continue ",
                        "the analysis. The variable labels that are ambiguous and require fixing are: ",
                        ambig.names)
    expect_error(Regression(stacked.data.check = TRUE,
                            unstacked.data = list(Y = numeric.multi.outcome,
                                                  X = numeric.grid.commas)),
                 )
})

test_that("check codeframe", {
    code.frame <- list(V1 = 0, V2 = 1, V3 = 2)
    expect_identical(flipRegression:::flagCodeframeReduction(code.frame),
                     c(V1 = FALSE, V2 = FALSE, V3 = FALSE))
    code.frame <- list(V1 = 0, V2 = 1, V3 = 2, SUM = 0:2)
    expect_identical(flipRegression:::flagCodeframeReduction(code.frame),
                     c(V1 = FALSE, V2 = FALSE, V3 = FALSE, SUM = TRUE))
    code.frame <- list(V1 = 0, V2 = 1, V3 = 2, SUM = 0:2)
    expect_identical(flipRegression:::flagCodeframeReduction(code.frame),
                     c(V1 = FALSE, V2 = FALSE, V3 = FALSE, SUM = TRUE))
    code.frame <- list(V1 = 0, V2 = c(1, 2), V3 = 2, SUM = 0:2)
    expect_identical(flipRegression:::flagCodeframeReduction(code.frame),
                     c(V1 = FALSE, V2 = TRUE, V3 = FALSE, SUM = TRUE))
    code.frame <- list(V1 = 0, Aggregate = 2:0, V2 = c(1, 2), V3 = 2, SUM = 0:2)
    expect_identical(flipRegression:::flagCodeframeReduction(code.frame),
                     c(V1 = FALSE, Aggregate = TRUE, V2 = TRUE, V3 = FALSE, SUM = TRUE))
    code.frame <- list(Aggregate = 2:0, V2 = c(1, 2), V3 = 2, SUM = 0:2)
    expect_identical(flipRegression:::flagCodeframeReduction(code.frame),
                     c(Aggregate = TRUE, V2 = TRUE, V3 = FALSE, SUM = TRUE))
})

# Test warnings for slight mismatches in data
test_that("Mismatch warnings", {
    # Add extra variable(s) to the outcome
    extra.outcome <- binary.multi.outcome.cleaned
    extra.outcome$NET <- NULL
    extra.outcome$Amazon <- c(1, 0)
    extra.outcome$NET <- apply(extra.outcome, 1, function(x) as.numeric(any(as.logical(x))))
    warning.msg <- paste0("The variable(s): ", sQuote("Amazon"), " have been removed from the set of outcome variables in ",
                          sQuote("Brand Binary"), " since they don't appear in the set of predictor variables in ",
                          sQuote("Qualities Binary"))
    expect_warning(output <- Regression(stacked.data.check = TRUE,
                                        unstacked.data = list(Y = extra.outcome,
                                                              X = binary.grid),
                                        method = "model.frame"),
                   warning.msg, fixed = TRUE)
    # Check warning when question attributes don't exist
    # Also it should include NET since it no question attributes exist, the NET could be a user predictor
    # and not a data reduction
    extra.outcome.without.attributes <- extra.outcome[1:ncol(extra.outcome)]
    removed.vars <- paste0(sQuote(c("Amazon", "NET")), collapse = ", ")
    warning.msg <- paste0("The variable(s): ", removed.vars, " have been removed from the set of outcome variables ",
                          "since these variables don't appear in the set of predictor variables")
    expect_warning(output <- Regression(stacked.data.check = TRUE,
                                        unstacked.data = list(Y = extra.outcome.without.attributes,
                                                              X = binary.grid),
                                        method = "model.frame"),
                   warning.msg, fixed = TRUE)
    expect_equivalent(output, binary.binary.stacked)
    extra.outcome$NET <- NULL
    extra.outcome$Walmart <- c(1, 0)
    extra.outcome$NET <- apply(extra.outcome, 1, function(x) as.numeric(any(as.logical(x))))
    warning.msg <- paste0("The variable(s): ", paste0(sQuote(c("Amazon", "Walmart")), collapse = ", "),
                          " have been removed from the set of outcome variables in ", sQuote("Brand Binary"),
                          " since they don't appear in the set of predictor variables in ",
                          sQuote("Qualities Binary"))
    expect_warning(output <- Regression(stacked.data.check = TRUE,
                                        unstacked.data = list(Y = extra.outcome,
                                                              X = binary.grid),
                                        method = "model.frame"),
                   warning.msg, fixed = TRUE)
    expect_identical(output, binary.binary.stacked)
    # Check extra variables in predictor grid are removed.
    extra.numeric.grid <- structure(list(`Fun, Apple` = c(0, 12), `Fun, Microsoft` = c(11, 0), `Fun, Amazon` = c(5, 0),
                                         `Fun, SUM` = c(16, 12),
                                         `Innovative, Apple` = c(15, 9), `Innovative, Microsoft` = c(9, 19),
                                         `Innovative, Amazon` = c(0, 5), `Innovative, SUM` = c(24, 33),
                                         `SUM, Apple` = c(15, 21), `SUM, Microsoft` = c(20, 19), `SUM, Amazon` = c(5, 5),
                                         `SUM, SUM` = c(40, 45)),
                              class = "data.frame", row.names = 1:2,
                              questiontype = "NumberGrid", question = "Qualities Numeric")
    warning.msg <- paste0("The variable(s): ", sQuote("Amazon"), " have been removed from the set of predictor variables in ",
                          sQuote("Qualities Numeric"), " since they don't appear in the set of outcome variables in ",
                          sQuote("Brand Numeric"))
    expect_warning(output <- Regression(stacked.data.check = TRUE,
                                        unstacked.data = list(Y = numeric.multi.outcome,
                                                              X = extra.numeric.grid),
                                        method = "model.frame"),
                   warning.msg, fixed = TRUE)
    # Check message when variable set structure name not available, unavailable question name stripped off
    # and data reduction appears.
    extra.numeric.grid <- extra.numeric.grid[1:ncol(extra.numeric.grid)]
    warning.msg <- paste0("The variable(s): ", paste0(sQuote(c("Amazon", "SUM")), collapse = ", "),
                          " have been removed from the set of predictor variables since these variables ",
                          "don't appear in the outcome variables.")
    expect_warning(Regression(stacked.data.check = TRUE,
                              unstacked.data = list(Y = numeric.multi.outcome,
                                                    X = extra.numeric.grid),
                              method = "model.frame"),
                   warning.msg, fixed = TRUE)
    # Check message when variable set structure name not available

    expect_identical(output, numeric.numeric.stacked)
    # Test warnings for ambiguous input
    error.msg <- paste0("Ambiguous labels in the grid predictors need to be reconciled before stacking can occur. ",
                        "The outcome variable ", sQuote("Brand Numeric"), " has labels: ",
                        paste0(sQuote(c("Apple", "Microsoft")), collapse = ", "), " and these labels appear ",
                        "in both dimensions of the grid predictor variables. Please rename the ",
                        "labels in either the outcome variables or grid predictor variables to ",
                        "stack the variables and proceed.")
    expect_error(ambiguous.output.1 <- Regression(stacked.data.check = TRUE,
                                                  unstacked.data = list(Y = numeric.multi.outcome,
                                                                        X = ambiguous.numeric.grid),
                                                  method = "model.frame"),
                 error.msg, fixed = TRUE)
})

test_that("Transpose and alignment correct", {
    # Check transposing
    expect_identical(Regression(formula(NULL), stacked.data.check = TRUE,
                                unstacked.data = list(Y = numeric.multi.outcome,
                                                      X = transposed.numeric.grid),
                                method = "model.frame"),
                     Regression(formula(NULL), stacked.data.check = TRUE,
                                unstacked.data = list(Y = numeric.multi.outcome,
                                                      X = numeric.grid),
                                method = "model.frame"))
    # Check alignment
    original.data.frame <- Regression(formula(NULL), stacked.data.check = TRUE,
                                      unstacked.data = list(Y = larger.numeric.multi.outcome,
                                                            X = larger.numeric.grid),
                                      method = "model.frame")
    data.frame.after.alignment <- Regression(formula(NULL), stacked.data.check = TRUE,
                                             unstacked.data = list(Y = larger.numeric.multi.outcome,
                                                                   X = misaligned.larger.numeric.grid),
                                             method = "model.frame")
    # Compute least squares coefficients
    original.lm <- lm(Y ~ ., data = original.data.frame)
    aligned.lm <- lm(Y ~ ., data = data.frame.after.alignment)
    expect_equal(original.lm$coefficients, aligned.lm$coefficients)
})

# Test Data Reduction columns are removed successfully.
test_that("Data Reduction for Multi and Grid inputs", {
    expect_identical(list(Y = binary.multi.outcome.cleaned, X = binary.grid.cleaned),
                     flipRegression:::removeDataReduction(list(Y = binary.multi.outcome,
                                                               X = binary.grid)))
    expect_identical(list(Y = nominal.multi.outcome, X = numeric.grid.cleaned),
                     flipRegression:::removeDataReduction(list(Y = nominal.multi.outcome,
                                                               X = numeric.grid)))
    expect_identical(list(Y = ordinal.multi.outcome, X = numeric.grid.cleaned),
                     flipRegression:::removeDataReduction(list(Y = ordinal.multi.outcome,
                                                               X = numeric.grid)))
    expect_identical(list(Y = numeric.multi.outcome.cleaned, X = numeric.grid.cleaned),
                     flipRegression:::removeDataReduction(list(Y = numeric.multi.outcome,
                                                               X = numeric.grid)))
})

test_that("Valid stackable data converted", {
    valid.outcome.vars <- c("binary.multi.outcome", "nominal.multi.outcome",
                            "ordinal.multi.outcome", "numeric.multi.outcome")
    valid.predictor.vars <- c("binary.grid", "numeric.grid")
    output.stacked.dfs <- paste0(rep(c("binary", "nominal", "ordinal", "numeric"), rep(2, 4)), ".",
                                 rep(c("binary", "numeric"), 4), ".stacked")
    stacked.index <- matrix(1:(length(valid.outcome.vars) * length(valid.predictor.vars)),
                            nrow = length(valid.outcome.vars), byrow = TRUE)
    for (i in seq_along(valid.outcome.vars))
        for (j in seq_along(valid.predictor.vars))
            expect_identical(Regression(stacked.data.check = TRUE,
                                        unstacked.data = list(Y = get(valid.outcome.vars[i]),
                                                              X = get(valid.predictor.vars[j])), method = "model.frame"),
                             get(output.stacked.dfs[stacked.index[i, j]]))
})

# Test Stacking output is consistent with already stacked data output
data(technology.unstacked, package = "flipRegression")

# Create the stacked data from the unstacked data for comparison
variables.to.stack <- list(Q3 = c("Apple", "Microsoft", "IBM", "Google", "Intel", "Hewlett-Packard",
                                  "Sony", "Dell", "Yahoo", "Nokia", "Samsung", "LG", "Panasonic"),
                           Q4a = c("Fun, Apple", "Fun, Microsoft",
                                   "Fun, IBM", "Fun, Google", "Fun, Intel", "Fun, Hewlett-Packard",
                                   "Fun, Sony", "Fun, Dell", "Fun, Yahoo", "Fun, Nokia", "Fun, Samsung",
                                   "Fun, LG", "Fun, Panasonic"),
                           Q4b = c("Worth what you pay for, Apple",
                                   "Worth what you pay for, Microsoft", "Worth what you pay for, IBM",
                                   "Worth what you pay for, Google", "Worth what you pay for, Intel",
                                   "Worth what you pay for, Hewlett-Packard", "Worth what you pay for, Sony",
                                   "Worth what you pay for, Dell", "Worth what you pay for, Yahoo",
                                   "Worth what you pay for, Nokia", "Worth what you pay for, Samsung",
                                   "Worth what you pay for, LG", "Worth what you pay for, Panasonic"),
                           Q4c = c("Innovative, Apple",
                                   "Innovative, Microsoft", "Innovative, IBM", "Innovative, Google",
                                   "Innovative, Intel", "Innovative, Hewlett-Packard", "Innovative, Sony",
                                   "Innovative, Dell", "Innovative, Yahoo", "Innovative, Nokia",
                                   "Innovative, Samsung", "Innovative, LG", "Innovative, Panasonic"),
                           Q4d = c("Good customer service, Apple",
                                   "Good customer service, Microsoft", "Good customer service, IBM",
                                   "Good customer service, Google", "Good customer service, Intel",
                                   "Good customer service, Hewlett-Packard", "Good customer service, Sony",
                                   "Good customer service, Dell", "Good customer service, Yahoo",
                                   "Good customer service, Nokia", "Good customer service, Samsung",
                                   "Good customer service, LG", "Good customer service, Panasonic"),
                           Q4e = c("Stylish, Apple",
                                   "Stylish, Microsoft", "Stylish, IBM", "Stylish, Google",
                                   "Stylish, Intel", "Stylish, Hewlett-Packard", "Stylish, Sony",
                                   "Stylish, Dell", "Stylish, Yahoo", "Stylish, Nokia", "Stylish, Samsung",
                                   "Stylish, LG", "Stylish, Panasonic"),
                           Q4f = c("Easy to use, Apple", "Easy to use, Microsoft",
                                   "Easy to use, IBM", "Easy to use, Google", "Easy to use, Intel",
                                   "Easy to use, Hewlett-Packard", "Easy to use, Sony", "Easy to use, Dell",
                                   "Easy to use, Yahoo", "Easy to use, Nokia", "Easy to use, Samsung",
                                   "Easy to use, LG", "Easy to use, Panasonic"),
                           Q4g = c("High quality, Apple", "High quality, Microsoft",
                                   "High quality, IBM", "High quality, Google", "High quality, Intel",
                                   "High quality, Hewlett-Packard", "High quality, Sony", "High quality, Dell",
                                   "High quality, Yahoo", "High quality, Nokia", "High quality, Samsung",
                                   "High quality, LG", "High quality, Panasonic"),
                           Q4h = c("High performance, Apple", "High performance, Microsoft",
                                   "High performance, IBM", "High performance, Google", "High performance, Intel",
                                   "High performance, Hewlett-Packard", "High performance, Sony",
                                   "High performance, Dell", "High performance, Yahoo", "High performance, Nokia",
                                   "High performance, Samsung", "High performance, LG", "High performance, Panasonic"),
                           Q4i = c("Low prices, Apple",
                                   "Low prices, Microsoft", "Low prices, IBM", "Low prices, Google",
                                   "Low prices, Intel", "Low prices, Hewlett-Packard", "Low prices, Sony",
                                   "Low prices, Dell", "Low prices, Yahoo", "Low prices, Nokia",
                                   "Low prices, Samsung", "Low prices, LG", "Low prices, Panasonic"))
technology.unstacked.df <- cbind.data.frame(unname(technology.unstacked))
id.variable <- 1:nrow(technology.unstacked.df)
technology.unstacked.df[["id.variable"]] <- id.variable
all.names <- names(technology.unstacked.df)
variables.to.exclude <- all.names[!all.names %in% c(unlist(variables.to.stack), "id.variable")]
technology.stacked <- reshape(technology.unstacked.df, idvar = 'id.variable', direction = "long",
                              drop = variables.to.exclude, varying = variables.to.stack)
technology.stacked["time"] <- NULL
technology.stacked["id.variable"] <- NULL
names(technology.stacked) <- names(variables.to.stack)
# Relabel outcome to allow count regression (inappropriate model for the data, just checking the model is computed)
count.Y <- as.data.frame(lapply(technology.unstacked[["Y"]], as.integer))
names(count.Y) <- names(technology.unstacked[["Y"]])
attr(count.Y, "question") <- "Brand"
attr(count.Y, "questiontype") <- "PickOneMulti"
stacked.count <- as.integer(technology.stacked$Q3)

types <- c("Linear" , "Ordered Logit", "Binary Logit", "Poisson", "Quasi-Poisson", "NBD", "Multinomial Logit")
count.types <- c("Poisson", "Quasi-Poisson", "NBD")
stacked.formula <- Q3 ~ Q4a + Q4b + Q4c + Q4d + Q4e + Q4f + Q4g + Q4h + Q4i
set.seed(12321)
n.cases <- nrow(technology.unstacked[["X"]])
n.outcomes <- ncol(technology.unstacked[["Y"]])
random.weights <- runif(n.cases)
attr(random.weights, "label") <- "Some Weights"
random.subset <- sample(c(TRUE, FALSE), size = n.cases, prob = c(0.75, 0.25), replace = TRUE)
attr(random.subset, "label") <- "A Filter"
interaction <- factor(sample(c("Male", "Female"), size = n.cases, prob = c(2/3, 1/3), replace = TRUE))
weight.choices <- list(NULL, random.weights)
subset.choices <- list(NULL, random.subset)
stacked.random.weights <- rep(random.weights, n.outcomes)
attr(stacked.random.weights, "label") <- "Some Weights"
stacked.random.filter <- rep(random.subset, n.outcomes)
attr(stacked.random.filter, "label") <- "A Filter"
stacked.weight.choices <- list(NULL, stacked.random.weights)
stacked.subset.choices <- list(NULL, stacked.random.filter)

# DS-2964 Create data that has the structure swapped (codeframe, secondarycodeframe, cols and reduction)
unstacked.codeframe.swap <- technology.unstacked
tmp <- attr(unstacked.codeframe.swap$X, "codeframe")
attr(unstacked.codeframe.swap$X, "codeframe") <- attr(unstacked.codeframe.swap$X, "secondarycodeframe")
attr(unstacked.codeframe.swap$X, "secondarycodeframe") <- tmp
# Swap the order of the comma separated labels in the grid (X) component
grid.labels <- strsplit(names(unstacked.codeframe.swap$X), split = ", ", fixed = TRUE)
names(unstacked.codeframe.swap$X) <- vapply(grid.labels, function(x) paste0(rev(x), collapse = ", "),
                                            character(1))
# Reorder columns themselves to be consistent with grid structure
outcomes.with.NET <- names(attr(unstacked.codeframe.swap$X, "secondarycodeframe"))
column.indices <- unlist(lapply(outcomes.with.NET, function(x) grep(paste0("^", x), names(unstacked.codeframe.swap$X))))
new.grid <- unstacked.codeframe.swap$X[column.indices]
new.grid <- flipU::CopyAttributes(new.grid, unstacked.codeframe.swap$X)
unstacked.codeframe.swap$X <- new.grid

for (type in types)
    for (s in seq_along(subset.choices))
        for (w in seq_along(weight.choices))
            test_that(paste0(type, " Regression Stacking output consistent: ",
                             " weights = ", if (w == 2) "Random" else "NULL",
                             ", subset = ", if (s == 2) "Random" else "NULL"), {
                mod.technology.stacked <- technology.stacked
                mod.technology.unstacked <- technology.unstacked
                mod.technology.unstacked.swap <- unstacked.codeframe.swap
                if (type %in% count.types)
                {
                    mod.technology.stacked$Q3 <- stacked.count
                    mod.technology.unstacked$Y <- count.Y
                    mod.technology.unstacked.swap$Y <- count.Y
                }
                stacked.regression <- suppressWarnings(Regression(stacked.formula, type = type,
                                                                  output = "Summary",
                                                                  subset = stacked.subset.choices[[s]],
                                                                  weights = stacked.weight.choices[[w]],
                                                                  data = mod.technology.stacked))
                stackable.regression <- suppressWarnings(Regression(type = type, stacked.data.check = TRUE,
                                                                    output = "Summary",
                                                                    subset = subset.choices[[s]],
                                                                    weights = weight.choices[[w]],
                                                                    unstacked.data = mod.technology.unstacked))
                stackable.regression.swap <- suppressWarnings(Regression(type = type, stacked.data.check = TRUE,
                                                                         output = "Summary",
                                                                         subset = subset.choices[[s]],
                                                                         weights = weight.choices[[w]],
                                                                         unstacked.data = mod.technology.unstacked.swap))
                expect_equal(unname(stacked.regression$coef),
                             unname(stackable.regression$coef))
                expect_equal(unname(stacked.regression$coef),
                             unname(stackable.regression.swap$coef))
                # Check meta data in weights and filters exists
                if (!is.null(weight.choices[[w]]))
                    expect_match(stackable.regression$sample.description, attr(weight.choices[[w]], "label"))
                if (!is.null(subset.choices[[s]]))
                    expect_match(stackable.regression$sample.description, attr(subset.choices[[s]], "label"))
            })


test_that("Check codeframe specific usage", {
    expect_true("NET" %in% names(attr(technology.unstacked$X, "codeframe")))
    expect_true("NET" %in% names(attr(technology.unstacked$X, "secondarycodeframe")))
    expect_true(any(grepl("^NET, ", names(technology.unstacked$X))))
    expect_true(any(grepl(", NET$", names(technology.unstacked$X))))
    expect_false("NET" %in% names(attr(technology.unstacked$Y, "secondarycodeframe")))
    cleaned.data <- flipRegression:::removeDataReduction(technology.unstacked)
    expect_false("NET" %in% names(attr(cleaned.data$X, "codeframe")))
    expect_false("NET" %in% names(attr(cleaned.data$X, "secondarycodeframe")))
    expect_false(any(grepl("^NET, ", names(cleaned.data$X))))
    expect_false(any(grepl(", NET$", names(cleaned.data$X))))
    expect_false("NET" %in% names(attr(cleaned.data$Y, "secondarycodeframe")))
    # Check Numeric Multi scenario
    numeric.unstacked <- technology.unstacked
    numeric.unstacked$Y <- suppressWarnings(AsNumeric(numeric.unstacked$Y, binary = FALSE))
    numeric.unstacked$Y <- transform(numeric.unstacked$Y, SUM = rowSums(numeric.unstacked$Y))
    attr(numeric.unstacked$Y, "question") <- "Brand"
    # Numeric multi only has a codeframe for the column information
    attr(numeric.unstacked$Y, "codeframe") <- attr(technology.unstacked$Y, "secondarycodeframe")
    attr(numeric.unstacked$Y, "codeframe")[["SUM"]] <- 0:(length(attr(numeric.unstacked$Y, "codeframe")) - 1)
    warning.msg <- paste0("The variable(s): ", sQuote("None of these"), " have been removed from the set of predictor ",
                          "variables in ", sQuote("Qualities"), " since they don't appear in the set of outcome ",
                          "variables in ", sQuote("Brand"))
    expect_warning(tech.out <- Regression(unstacked.data = technology.unstacked, stacked.data.check = TRUE),
                   warning.msg, fixed = TRUE)
    expect_warning(numeric.out <- Regression(unstacked.data = numeric.unstacked, stacked.data.check = TRUE),
                   warning.msg, fixed = TRUE)
    expect_equivalent(tech.out$original$coef, numeric.out$original$coef)
    # Check messages when column doesn't match
    technology.unstacked.y.wrong <- technology.unstacked
    names(technology.unstacked.y.wrong$Y)[1] <- "Amazon"
    names(attr(technology.unstacked.y.wrong$Y, "secondarycodeframe"))[1] <- "Amazon"
    outcome.warning.msg <- paste0("The variable(s): ", sQuote("Amazon"), " have been removed from the set of outcome ",
                                  "variables in ", sQuote("Brand"), " since they don't appear in the set of predictor ",
                                  "variables in ", sQuote("Qualities"))
    predictor.warning.msg <- paste0("The variable(s): ", paste0(sQuote(c("Apple", "None of these")), collapse = ", "),
                                    " have been removed from the set of predictor variables in ", sQuote("Qualities"),
                                    " since they don't appear in the set of outcome variables in ", sQuote("Brand"))
    y.wrong.warnings <- capture_warnings(Regression(unstacked.data = technology.unstacked.y.wrong,
                                                    stacked.data.check = TRUE))
    expect_true(outcome.warning.msg %in% y.wrong.warnings)
    expect_true(predictor.warning.msg %in% y.wrong.warnings)
    # Check same for numeric multi (uses codeframe instead of secondary codeframe)
    numeric.y.wrong <- numeric.unstacked
    names(numeric.y.wrong$Y)[1] <- "Amazon"
    names(attr(numeric.y.wrong$Y, "codeframe"))[1] <- "Amazon"
    outcome.warning.msg <- paste0("The variable(s): ", sQuote("Amazon"), " have been removed from the set of outcome ",
                                  "variables in ", sQuote("Brand"), " since they don't appear in the set of predictor ",
                                  "variables in ", sQuote("Qualities"))
    predictor.warning.msg <- paste0("The variable(s): ", paste0(sQuote(c("Apple", "None of these")), collapse = ", "),
                                    " have been removed from the set of predictor variables in ", sQuote("Qualities"),
                                    " since they don't appear in the set of outcome variables in ", sQuote("Brand"))
    y.wrong.warnings <- capture_warnings(Regression(unstacked.data = numeric.y.wrong,
                                                    stacked.data.check = TRUE))
    expect_true(outcome.warning.msg %in% y.wrong.warnings)
    expect_true(predictor.warning.msg %in% y.wrong.warnings)
    # Check stacking handles commas effectively when codeframe available
    unstacked.with.commas <- technology.unstacked
    names(unstacked.with.commas$X) <- sub("Good customer", "Good, customer", names(unstacked.with.commas$X))
    names(attr(unstacked.with.commas$X, "secondarycodeframe")) <- sub("Good customer", "Good, customer",
                                                                      names(attr(unstacked.with.commas$X,
                                                                                 "secondarycodeframe")))
    names(unstacked.with.commas$X) <- sub("Worth what you pay for", "Worth,what,you,pay,for",
                                          names(unstacked.with.commas$X))
    names(attr(unstacked.with.commas$X, "secondarycodeframe")) <- sub("Worth what you pay for", "Worth,what,you,pay,for",
                                                                      names(attr(unstacked.with.commas$X,
                                                                                 "secondarycodeframe")))
    suppressWarnings(comma.out <- Regression(unstacked.data = unstacked.with.commas, stacked.data.check = TRUE))
    expect_equal(comma.out$original$coefficients, tech.out$original$coefficients)
})

outputs <- c("Relative Importance Analysis", "Shapley Regression")
# Test Relative importance output and Shapley, use subset for slight speed improvement
for (output in outputs)
    test_that(paste0(output, " consistent when stacking"), {
        stacked.regression <- suppressWarnings(Regression(stacked.formula, type = "Linear",
                                                          output = output,
                                                          subset = rep(subset.choices[[2]], n.outcomes),
                                                          data = technology.stacked))
        stackable.regression <- suppressWarnings(Regression(Y ~ X, type = "Linear", stacked.data.check = TRUE,
                                                            output = output,
                                                            subset = subset.choices[[2]],
                                                            unstacked.data = technology.unstacked))
        # Ignore names since formula names are slightly different
        stacked.output <- lapply(stacked.regression$importance, unname)
        stackable.output <- lapply(stackable.regression$importance, unname)
        # Check all importance values, statistics, standard.errors and p-values are the same
        expect_equal(stacked.output, stackable.output)
    })

# Check interaction output works and is the same between models
test_that("Interaction consistent when stacking:", {
    stacked.regression <- suppressWarnings(Regression(stacked.formula, type = "Linear",
                                                      output = "Summary",
                                                      subset = rep(subset.choices[[2]], n.outcomes),
                                                      interaction = interaction,
                                                      data = technology.stacked))
    stackable.regression <- suppressWarnings(Regression(Y ~ X, type = "Linear", stacked.data.check = TRUE,
                                                        output = "Summary",
                                                        subset = subset.choices[[2]],
                                                        interaction = interaction,
                                                        unstacked.data = technology.unstacked))
    # Check interaction coefficient table
    # Again ignore the names since formulae have different names
    expect_true(all.equal(stacked.regression$interaction$coefficients,
                          stackable.regression$interaction$coefficients,
                          check.attributes = FALSE))
})

test_that("Check categorical to numeric outcome warning", {
    outcome.test <- do.call("rbind", replicate(2, nominal.multi.outcome, simplify = FALSE))
    numeric.grid.test <- do.call("rbind", replicate(2, numeric.grid, simplify = FALSE))
    unstacked.data.test <- list(Y = outcome.test,
                                X = numeric.grid.test)
    subset <- c(TRUE, FALSE, FALSE, TRUE)
    attr(subset, "label") <- "A Filter"
    weights <- runif(4)
    attr(weights, "label") <- "Some weights"
    categorical.outcome.warnings <- capture_warnings(z <- Regression(type = "Linear",
                                                                     output = "Summary",
                                                                     subset = subset,
                                                                     stacked.data.check = TRUE,
                                                                     unstacked.data = unstacked.data.test))
    expect_equal(categorical.outcome.warnings[1],
                 paste0("Outcome variable is a factor; it has been made numeric. Consider using another ",
                        "type of regression (e.g., Ordered Logit or Binary Logit)."))
    expect_equal(categorical.outcome.warnings[2],
                 paste0("Data has been automatically converted to numeric. ",
                        "Values are assigned in the order of the categories: 1, 2, 3, ...; To use ",
                        "alternative numeric values, transform the data prior including it in this ",
                        "analysis (e.g. by changing its structure). ",
                        "The variable Brand Nominal has been converted."))
})


test_that("Check auxiliary methods", {
    stacked.regression <- suppressWarnings(Regression(type = "Linear", stacked.data.check = TRUE,
                                                      unstacked.data = technology.unstacked))
    stacked.binary.regression <- suppressWarnings(Regression(type = "Binary Logit", stacked.data.check = TRUE,
                                                             unstacked.data = technology.unstacked))
    # Check predict, residual and fitted value methods run without error
    # (should only fail on Rserver due to dataset dimension mismatch)
    expect_error(predicted <- predict(stacked.regression), NA)
    expect_true(length(predicted) == 3926)
    expect_error(stacked.residuals <- residuals(stacked.regression), NA)
    expect_true(length(stacked.residuals) == 3926)
    expect_error(stacked.fited <- fitted(stacked.regression), NA)
    expect_true(length(stacked.fited) == 3926)
    # Check probabilities doesn't fail
    expect_error(stacked.probabilities <- flipData::Probabilities(stacked.binary.regression), NA)
    expect_equal(dim(stacked.probabilities), c(3926, 2))
})

test_that("DS-2694: Ensure data stacking occurs properly", {
    # Create new order in Grid
    unstacked <- technology.unstacked
    new.ordering <- c("Yahoo", "Microsoft", "Dell", "Panasonic", "Sony", "IBM", "Samsung",
                      "Google", "LG", "Intel", "Nokia", "Apple", "Hewlett-Packard")
    new.alignment <- match(new.ordering, names(unstacked$Y))
    unstacked$Y <- unstacked$Y[new.alignment]
    unstacked$Y <- CopyAttributes(unstacked$Y, technology.unstacked$Y)
    names(attr(unstacked$Y, "secondarycodeframe")) <- new.ordering
    outcome.names <- new.ordering
    expect_equal(names(unstacked$Y), new.ordering)
    outcome.names.in.grid <- names(attr(unstacked$X, "codeframe"))
    expect_setequal(names(unstacked$Y), intersect(outcome.names.in.grid, outcome.names))
    expect_false(all(names(unstacked$Y) == intersect(outcome.names.in.grid, outcome.names)))
    # Remove datareduction and other variables
    unstacked.data <- removeDataReduction(unstacked)
    expect_warning(unstacked <- validateDataForStacking(unstacked.data),
                   "have been removed from the set of predictor variables in")
    # Check data aligned properly
    expect_equal(names(unstacked$data$Y), names(attr(unstacked$data$X, "codeframe")))
    expect_equal(names(unstacked$data$Y), names(attr(unstacked$data$Y, "secondarycodeframe")))
    # Ensure alignment check passes
    expect_error(flipRegression:::stackData(unstacked$data), NA)
})

test_that("DS-2694: Ensure NET and duplicated variables are removed", {
    # Ensure duplicated variables are removed (aliased variables)
    # Start on numeric-multi
    Y <- structure(list(`Out A` = c(1.36, -0.23, -0.16),
                        `Out B` = c(1.59, 0.05, 0.11),
                        `Out C` = c(0.3, -0.37, 0.5),
                        `Out D` = c(1.36, -0.23, -0.16),
                        `Out E` = c(1.59, 0.05, 0.11),
                        `Out F` = c(0.3, -0.37, 0.5),
                        SUM = c(3.25, -0.55, 0.45)),
                   row.names = c(NA, 3L), questiontype = "NumberMulti", question = "Y",
                   dataset = "Fake data",
                   codeframe = list(`Out A` = 0L, `Out B` = 1L, `Out C` = 2L, `Out D` = 0L,
                                    `Out E` = 1L, `Out F` = 2L, SUM = 0:2),
                   transposed = FALSE, class = "data.frame")
    expected.flags <- c(rep(c(FALSE, TRUE), c(3, 4)))
    names(expected.flags) <- c(paste0("Out ", LETTERS[1:6]), "SUM")
    expect_identical(flipRegression:::flagCodeframeReduction(attr(Y, "codeframe")),
                     expected.flags)
    # Numeric - grid with duplicates
    X <- structure(list(`1, B` = c(1.36, -0.23, -0.16), `1, A` = c(1.36, -0.23, -0.16), `1, A` = c(1.36, -0.23, -0.16),
                        `1, SUM` = c(2.72, -0.46, -0.33),
                        `1, B` = c(1.36, -0.23, -0.16), `1, A` = c(1.36, -0.23, -0.16), `1, A` = c(1.36, -0.23, -0.16),
                        `1, SUM` = c(2.72, -0.46, -0.33),
                        `2, B` = c(1.59, 0.05, 0.11), `2, A` = c(1.59, 0.05, 0.11), `2, A` = c(1.59, 0.05, 0.11),
                        `2, SUM` = c(3.17, 0.1, 0.22),
                        `2, B` = c(1.59, 0.05, 0.11), `2, A` = c(1.59, 0.05, 0.11),  `2, A` = c(1.59, 0.05, 0.11),
                        `2, SUM` = c(3.17, 0.1, 0.22),
                        `3, B` = c(0.3, -0.37, 0.5), `3, A` = c(0.3, -0.37, 0.5),  `3, A` = c(0.3, -0.37, 0.5),
                        `3, SUM` = c(0.61, -0.74, 1.01),
                        `SUM, B` = c(3.25, -0.55, 0.45), `SUM, A` = c(3.25, -0.55, 0.45), `SUM, A` = c(3.25, -0.55, 0.45),
                        `SUM, SUM` = c(6.5, -1.1, 0.89)),
                   row.names = c(NA, 3L), questiontype = "NumberGrid", question = "X", dataset = "Fake data",
                   codeframe = list(B = 0L, A = 1L, A = 1L, SUM = 0:1),
                   secondarycodeframe = list(`1` = 0L, `1` = 0L, `2` = 1L, `2` = 1L, `3` = 2L, SUM = 0:2),
                   transposed = FALSE, class = "data.frame")
    expected.flags.codeframe <- rep(c(FALSE, TRUE), c(2, 2))
    names(expected.flags.codeframe) <- c("B", "A", "A", "SUM")
    expect_identical(flipRegression:::flagCodeframeReduction(attr(X, "codeframe")),
                     expected.flags.codeframe)
    expected.flags.secondary.codeframe <- rep(c(FALSE, TRUE), c(2, 2))
    names(expected.flags.codeframe) <- c("B", "A", "A", "SUM")
    expected.flags.secondary.codeframe <- rep(c(FALSE, TRUE), 3)
    names(expected.flags.secondary.codeframe) <- c(1, 1, 2, 2, 3, "SUM")
    expect_identical(flipRegression:::flagCodeframeReduction(attr(X, "secondarycodeframe")),
                     expected.flags.secondary.codeframe)
    # Expected filtered data with duplicates removed
    expected.reduced.data <- list(Y = structure(list(`Out A` = c(1.36, -0.23, -0.16),
                                                     `Out B` = c(1.59, 0.05, 0.11),
                                                     `Out C` = c(0.3, -0.37, 0.5)),
                                                row.names = c(NA, 3L), questiontype = "NumberMulti",
                                                question = "Y", dataset = "Fake data",
                                                codeframe = list(`Out A` = 0L, `Out B` = 1L, `Out C` = 2L),
                                                transposed = FALSE, class = "data.frame"),
                                  X = structure(list(`1, B` = c(1.36, -0.23, -0.16),
                                                     `1, A` = c(1.36, -0.23, -0.16),
                                                     `2, B` = c(1.59, 0.05, 0.11),
                                                     `2, A` = c(1.59, 0.05, 0.11),
                                                     `3, B` = c(0.3, -0.37, 0.5),
                                                     `3, A` = c(0.3, -0.37, 0.5)),
                                                row.names = c(NA, 3L), questiontype = "NumberGrid",
                                                question = "X", dataset = "Fake data",
                                                codeframe = list(B = 0L, A = 1L),
                                                secondarycodeframe = list(`1` = 0L, `2` = 1L, `3` = 2L),
                                                transposed = FALSE, class = "data.frame"))
    expect_identical(flipRegression:::removeDataReduction(list(Y = Y, X = X)),
                     expected.reduced.data)
    # Nominal - Multi
    nom.multi <- structure(list(`Coca-Cola` = structure(c(2L, 2L, 2L, 2L, 2L),
                                                        .Label = c("Hate", "Dislike", "Neither like nor dislike", "Love + Like"),
                                                        class = "factor"),
                                `Diet Coke` = structure(c(4L, 4L, 4L, 4L, 4L),
                                                        .Label = c("Hate", "Dislike", "Neither like nor dislike", "Love + Like"),
                                                        class = "factor"),
                                `Coke Zero` = structure(c(2L, 2L, 2L, 2L, 2L),
                                                        .Label = c("Hate", "Dislike", "Neither like nor dislike", "Love + Like"),
                                                        class = "factor"),
                                Pepsi = structure(c(3L, 3L, 3L, 3L, 3L),
                                                  .Label = c("Hate", "Dislike", "Neither like nor dislike", "Love + Like"),
                                                  class = "factor"),
                                `Diet Pepsi` = structure(c(1L, 1L, 1L, 1L, 1L),
                                                         .Label = c("Hate", "Dislike", "Neither like nor dislike", "Love + Like"),
                                                         class = "factor"),
                                `Pepsi Max` = structure(c(4L, 4L, 4L, 4L, 4L),
                                                        .Label = c("Hate", "Dislike", "Neither like nor dislike", "Love + Like"),
                                                        class = "factor")),
                           questiontype = "PickOneMulti", question = "Brand attitude",
                           dataset = "Cola Tracking - January to December.sav",
                           codeframe = list(Hate = -2, Dislike = -1, `Hate + Dislike` = c(-2, -1),
                                            `Neither like nor dislike` = 0, `Love + Like` = c(1, 2),
                                            NET = c(-2, -1, 0, 1, 2)),
                           secondarycodeframe = list(`Coca-Cola` = 0L, `Diet Coke` = 1L, `Coke Zero` = 2L, Pepsi = 3L, `Diet Pepsi` = 4L, `Pepsi Max` = 5L),
                           transposed = FALSE, row.names = 1:5,
                           class = "data.frame")
    expected.nom.codeframe <- c(Hate = FALSE, Dislike = FALSE, `Hate + Dislike` = TRUE,
                                `Neither like nor dislike` = FALSE, `Love + Like` = FALSE, NET = TRUE)
    expect_identical(flipRegression:::flagCodeframeReduction(attr(nom.multi, "codeframe")),
                     expected.nom.codeframe)
    expected.nom.secondary <- rep(FALSE, 6)
    names(expected.nom.secondary) <- names(nom.multi)
    expect_identical(flipRegression:::flagCodeframeReduction(attr(nom.multi, "secondarycodeframe")),
                     expected.nom.secondary)
    binary.grid <- structure(list(`Feminine, Coke` = c(0, 0, 0, 0, 0),
                                  `Feminine, Diet Coke` = c(1, 1, 1, 1, 1),
                                  `Feminine, Coke Zero` = c(0, 0, 0, 0, 0),
                                  `Feminine, Diet Coke + Coke Zero` = c(1, 1, 1, 1, 1),
                                  `Feminine, Pepsi` = c(0, 0, 0, 0, 0),
                                  `Feminine, Coke + Diet Coke + Coke Zero + Pepsi` = c(1, 1, 1, 1, 1),
                                  `Feminine, Pepsi Max` = c(0, 0, 0, 0, 0),
                                  `Feminine, NET` = c(1, 1, 1, 1, 1),
                                  `Health-conscious, Coke` = c(0, 0, 0, 0, 0),
                                  `Health-conscious, Diet Coke` = c(1, 1, 1, 1, 1),
                                  `Health-conscious, Coke Zero` = c(0, 0, 0, 0, 0),
                                  `Health-conscious, Diet Coke + Coke Zero` = c(1, 1, 1, 1, 1),
                                  `Health-conscious, Pepsi` = c(0, 0, 0, 0, 0),
                                  `Health-conscious, Coke + Diet Coke + Coke Zero + Pepsi` = c(1, 1, 1, 1, 1),
                                  `Health-conscious, Pepsi Max` = c(0, 0, 0, 0, 0),
                                  `Health-conscious, NET` = c(1, 1, 1, 1, 1),
                                  `Feminine + Health-conscious, Coke` = c(0, 0, 0, 0, 0),
                                  `Feminine + Health-conscious, Diet Coke` = c(1, 1, 1, 1, 1),
                                  `Feminine + Health-conscious, Coke Zero` = c(0, 0, 0, 0, 0),
                                  `Feminine + Health-conscious, Diet Coke + Coke Zero` = c(1, 1, 1, 1, 1),
                                  `Feminine + Health-conscious, Pepsi` = c(0, 0, 0, 0, 0),
                                  `Feminine + Health-conscious, Coke + Diet Coke + Coke Zero + Pepsi` = c(1, 1, 1, 1, 1),
                                  `Feminine + Health-conscious, Pepsi Max` = c(0, 0, 0, 0, 0),
                                  `Feminine + Health-conscious, NET` = c(1, 1, 1, 1, 1),
                                  `Innocent, Coke` = c(0, 0, 0, 0, 0), `Innocent, Diet Coke` = c(0, 0, 0, 0, 0),
                                  `Innocent, Coke Zero` = c(0, 0, 0, 0, 0), `Innocent, Diet Coke + Coke Zero` = c(0, 0, 0, 0, 0),
                                  `Innocent, Pepsi` = c(0, 0, 0, 0, 0), `Innocent, Coke + Diet Coke + Coke Zero + Pepsi` = c(0, 0, 0, 0, 0),
                                  `Innocent, Pepsi Max` = c(0, 0, 0, 0, 0), `Innocent, NET` = c(1, 1, 1, 1, 1),
                                  `Older, Coke` = c(1, 1, 1, 1, 1), `Older, Diet Coke` = c(0, 0, 0, 0, 0),
                                  `Older, Coke Zero` = c(0, 0, 0, 0, 0), `Older, Diet Coke + Coke Zero` = c(0, 0, 0, 0, 0),
                                  `Older, Pepsi` = c(1, 1, 1, 1, 1), `Older, Coke + Diet Coke + Coke Zero + Pepsi` = c(1, 1, 1, 1, 1),
                                  `Older, Pepsi Max` = c(0, 0, 0, 0, 0), `Older, NET` = c(1, 1, 1, 1, 1),
                                  `Open to new experiences, Coke` = c(0, 0, 0, 0, 0),
                                  `Open to new experiences, Diet Coke` = c(0, 0, 0, 0, 0),
                                  `Open to new experiences, Coke Zero` = c(0, 0, 0, 0, 0),
                                  `Open to new experiences, Diet Coke + Coke Zero` = c(0, 0, 0, 0, 0),
                                  `Open to new experiences, Pepsi` = c(0, 0, 0, 0, 0),
                                  `Open to new experiences, Coke + Diet Coke + Coke Zero + Pepsi` = c(0, 0, 0, 0, 0),
                                  `Open to new experiences, Pepsi Max` = c(1, 1, 1, 1, 1),
                                  `Open to new experiences, NET` = c(1, 1, 1, 1, 1),
                                  `Rebellious + Sleepy + Traditional, Coke` = c(1, 1, 1, 1, 1),
                                  `Rebellious + Sleepy + Traditional, Diet Coke` = c(0, 0, 0, 0, 0),
                                  `Rebellious + Sleepy + Traditional, Coke Zero` = c(0, 0, 0, 0, 0),
                                  `Rebellious + Sleepy + Traditional, Diet Coke + Coke Zero` = c(0, 0, 0, 0, 0),
                                  `Rebellious + Sleepy + Traditional, Pepsi` = c(1, 1, 1, 1, 1),
                                  `Rebellious + Sleepy + Traditional, Coke + Diet Coke + Coke Zero + Pepsi` = c(1, 1, 1, 1, 1),
                                  `Rebellious + Sleepy + Traditional, Pepsi Max` = c(1, 1, 1, 1, 1),
                                  `Rebellious + Sleepy + Traditional, NET` = c(1, 1, 1, 1, 1),
                                  `Weight-conscious, Coke` = c(0, 0, 0, 0, 0), `Weight-conscious, Diet Coke` = c(0, 0, 0, 0, 0),
                                  `Weight-conscious, Coke Zero` = c(0, 0, 0, 0, 0), `Weight-conscious,
                                  Diet Coke + Coke Zero` = c(0, 0, 0, 0, 0), `Weight-conscious, Pepsi` = c(0, 0, 0, 0, 0),
                                  `Weight-conscious, Coke + Diet Coke + Coke Zero + Pepsi` = c(0, 0, 0, 0, 0),
                                  `Weight-conscious, Pepsi Max` = c(0, 0, 0, 0, 0),
                                  `Weight-conscious, NET` = c(1, 1, 1, 1, 1), `NET, Coke` = c(1, 1, 1, 1, 1),
                                  `NET, Diet Coke` = c(1, 1, 1, 1, 1), `NET, Coke Zero` = c(0, 0, 0, 0, 0),
                                  `NET, Diet Coke + Coke Zero` = c(1, 1, 1, 1, 1),
                                  `NET, Pepsi` = c(1, 1, 1, 1, 1),
                                  `NET, Coke + Diet Coke + Coke Zero + Pepsi` = c(1, 1, 1, 1, 1),
                                  `NET, Pepsi Max` = c(1, 1, 1, 1, 1),
                                  `NET, NET` = c(1, 1, 1, 1, 1)),
                             questiontype = "PickAnyGrid", question = "q5",
                             dataset = "Cola Tracking - January to December.sav",
                             values = c(No = 0, Yes = 1),
                             codeframe = list(Coke = 0L, `Diet Coke` = 1L,`Coke Zero` = 2L,
                                              `Diet Coke + Coke Zero` = 1:2, Pepsi = 3L,
                                              `Coke + Diet Coke + Coke Zero + Pepsi` = 0:3,
                                              `Pepsi Max` = 5L, NET = 0:6),
                             secondarycodeframe = list(Feminine = 0L, `Health-conscious` = 1L,
                                                       `Feminine + Health-conscious` = 0:1,
                                                       Innocent = 2L, Older = 3L,
                                                       `Open to new experiences` = 4L,
                                                       `Rebellious + Sleepy + Traditional` = 5:7,
                                                       `Weight-conscious` = 8L, NET = 0:8),
                             transposed = FALSE, row.names = 1:5, class = "data.frame")
    expected.flag.codeframe <- c(Coke = FALSE, `Diet Coke` = FALSE, `Coke Zero` = FALSE,
                                 `Diet Coke + Coke Zero` = TRUE, Pepsi = FALSE,
                                 `Coke + Diet Coke + Coke Zero + Pepsi` = TRUE, `Pepsi Max` = FALSE,
                                 NET = TRUE)
    expect_identical(flipRegression:::flagCodeframeReduction(attr(binary.grid, "codeframe")),
                     expected.flag.codeframe)
    expected.flag.secondary <- c(Feminine = FALSE, `Health-conscious` = FALSE, `Feminine + Health-conscious` = TRUE,
                                 Innocent = FALSE, Older = FALSE, `Open to new experiences` = FALSE,
                                 `Rebellious + Sleepy + Traditional` = FALSE, `Weight-conscious` = FALSE,
                                 NET = TRUE)
    expect_identical(flipRegression:::flagCodeframeReduction(attr(binary.grid, "secondarycodeframe")),
                     expected.flag.secondary)
    # Check reduced data correct
    expected.list <- list(Y = structure(list(`Coca-Cola` = structure(rep(2, 5),
                                                                     .Label = c("Hate", "Dislike", "Neither like nor dislike", "Love + Like"),
                                                                     class = "factor"),
                                             `Diet Coke` = structure(rep(4, 5), .Label = c("Hate", "Dislike", "Neither like nor dislike", "Love + Like"),
                                                                     class = "factor"),
                                             `Coke Zero` = structure(rep(2, 5), .Label = c("Hate", "Dislike", "Neither like nor dislike", "Love + Like"),
                                                                     class = "factor"),
                                             Pepsi = structure(rep(3, 5),
                                                               .Label = c("Hate", "Dislike", "Neither like nor dislike", "Love + Like"),
                                                               class = "factor"),
                                             `Diet Pepsi` = structure(rep(1, 5),
                                                                      .Label = c("Hate", "Dislike", "Neither like nor dislike","Love + Like"),
                                                                      class = "factor"),
                                             `Pepsi Max` = structure(rep(4, 5),
                                                                     .Label = c("Hate", "Dislike", "Neither like nor dislike", "Love + Like"),
                                                                     class = "factor")),
                                        questiontype = "PickOneMulti", question = "Brand attitude", dataset = "Cola Tracking - January to December.sav",
                                        codeframe = list(Hate = -2, Dislike = -1, `Hate + Dislike` = c(-2, -1), `Neither like nor dislike` = 0,
                                                         `Love + Like` = c(1, 2), NET = c(-2, -1, 0, 1, 2)),
                                        secondarycodeframe = list(`Coca-Cola` = 0L, `Diet Coke` = 1L, `Coke Zero` = 2L, Pepsi = 3L, `Diet Pepsi` = 4L, `Pepsi Max` = 5L),
                                        transposed = FALSE, row.names = 1:5,
                                        class = "data.frame"),
                          X = structure(list(`Feminine, Coke` = c(0, 0, 0, 0, 0),
                                             `Feminine, Diet Coke` = c(1, 1, 1, 1, 1),
                                             `Feminine, Coke Zero` = c(0, 0, 0, 0, 0),
                                             `Feminine, Pepsi` = c(0, 0, 0, 0, 0),
                                             `Feminine, Pepsi Max` = c(0,0, 0, 0, 0),
                                             `Health-conscious, Coke` = c(0, 0, 0, 0, 0),
                                             `Health-conscious, Diet Coke` = c(1, 1, 1, 1, 1),
                                             `Health-conscious, Coke Zero` = c(0, 0, 0, 0, 0),
                                             `Health-conscious, Pepsi` = c(0, 0, 0, 0, 0),
                                             `Health-conscious, Pepsi Max` = c(0, 0, 0, 0, 0),
                                             `Innocent, Coke` = c(0, 0, 0, 0, 0), `Innocent, Diet Coke` = c(0, 0, 0, 0, 0),
                                             `Innocent, Coke Zero` = c(0, 0, 0, 0, 0), `Innocent, Pepsi` = c(0, 0, 0, 0, 0),
                                             `Innocent, Pepsi Max` = c(0, 0, 0, 0, 0), `Older, Coke` = c(1, 1, 1, 1, 1),
                                             `Older, Diet Coke` = c(0, 0, 0, 0, 0), `Older, Coke Zero` = c(0, 0, 0, 0, 0),
                                             `Older, Pepsi` = c(1, 1, 1, 1, 1), `Older, Pepsi Max` = c(0, 0, 0, 0, 0),
                                             `Open to new experiences, Coke` = c(0, 0, 0, 0, 0),
                                             `Open to new experiences, Diet Coke` = c(0, 0, 0, 0, 0),
                                             `Open to new experiences, Coke Zero` = c(0, 0, 0, 0, 0),
                                             `Open to new experiences, Pepsi` = c(0, 0, 0, 0, 0 ),
                                             `Open to new experiences, Pepsi Max` = c(1, 1, 1, 1, 1 ),
                                             `Rebellious + Sleepy + Traditional, Coke` = c(1, 1, 1, 1, 1),
                                             `Rebellious + Sleepy + Traditional, Diet Coke` = c(0, 0, 0, 0, 0),
                                             `Rebellious + Sleepy + Traditional, Coke Zero` = c(0, 0, 0, 0, 0),
                                             `Rebellious + Sleepy + Traditional, Pepsi` = c(1, 1, 1, 1, 1),
                                             `Rebellious + Sleepy + Traditional, Pepsi Max` = c(1, 1, 1, 1, 1),
                                             `Weight-conscious, Coke` = c(0, 0, 0, 0, 0),
                                             `Weight-conscious, Diet Coke` = c(0, 0, 0, 0, 0),
                                             `Weight-conscious, Coke Zero` = c(0, 0, 0, 0, 0),
                                             `Weight-conscious, Pepsi` = c(0, 0, 0, 0, 0), `Weight-conscious, Pepsi Max` = c(0, 0, 0, 0, 0)), questiontype = "PickAnyGrid", question = "q5", dataset = "Cola Tracking - January to December.sav",
                                        codeframe = list(Coke = 0L, `Diet Coke` = 1L, `Coke Zero` = 2L, Pepsi = 3L, `Pepsi Max` = 5L),
                                        secondarycodeframe = list(Feminine = 0L, `Health-conscious` = 1L, Innocent = 2L, Older = 3L, `Open to new experiences` = 4L, `Rebellious + Sleepy + Traditional` = 5:7, `Weight-conscious` = 8L),
                                        transposed = FALSE, row.names = c(NA, 5L), values = c(No = 0, Yes = 1), class = "data.frame"))
        expect_identical(flipRegression:::removeDataReduction(list(Y = nom.multi, X = binary.grid)),
                         expected.list)
})
