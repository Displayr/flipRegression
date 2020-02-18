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

# Expected Stacked data

binary.binary.stacked <- structure(list(Y = structure(c(0, 1, 1, 1), label = "Brand Binary"),
                                        X1 = structure(c(0, 1, 1, 0), label = "Fun"),
                                        X2 = structure(c(1, 1, 1, 1), label = "Innovative")),
                                   class = "data.frame", row.names = c("1.1","2.1", "1.2", "2.2"))
binary.numeric.stacked <- structure(list(Y = structure(c(0, 1, 1, 1), label = "Brand Binary"),
                                         X1 = structure(c(0, 12, 11, 0), label = "Fun"),
                                         X2 = structure(c(15, 9, 9, 19), label = "Innovative")),
                                    class = "data.frame", row.names = c("1.1", "2.1", "1.2", "2.2"))
nominal.binary.stacked <- structure(list(Y = structure(c(1L, 2L, 2L, 2L), .Label = c("Low", "High"),
                                                       class = "factor", label = "Brand Nominal"),
                                         X1 = structure(c(0, 1, 1, 0), label = "Fun"),
                                         X2 = structure(c(1, 1, 1, 1), label = "Innovative")),
                                    class = "data.frame", row.names = c("1.1", "2.1", "1.2", "2.2"))
nominal.numeric.stacked <- structure(list(Y = structure(c(1L, 2L, 2L, 2L), .Label = c("Low", "High"),
                                                        class = "factor", label = "Brand Nominal"),
                                          X1 = structure(c(0, 12, 11, 0), label = "Fun"),
                                          X2 = structure(c(15, 9, 9, 19), label = "Innovative")),
                                     class = "data.frame", row.names = c("1.1", "2.1", "1.2", "2.2"))
ordinal.binary.stacked <- structure(list(Y = structure(c(1L, 2L, 2L, 2L), .Label = c("Low", "High"),
                                                       class = c("ordered", "factor"), label = "Brand Ordinal"),
                                         X1 = structure(c(0, 1, 1, 0), label = "Fun"),
                                         X2 = structure(c(1, 1, 1, 1), label = "Innovative")),
                                    class = "data.frame", row.names = c("1.1", "2.1", "1.2", "2.2"))
ordinal.numeric.stacked <- structure(list(Y = structure(c(1L, 2L, 2L, 2L), .Label = c("Low", "High"),
                                                        class = c("ordered", "factor"), label = "Brand Ordinal"),
                                          X1 = structure(c(0, 12, 11, 0), label = "Fun"),
                                          X2 = structure(c(15, 9, 9, 19), label = "Innovative")),
                                     class = "data.frame", row.names = c("1.1", "2.1", "1.2", "2.2"))
numeric.binary.stacked <- structure(list(Y = structure(c(6, 8, 10, 8), label = "Brand Numeric"),
                                         X1 = structure(c(0, 1, 1, 0), label = "Fun"),
                                         X2 = structure(c(1, 1, 1, 1), label = "Innovative")),
                                    class = "data.frame", row.names = c("1.1", "2.1", "1.2", "2.2"))
numeric.numeric.stacked <- structure(list(Y = structure(c(6, 8, 10, 8), label = "Brand Numeric"),
                                          X1 = structure(c(0, 12, 11, 0), label = "Fun"),
                                          X2 = structure(c(15, 9, 9, 19), label = "Innovative")),
                                     class = "data.frame", row.names = c("1.1", "2.1", "1.2", "2.2"))

test_that("Test input error messages", {
    # Test default incorrect input format
    error.msg <- paste0("'unstacked.data' needs to be a list with two elements, ",
                        "'Y' containing the outcome variables and 'X' containing the predictor variables. ",
                        "Outcome and predictor variables need to be variable sets that can be stacked. ",
                        "The outcome variable should be a Binary - Multi, Nominal - Multi, Ordinal - Multi ",
                        "or Numeric - Multi and The predictor variable should be a Binary - Grid or Numeric - Grid.")
    expect_error(Regression(formula = Y ~ X, stacked.data.check = TRUE, unstacked.data = NULL), error.msg)
    error.msg <- paste0("Size of variables doesn't agree, the provided outcome variables  have 2 observations",
                        " while the provided predictor variables  have 3 observations. ",
                        "Please input variables that have the same size.")
    # Test different sizes inputs
    expect_error(Regression(formula = Y ~ X, stacked.data.check = TRUE, unstacked.data = list(Y = data.frame(1:2),
                                                                                              X = data.frame(1:3))),
                 error.msg)
    error.msg <- paste0("Size of variables doesn't agree, the provided outcome variables ", sQuote('Brand Binary'),
                        " have 2 observations while the provided predictor variables ", sQuote('Qualities Binary'),
                        " have 1 observations. Please input variables that have the same size.")
    expect_error(Regression(formula = Y ~ X, stacked.data.check = TRUE,
                            unstacked.data = list(Y = binary.multi.outcome,
                                                  X = binary.grid.cleaned[1, ])),
                 error.msg, fixed = TRUE)
    # Test completely mismatched variables (no outcome labels match predictor labels)
    binary.mismatch.outcome <- binary.multi.outcome
    names(binary.mismatch.outcome)[1:2] <- c("Google", "Amazon")
    error.msg <- paste0("It is not possible to stack these variables since none of the outcome variable names match ",
                        "the variable names in the predictor variables. The outcome variable set ",
                        sQuote("Brand Binary"), " has names: ", paste0(sQuote(c("Google", "Amazon")), collapse = ", "),
                        " which don't appear in the names of the grid predictor variable set structure")
    expect_error(Regression(formula = Y ~ X, stacked.data.check = TRUE,
                            unstacked.data = list(Y = binary.mismatch.outcome,
                                                  X = binary.grid.cleaned)),
                 error.msg, fixed = TRUE)
    binary.mismatch.outcome <- binary.multi.outcome
    names(binary.mismatch.outcome)[1:2] <- c("Fun", "Microsoft")
    error.msg <- paste0("Ambiguous names in the grid predictors need to be reconciled before stacking can occur. ",
                        "The outcome variable ", sQuote("Brand Binary"), " has names: ",
                        paste0(sQuote(c("Fun", "Microsoft")), collapse = ", "), " and these names appear in both ",
                        "dimensions of the grid predictor input variable set. Please rename the names ",
                        "in eithe the outcome variable set or grid predictor variable set to stack the variables ",
                        "and proceed")
    expect_error(Regression(formula = Y ~ X, stacked.data.check = TRUE,
                            unstacked.data = list(Y = binary.mismatch.outcome,
                                                  X = binary.grid.cleaned)),
                 error.msg, fixed = TRUE)
    # Check outcome input appropriate
    error.msg <- "Outcome variable needs to have the question type attribute to be processed for stacking"
    outcome.without.attributes <- numeric.multi.outcome[1:ncol(numeric.multi.outcome.cleaned)]
    expect_error(Regression(formula = Y ~ X, stacked.data.check = TRUE,
                            unstacked.data = list(Y = outcome.without.attributes,
                                                  X = binary.grid.cleaned)),
                 error.msg)
    wrong.type <- numeric.multi.outcome
    attr(wrong.type, "questiontype") <- "Text"
    allowed.types <- paste0(sQuote(c('PickAny', 'PickOneMulti', 'NumberMulti')), collapse = ", ")
    error.msg <- paste0("Outcome variable to be stacked needs to be either a ", allowed.types,
                        " question type. Supplied outcome variable is ", sQuote("Text"))
    expect_error(Regression(formula = Y ~ X, stacked.data.check = TRUE,
                            unstacked.data = list(Y = wrong.type,
                                                  X = binary.grid.cleaned)),
                 error.msg)
    # Check Grid input appropriate
    error.msg <- "Grid Predictor variable set needs to have the question type attribute to be processed for stacking"
    predictor.without.attributes <- numeric.grid[1:ncol(numeric.grid.cleaned)]
    expect_error(Regression(formula = Y ~ X, stacked.data.check = TRUE,
                            unstacked.data = list(Y = numeric.multi.outcome,
                                                  X = predictor.without.attributes)),
                 error.msg)
    wrong.type <- numeric.grid
    attr(wrong.type, "questiontype") <- "Text"
    allowed.types <- paste0(sQuote(c('PickAnyGrid', 'NumberGrid')), collapse = ", ")
    error.msg <- paste0("Grid Predictor variable set to be stacked needs to be either a ", allowed.types,
                        " question type. Supplied variable is ", sQuote("Text"))
    expect_error(Regression(formula = Y ~ X, stacked.data.check = TRUE,
                            unstacked.data = list(Y = numeric.multi.outcome,
                                                  X = wrong.type)),
                 error.msg)
})

# Test warnings for slight mismatches in data
test_that("Mismatch warnings", {
    # Add extra variable(s) to the outcome
    extra.outcome <- binary.multi.outcome.cleaned
    extra.outcome$NET <- NULL
    extra.outcome$Amazon <- c(1, 0)
    extra.outcome$NET <- apply(extra.outcome, 1, function(x) as.numeric(any(as.logical(x))))
    warning.msg <- paste0("The variable(s): ", sQuote("Amazon"), " have been removed from the Outcome variable set ",
                          sQuote("Brand Binary"), " since these variables don't appear in the predictor variable set ",
                          sQuote("Qualities Binary"))
    expect_warning(output <- Regression(formula = Y ~ X, stacked.data.check = TRUE,
                                        unstacked.data = list(Y = extra.outcome,
                                                              X = binary.grid),
                                        method = "model.frame"),
                   warning.msg, fixed = TRUE)
    expect_identical(output, binary.binary.stacked)
    extra.outcome$NET <- NULL
    extra.outcome$Walmart <- c(1, 0)
    extra.outcome$NET <- apply(extra.outcome, 1, function(x) as.numeric(any(as.logical(x))))
    warning.msg <- paste0("The variable(s): ", paste0(sQuote(c("Amazon", "Walmart")), collapse = ", "),
                          " have been removed from the Outcome variable set ", sQuote("Brand Binary"),
                          " since these variables don't appear in the predictor variable set ",
                          sQuote("Qualities Binary"))
    expect_warning(output <- Regression(formula = Y ~ X, stacked.data.check = TRUE,
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
    warning.msg <- paste0("The variable(s): ", sQuote("Amazon"), " have been removed from the Predictor variable set ",
                          sQuote("Qualities Numeric"), " since these variables don't appear in the outcome variable set ",
                          sQuote("Brand Numeric"))
    expect_warning(output <- Regression(formula = Y ~ X, stacked.data.check = TRUE,
                                        unstacked.data = list(Y = numeric.multi.outcome,
                                                              X = extra.numeric.grid),
                                        method = "model.frame"),
                   warning.msg, fixed = TRUE)
    expect_identical(output, numeric.numeric.stacked)
    # Test warnings for ambiguous input
    warning.msg <- paste0("Ambiguous names between the outcome variable set and in the grid predictors variable set. ",
                          "The outcome variable ", sQuote("Brand Numeric"), " has names: ",
                          paste0(sQuote(c("Apple", "Microsoft")), collapse = ", "), " and these names appear ",
                          "in both dimensions of the grid predictor input variable set. Please rename the ",
                          "names in eithe the outcome variable set or grid predictor variable set to ",
                          "stack the variables and proceed.")
    expect_warning(ambiguous.output.1 <- Regression(formula = Y ~ X, stacked.data.check = TRUE,
                                                    unstacked.data = list(Y = numeric.multi.outcome,
                                                                          X = ambiguous.numeric.grid),
                                                    method = "model.frame"),
                   warning.msg, fixed = TRUE)
    warning.msg <- paste0("Ambiguous names between the outcome variable set and in the grid predictors variable set. ",
                          "The outcome variable ", sQuote("Brand Numeric"), " has names: ",
                          paste0(sQuote(c("Apple", "Microsoft")), collapse = ", "), " and these names appear ",
                          "in both dimensions of the grid predictor input variable set. Please rename the ",
                          "names in eithe the outcome variable set or grid predictor variable set to ",
                          "stack the variables and proceed.")
    expect_warning(ambiguous.output.2 <- Regression(formula = Y ~ X, stacked.data.check = TRUE,
                                                    unstacked.data = list(Y = numeric.multi.outcome,
                                                                          X = transposed.ambiguous.numeric.grid),
                                                    method = "model.frame"),
                   warning.msg, fixed = TRUE)
    # Expect transposed output to be the same (i.e. transpose occurs)
    expect_identical(ambiguous.output.1, ambiguous.output.2)
})

test_that("Transpose and alignment correct", {
    # Check transposing
    expect_identical(Regression(Y ~ X, stacked.data.check = TRUE,
                                unstacked.data = list(Y = numeric.multi.outcome,
                                                      X = transposed.numeric.grid),
                                method = "model.frame"),
                     Regression(Y ~ X, stacked.data.check = TRUE,
                                unstacked.data = list(Y = numeric.multi.outcome,
                                                      X = numeric.grid),
                                method = "model.frame"))
    # Check alignement
    original.data.frame <- Regression(Y ~ X, stacked.data.check = TRUE,
                                      unstacked.data = list(Y = larger.numeric.multi.outcome,
                                                            X = larger.numeric.grid),
                                      method = "model.frame")
    data.frame.after.alignment <- Regression(Y ~ X, stacked.data.check = TRUE,
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
                     flipRegression::removeDataReductionColumns(list(Y = binary.multi.outcome,
                                                                     X = binary.grid)))
    expect_identical(list(Y = nominal.multi.outcome, X = numeric.grid.cleaned),
                     flipRegression::removeDataReductionColumns(list(Y = nominal.multi.outcome,
                                                                     X = numeric.grid)))
    expect_identical(list(Y = ordinal.multi.outcome, X = numeric.grid.cleaned),
                     flipRegression::removeDataReductionColumns(list(Y = ordinal.multi.outcome,
                                                                     X = numeric.grid)))
    expect_identical(list(Y = numeric.multi.outcome.cleaned, X = numeric.grid.cleaned),
                     flipRegression::removeDataReductionColumns(list(Y = numeric.multi.outcome,
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
            expect_equal(Regression(formula = Y ~ X, stacked.data.check = TRUE,
                                    unstacked.data = list(Y = get(valid.outcome.vars[i]),
                                                          X = get(valid.predictor.vars[j])), method = "model.frame"),
                         get(output.stacked.dfs[stacked.index[i, j]]))
})


