# Test the new stackable data feature

# Multi outcome variable sets
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

# Grid variable sets
numeric.grid <- structure(list(`Fun, Apple` = c(0, 1), `Fun, Microsoft` = c(1, 0),
                               `Fun, SUM` = c(1, 1),
                               `Innovative, Apple` = c(1, 1), `Innovative, Microsoft` = c(1, 1),
                               `Innovative, SUM` = c(2, 2), `SUM, Apple` = c(1, 2), `SUM, Microsoft` = c(2, 1),
                               `SUM, SUM` = c(3, 3)),
                          class = "data.frame", row.names = 1:2,
                          questiontype = "NumberGrid", question = "Qualities Numeric")
numeric.grid.cleaned <- structure(list(`Fun, Apple` = c(0, 1), `Fun, Microsoft` = c(1, 0),
                                       `Innovative, Apple` = c(1, 1), `Innovative, Microsoft` = c(1, 1)),
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

# Expect errors if incorrect variable sets are used.
test_that("Errors for incorrect structure and/or input", {
    error.msg <- paste0("'unstacked.data' needs to be a list with two elements, ",
                        "'Y' containing the outcome variables and 'X' containing the predictor variables. ",
                        "Outcome and predictor variables need to be variable sets that can be stacked. ",
                        "The outcome variable should be a Binary - Multi, Nominal - Multi, Ordinal - Multi or Numeric - Multi and The predictor variable should be a Binary - Grid or Numeric - Grid.")
    expect_error(Regression(formula = Y ~ X, stacked.data.check = TRUE, unstacked.data = NULL), error.msg)
    error.msg <- paste0("Size of variables doesn't agree, the provided outcome variables  have 2 observations",
                        " while the provided predictor variables  have 3 observations. ",
                        "Please input variables that have the same size.")
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
    expect_error(Regression(formula = Y ~ X, stacked.data.check = TRUE,
                            unstacked.data = list(Y = binary.multi.outcome,
                                                  X = binary.grid), method = "model.frame"),
                 NA)

})

# Test Data Reduction columns are removed successfully.
test_that("Data Reduction for Mutli and Grid inputs", {
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
