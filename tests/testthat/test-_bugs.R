context("Bugs")
data(bank, package = "flipExampleData")
bank <- bank[sample(nrow(bank), 200), ] # random sample of 200 rows to improve perfomance
zformula <- Overall ~ Fees + Interest + Phone + Branch + Online + ATM
sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 100"
wgt <- bank$ID
attr(wgt, "label") <- "ID"
bank <- transform(bank, o2 = factor(unclass(bank$Overall) > 3))


type <- "Multinomial Logit"
missing.options <- c("Multiple imputation", "Imputation (replace missing values with estimates)",
                     "Exclude cases with missing data", "Dummy variable adjustment")
for (missing in missing.options)
        test_that(paste("DS-884 MNL with 2 category dependent variable", missing), {
          # Prevent pop-ups
          mockery::stub(print.Regression, "print.htmlwidget", NULL)
          # no weight, no filter
          z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = NULL, type = type))
          z = suppressWarnings(Regression(o2 ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = NULL, type = type))
          # weight
          expect_error(suppressWarnings(Regression(o2 ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = NULL, type = type)), NA)
          # weight, filter
          expect_error(suppressWarnings(Regression(o2 ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = wgt, type = type)), NA)
          # weight, filter
          expect_error(z <- suppressWarnings(Regression(o2 ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = wgt, type = type)), NA)
          expect_error(suppressWarnings(print(z)),NA)
      })


test_that("Poisson ANOVA p-values are very different to Regression + ignore robust SE",
{
    # Removed support for robust se from regression 17 Nov 2016
    expect_error(suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = "Poisson", robust.se = TRUE)))
})

test_that("Duplicated labels",
{
    df.small <- data.frame(
            Q4_2 = structure(
                c(-100, 0, 100, -100, 100, 100, 100, 100, 100, 100, 100, 0, 100, 100, 100, 100, 100, 100, 100, 100),
                questiontype = "Number",
                name = "Q4_2",
                label = "How likely are you to recommend this campsight to friends and family?",
                question = "How likely are you to recommend this campsight to friends and family? - NPS")
            ,
            Q5a_2 = structure(
                c(4, 5, 4, 5, 4, 5, 5, 4, 5, 3, 4, 2, 4, 4, 3, 4, 5, 5, 5, 5),
                questiontype = "NumberMulti",
                name = "Q5a_2",
                label = "Store overall",
                question = "How satisfied were you with... 2"
            ),
            Q5b_2 = structure(
                c(5, 5, 4, 5, 4, 5, 3, 5, 4, 5, 5, 5, 4, 3, 5, 4, 5, 4, 5, 4),
                questiontype = "NumberMulti",
                name = "Q5b_2",
                label = "Variety",
                question = "How satisfied were you with... 2"
            ),
            Q5c_2 = structure(
                c(5, 4, 3, 5, 4, 5, 5, 4, 5, 3, 4, 2, 4, 5, 3, 5, 4, 4, 3, 5),
                questiontype = "NumberMulti",
                name = "Q5c_2",
                label = "Service",
                question = "How satisfied were you with... 2"
            ),
            Q5d_2 = structure(
                c(5, 3, 2, 4, 1, 1, 3, 4, 3, 2, 2, 3, 2, 3, 3, 2, 1, 3, 2, 3),
                questiontype = "NumberMulti",
                name = "Q5d_2",
                label = "Cooking overall",
                question = "How satisfied were you with... 2"
            ),
            Q5e_2 = structure(
                c(4, 4, 1, 5, 2, 4, 2, 2, 2, 4, 1, 2, 1, 2, 1, 1, 3, 2, 2, 3),
                questiontype = "NumberMulti",
                name = "Q5e_2",
                label = "Cleanliness",
                question = "How satisfied were you with... 2"
            ),
            Q5f_2 = structure(
                c(5, 2, 2, 5, 1, 4, 3, 1, 1, 2, 1, 1, 2, 2, 1, 1, 2, 3, 3, 2),
                questiontype = "NumberMulti",
                name = "Q5f_2",
                label = "Wait time",
                question = "How satisfied were you with... 2"
            ),
            Q5g_2 = structure(
                c(5, 3, 3, 5, 3, 4, 4, 2, 3, 2, 4, 3, 1, 4, 2, 1, 4, 4, 5, 5),
                questiontype = "NumberMulti",
                name = "Q5g_2",
                label = "Seating",
                question = "How satisfied were you with... 2"
            ),
            Q5h_2 = structure(
                c(5, 2, 4, 1, 5, 3, 3, 3, 4, 3, 3, 4, 4, 3, 2, 2, 3, 4, 4, 2),
                questiontype = "NumberMulti",
                name = "Q5h_2",
                label = "Activities overall",
                question = "How satisfied were you with... 2"
            ),
            Q5i_2 = structure(
                c(5, 1, 1, 1, 3, 5, 2, 4, 5, 4, 4, 2, 2, 4, 3, 5, 4, 3, 3, 3),
                questiontype = "NumberMulti",
                name = "Q5i_2",
                label = "Number of activities",
                question = "How satisfied were you with... 2"
            ),
            Q5j_2 = structure(
                c(2, 3, 4, 5, 3, 3, 1, 2, 4, 1, 3, 3, 1, 3, 1, 3, 4, 3, 3, 4),
                questiontype = "NumberMulti",
                name = "Q5j_2",
                label = "Swimming",
                question = "How satisfied were you with... 2"
            ),
            Q5k_2 = structure(
                c(5, 5, 5, 1, 3, 5, 4, 5, 4, 5, 4, 2, 5, 4, 4, 3, 4, 5, 5, 5),
                questiontype = "NumberMulti",
                name = "Q5k_2",
                label = "Restroom overall",
                question = "How satisfied were you with... 2"
            ),
            Q5l_2 = structure(
                c(5, 5, 3, 5, 5, 5, 5, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5),
                questiontype = "NumberMulti",
                name = "Q5l_2",
                label = "Wait time",
                question = "How satisfied were you with... 2"
            ),
            Q5m_2 = structure(
                c(5, 5, 5, 5, 4, 5, 2, 2, 5, 4, 4, 5, 5, 5, 3, 4, 5, 4, 4, 5),
                questiontype = "NumberMulti",
                name = "Q5m_2",
                label = "Cleanliness",
                question = "How satisfied were you with... 2"
            ),
            Q5n_2 = structure(
                c(5, 4, 5, 5, 4, 5, 4, 5, 3, 5, 5, 3, 4, 5, 5, 4, 5, 5, 4, 2),
                questiontype = "NumberMulti",
                name = "Q5n_2",
                label = "Campsite overall",
                question = "How satisfied were you with... 2"
            ),
            Q5o_2 = structure(
                c(5, 5, 5, 5, 4, 5, 5, 5, 4, 4, 5, 5, 3, 4, 5, 3, 3, 5, 5, 4),
                questiontype = "NumberMulti",
                name = "Q5o_2",
                label = "Privacy",
                question = "How satisfied were you with... 2"
            ),
            Q5p_2 = structure(
                c(2, 5, 5, 2, 4, 5, 5, 5, 5, 4, 5, 2, 5, 3, 4, 5, 2, 4, 5, 5),
                questiontype = "NumberMulti",
                name = "Q5p_2",
                label = "Cleanliness",
                question = "How satisfied were you with... 2"
            ),
            Q5q_2 = structure(
                c(5, 3, 4, 4, 5, 3, 5, 5, 5, 3, 5, 3, 4, 5, 4, 5, 4, 3, 5, 5),
                questiontype = "NumberMulti",
                name = "Q5q_2",
                label = "Atmosphere",
                question = "How satisfied were you with... 2"
            ),
            Q5r_2 = structure(
                c(2, 2, 4, 5, 5, 5, 4, 5, 5, 3, 5, 3, 5, 4, 3, 4, 3, 4, 5, 5),
                questiontype = "NumberMulti",
                name = "Q5r_2",
                label = "Landscaping",
                question = "How satisfied were you with... 2"
            ))


    # Prevent pop-ups
    mockery::stub(print.Regression, "print.htmlwidget", NULL, depth = 2)
    expect_warning(print(Regression(Q4_2 ~ ., data = df.small, show.labels = TRUE,
            output = "Relative Importance Analysis")))
    expect_warning(print(Regression(Q4_2 ~ ., data = df.small, show.labels = TRUE,
            output = "ANOVA"), NA))
    expect_warning(print(Regression(Q4_2 ~ ., data = df.small, show.labels = TRUE,
            missing = "Multiple imputation")))
})

test_that("Simple linear regression without intercept prints without error", {
    set.seed(123)
    y <- rnorm(10)
    x <- rnorm(10)
    expect_error(z <- Regression(y ~ -1 + x), NA)
    # Prevent pop-ups
    mockery::stub(print.Regression, "print.htmlwidget", NULL)
    expect_error(print(z), NA)
})

test_that("DS-2919: Check error handling of polr and svyolr", {
    data(cola, package = "flipExampleData")
    data(bank, package = "flipExampleData")
    # Test regular ordered logit via polr gives informative error
    outcome.levels <- paste0(sQuote(levels(cola$Q2)), collapse = " and ")
    expect_error(suppressWarnings(Regression(Q2 ~ Q3, data = cola, type = "Ordered Logit")),
                 paste0("Fitting an Ordered Logit model requires the outcome variable to have three or more levels. ",
                        "The outcome variable here has two levels: ", outcome.levels, ". ",
                        "Consider using a Binary Logit model instead."))
    cola$only.one.level <- rep("Blueberry", nrow(cola))
    expect_error(suppressWarnings(Regression(only.one.level ~ Q3, data = cola, type = "Ordered Logit")),
                 paste0("Fitting an Ordered Logit model requires the outcome variable to have three or more levels. ",
                        "The outcome variable here has only one level: ", sQuote("Blueberry"),
                        ". A Regression model cannot be computed when the outcome variable has no variation."))
})
