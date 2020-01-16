context("flipFormat tests")

data(bank, package = "flipExampleData")

zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 100"
wgt <- bank$ID
attr(wgt, "label") <- "ID"
bank$dep <- (unclass(bank$Overall) - 1) / 6
attr(bank$dep, "label") <- "Overall satisfaction"
attr(bank$Fees, "label") <- "Fees paid"
attr(bank$Online, "label") <- "Online banking"
bank$fBranch <- factor(bank$Branch)
attr(bank$fBranch, "label") <- "Branch as a factor"
attr(bank$Overall, "label") <- "Overall satisfaction"
library(flipRegression)

# Below tests fail (optimization errors when run inside testthat run on the subset of 200 rows.
# Hence are run on the full data set. They do not fail when run outside testthat.
test_that("Regression: Variable names to labels",
          {
              # Variable labels
              z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Ordered Logit",
                                               subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE))
              expect_equal(rownames(z$summary$coefficients)[1], "Fees paid")
              expect_equal(rownames(z$summary$coefficients)[4], "Branch as a factor: 2")

              # Multiple imputation
              z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Linear", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE, missing = "Multiple imputation"))
              expect_equal(rownames(z$summary$coefficients)[2], "Fees paid")
              expect_equal(rownames(z$summary$coefficients)[5], "Branch as a factor: 2")
          })


#### REDUCE DATA SIZE FOR TESTS WITHOUT NUMERICAL EQUALITY ###

bank <- bank[sample(nrow(bank), 200), ] # random sample of 200 rows to improve perfomance

zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 100"
wgt <- bank$ID
attr(wgt, "label") <- "ID"
bank$dep <- (unclass(bank$Overall) - 1) / 6
attr(bank$dep, "label") <- "Overall satisfaction"
attr(bank$Fees, "label") <- "Fees paid"
attr(bank$Online, "label") <- "Online banking"
bank$fBranch <- factor(bank$Branch)
attr(bank$fBranch, "label") <- "Branch as a factor"
attr(bank$Overall, "label") <- "Overall satisfaction"


test_that("DS-1467 and 1468",
    {
        state = factor(rep(c("NSW","VC"), 2))
        attr(state, "label") = "State"
        attr(state, "question") = "State1"
        expect_equal(Labels(state), "State1: State")
        attr(state, "questiontype") = "PickAny"
        expect_equal(Labels(state), "State1: State")
        z = data.frame(state, state)
        expect_equal(as.character(Labels(z)), rep("State1: State", 2))
        attr(state, "questiontype") = "Date"
        expect_equal(Labels(state), "State1")
        attr(state, "questiontype") = "Number"
        expect_equal(Labels(state), "State1")
        attr(state, "questiontype") = "PickOne"
        expect_equal(Labels(state), "State1")
        attr(state, "questiontype") = "Text"
        expect_equal(Labels(state), "State1")
        z = data.frame(state, state)
        expect_equal(as.character(Labels(z)), rep("State1", 2))
        attr(state, "question") = ""
        expect_equal(Labels(state), "State")
        attr(state, "question") = NULL
        expect_equal(Labels(state), "State")

        bank$overall_dog = bank$Overall
        bank$zx = bank$Fees
        attr(bank$zx, "label") = "Label"
        attr(bank$zx, "question") = "Question"
        z = Regression(overall_dog ~ zx, data = bank, show.labels = TRUE)
        expect_equal(rownames(z$summary$coefficients)[[2]], "Question: Label")
        attr(bank$zx, "questiontype") = "PickAny"
        z = Regression(overall_dog ~ zx, data = bank, show.labels = TRUE)
        expect_equal(rownames(z$summary$coefficients)[[2]], "Question: Label")
        attr(bank$zx, "questiontype") = "PickOne"
        z = Regression(overall_dog ~ zx, data = bank, show.labels = TRUE)
        expect_equal(rownames(z$summary$coefficients)[[2]], "Question")
        attr(bank$zx, "question") = "Label"
        z = Regression(overall_dog ~ zx, data = bank, show.labels = TRUE)
        expect_equal(rownames(z$summary$coefficients)[[2]], "Label")
        #DS-1468
        Regression(overall_dog ~ Fees, data = bank, show.labels = TRUE)
})



test_that("Labels",
{
    data("phone", package = "flipExampleData")
    # A data frame with labels for everything
    expect_equal(as.character(Labels(phone, names(phone))), as.character(unlist(sapply(phone, function(x) attr(x, "label")))))
    # A few missing labels
    phone <- phone[,1:6]
    attr(phone[, 1], "label") <- NULL
    attr(phone[, 3], "label") <- NULL
    expect_equal(Labels(phone, names(phone)), c("id", "Does respondent have a mobile phone?", "q2", "Occupation", "Age",  "Top of mind awareness"))
    # Backticks put in manually.
    names(phone) <- paste0("`", names(phone), "`")
    expect_equal(Labels(phone, names(phone)), c("id", "Does respondent have a mobile phone?", "q2", "Occupation", "Age",  "Top of mind awareness"))
    #Factors in regression models
    data("cola", package = "flipExampleData")
    factor.coefficient.names <- suppressWarnings(names(coef(lm(Q2 ~ Q3, data = cola))))
    expect_equal(Labels(cola, factor.coefficient.names), c("(Intercept)", "Q3. Age: 25 to 29", "Q3. Age: 30 to 34", "Q3. Age: 35 to 39", "Q3. Age: 40 to 44", "Q3. Age: 45 to 49", "Q3. Age: 50 to 54", "Q3. Age: 55 to 64", "Q3. Age: 65 or more"))
    # Regression.
    data("bank", package = "flipExampleData")
    zbank <- bank[1:200,]
    set.seed(23442)
    zbank$rnd <- runif(nrow(zbank))
    zbank$rnd1 <- runif(nrow(zbank))
    zbank$rnd2 <- runif(nrow(zbank))
    zbank$rnd3 <- runif(nrow(zbank))
    zbank$rnd4 <- runif(nrow(zbank))
    attr(bank$Overall, "label") <- "Overall satisfaction"
    attr(bank$Fees, "label") <- "Fees paid"
    attr(bank$Online, "label") <- "Online banking"

    data("cola", package = "flipExampleData")
    cola <- cola[1:150,]
    cola$Q3[1:100] <- NA
    cola$Q3 <- unclass(cola$Q3)
    suppressWarnings(Regression(Overall ~ Fees, data = bank, type = "Ordered Logit", missing = "Multiple imputation", detail = FALSE, show.labels = TRUE))
    # DS-1467
    attr(bank$Fees, "question") <- "Fees paid1"
    suppressWarnings(Regression(Overall ~ Fees, data = bank, type = "Ordered Logit", missing = "Multiple imputation", detail = FALSE, show.labels = TRUE))
    attr(bank$Fees, "questiontype") <- "PickAny"
    suppressWarnings(Regression(Overall ~ Fees, data = bank, type = "Ordered Logit", missing = "Multiple imputation", detail = FALSE, show.labels = TRUE))
    attr(bank$Fees, "questiontype") <- "PickOne"
    suppressWarnings(Regression(Overall ~ Fees, data = bank, type = "Ordered Logit", missing = "Multiple imputation", detail = FALSE, show.labels = TRUE))





    # Some variables have labels and others do not.
    z <- data.frame(a = 1:10, b = 1:10, c = 1:10)
    flipFormat::Labels(z) <- c("A", "B")
    expect_equal(as.character(flipFormat::Labels(z)), c("A", "B", "c"))
})


data(bank, package = "flipExampleData")

test_that("BaseDescription",
          {
              library(flipRegression)
              # Unweighted, unfiltered
              expect_error(z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)),
                           NA)
              #z$sample.description
              # Weighted
              expect_error(z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = ID)),
                           NA)
              #z$sample.description
              # Filtered
              expect_error(z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = ID > 0.5)),
                           NA)
              #z$sample.description
              # Weighted, Filtered
              expect_error(z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = ID)),
                           NA)
              #z$sample.description
          })


test_that("Numeric dependent ~ numeric, factor, numeric factor",
          {
              data(colas, package= "flipExampleData")
              library(flipRegression)
              colas$num <- colas$q1a
              colas$q1b <- as.numeric(unclass(colas$q1a))
              colas$q1c <- as.numeric(unclass(colas$q1c))
              z <-suppressWarnings(Regression(num ~ q1b + q3 + q1c + d3, data = colas, detail = FALSE, show.labels = TRUE))
              expect_equal( rownames(z$summary$coefficients)[11], "Gender: Female")
          })

test_that("Regression: labels are extracted from variables containing $",
          {
              library(flipRegression)
              attach(bank)
              z = data.frame(q = Fees)
              zz <- rownames(Regression(Overall ~ z$q + Phone, detail = FALSE, show.labels = TRUE)$summary$coef)[2]
              expect_equal(zz, "Fees paid")
              detach(bank)
          })


test_that("Regression: Variable names to labels",
{

    # Variable names
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Linear", subset = sb, weights = wgt, detail = FALSE, show.labels = FALSE))
    expect_equal(rownames(z$summary$coefficients)[5], "fBranch2")
    expect_equal(rownames(z$summary$coefficients)[2], "Fees")
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Binary Logit", subset = sb, weights = wgt, detail = FALSE, show.labels = FALSE))
    expect_equal(rownames(z$summary$coefficients)[5], "fBranch2")
    expect_equal(rownames(z$summary$coefficients)[2], "Fees")

    # Variable labels
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Linear", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE))
    expect_equal(rownames(z$summary$coefficients)[2], "Fees paid")
    expect_equal(rownames(z$summary$coefficients)[5], "Branch as a factor: 2")
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Binary Logit", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE))
    expect_equal(rownames(z$summary$coefficients)[2], "Fees paid")
    expect_equal(rownames(z$summary$coefficients)[5], "Branch as a factor: 2")
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Multinomial Logit", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE))
    expect_equal(colnames(z$summary$coefficients)[2], "Fees paid")
    expect_equal(colnames(z$summary$coefficients)[5], "Branch as a factor: 2")
    # This test is run above on the full data set
    #z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Ordered Logit",
    #                                 subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE)
    #expect_equal(rownames(z$summary$coefficients)[1], "Fees paid")
    #expect_equal(rownames(z$summary$coefficients)[4], "Branch as a factor: 2")
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Poisson", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE))
    expect_equal(rownames(z$summary$coefficients)[2], "Fees paid")
    expect_equal(rownames(z$summary$coefficients)[5], "Branch as a factor: 2")

    # Small binary logit
    data(cola, package = "flipExampleData")
    attr(cola$Q2, "label") <- "Gender"
    attr(cola$Q3, "label") <- "Age of person"
    z <- suppressWarnings(Regression(Q3 ~ Q2, data = cola, type = "Binary Logit", detail = FALSE, show.labels = TRUE))
    expect_equal(rownames(z$summary$coefficients)[2], "Gender: Female")

    # Multiple imputation
    # This test is run above on the full data set
    #z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Linear", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE, missing = "Multiple imputation"))
    #expect_equal(rownames(z$summary$coefficients)[2], "Fees paid")
    #expect_equal(rownames(z$summary$coefficients)[5], "Branch as a factor: 2")
})







test_that("RegressionTable",{

    ft <- "Yo! This footer specifically designed
          to communicate important information.
    Since it is so important, it will of course
    extend over many lines.  In fact, on narrow tables,
    it might take >3.  On wide tables, it might only
    require one.  Feel free to adjust the width,
    and the importance and significance does not
    go away."

    data(weight, package = "flipExampleData")
    z = summary(lm(Weight ~ Height + Age, data = weight))$coef
    RegressionTable(z, TRUE, footer = ft,  title = "My awesome regression", subtitle = "Big brown dog", p.cutoff = 0.05)
    expect_error(RegressionTable(z, TRUE, footer = ft,  title = "My awesome regression", subtitle = "Big brown dog"), NA)

    ## Linear regression
    suppressWarnings(Regression(Overall ~  Fees + Interest + Phone + Branch + Online  +ATM, data = bank, detail = FALSE))

    # Linear regression with robust se
    suppressWarnings(Regression(Overall ~  Fees + Interest + Phone + Branch + Online  +ATM, data = bank, robust.se = TRUE, detail = FALSE))

    # Ordered logit (has a z statistic rather than a t)
    suppressWarnings(Regression(Overall ~  Fees + Interest + Phone + Branch + Online  +ATM, data = bank, type = "Ordered Logit", detail = FALSE))

    coef.matrix <- summary(lm(Sepal.Length ~ Species * Sepal.Width, iris))$coef
    rownames(coef.matrix)[1] <- "Big dog"
    RegressionTable(coef.matrix, TRUE, footer = ft,  title = "My awesome regression", subtitle = "Big brown dog")

    expect_error(RegressionTable(coef.matrix, TRUE, footer = ft,  title = "My awesome regression", subtitle = "Big brown dog"), NA)
})






