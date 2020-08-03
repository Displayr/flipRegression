context("Relative importance analysis")

data(bank, package = "flipExampleData")

X <- structure(list(v1 = c(9, 8,
    7, 8, 9, 9, 5, 7, 5, 7, 8, 6, 5, 6, 9, 8, 5, 7, 7, 4, 6, 9, 9,
    5, 9, 9, 7, 9, 6, 5, 5, 7, 7, 9, 5, 9, 9, 7, 9, 9, 9, 6, 9, 8,
    7, 7, 7, 5, 9, 9, 9, 9, 9, 7, 6, 6, 9, 7, 6, 8, 9, 4, 8, 7, 8,
    8, 6, 9, 9, 6, 7, 9, 4, 7, 9, 8, 9, 5, 8, 8, 9, 4, 4, 6, 6, 9,
    6, 7, 9, 4, 6, 8, 7, 6, 7, 5, 6, 7, 9, 9, 9, 9, 8, 4, 5, 4, 9,
    5, 8, 4, 5, 9, 8, 4, 5, 8, 9, 6, 7, 7, 6, 9, 7, 5, 4, 4, 9, 5,
    9, 8, 5, 9, 9, 9, 8, 9, 7, 7, 7, 9, 5, 5, 8, 5, 7, 8, 7, 9, 7,
    9, 7, 6, 5, 7, 6, 8, 8, 9, 9, 6, 9, 9, 7, 9, 7, 7, 7, 5, 9, 6,
    8, 5, 6, 9, 5, 9, 4, 9, 9, 7, 9, 7, 7, 6, 5, 7, 8, 7, 4, 6, 4,
    4, 9, 6, 7, 6, 6, 5, 7, 6, 9, 9, 4, 7, 4, 7, 9, 7, 4, 9, 6, 6,
    9, 6, 4, 5, 7, 7, 8, 7, 5, 9, 6, 8, 8, 7, 8, 6, 6, 4, 7, 7, 5,
    8, 9, 7, 8, 7, 7, 9, 9, 6, 6, 7, 6, 9, 6, 7, 9, 6, 8, 9, 8, 7,
    6, 9, 6, 6, 9, 9, 9, 9, 7, 7, 9, 7, 9, 6, 8, 9, 9, 9, 7, 6, 6,
    7, 7, 9, 4, 7, 6, 5, 8, 4, 9, 6, 9, 7, 7, 4, 6, 6, 9, 8, 9, 7,
    4, 5, 7, 7, 9, 8, 9, 9, 6, 7, 9, 9, 9, 8, 9, 6, 6, 5, 9, 8, 4,
    6, 4, 9, 8, 7, 7, 9, 7, 6, 4),
    v2 = c(7,
    9, 3, 1, 3, 1, 8, 4, 3, 4, 3, 6, 2, 4, 2, 1, 1, 7, 1, 3, 4, 9,
    2, 2, 6, 4, 4, 2, 1, 7, 2, 4, 9, 3, 9, 4, 3, 1, 5, 2, 4, 6, 4,
    7, 7, 1, 3, 3, 3, 6, 1, 2, 1, 7, 4, 7, 7, 5, 8, 3, 4, 2, 8, 3,
    5, 8, 4, 6, 8, 6, 8, 3, 4, 6, 1, 4, 4, 8, 6, 9, 7, 1, 6, 1, 1,
    4, 5, 9, 5, 4, 7, 4, 6, 4, 4, 2, 3, 6, 1, 1, 9, 6, 4, 6, 3, 3,
    4, 5, 6, 2, 3, 4, 7, 6, 1, 8, 6, 1, 6, 1, 6, 2, 5, 2, 5, 1, 4,
    1, 2, 4, 4, 6, 2, 4, 3, 2, 4, 1, 1, 2, 3, 4, 4, 4, 6, 5, 6, 6,
    6, 3, 2, 3, 5, 4, 1, 5, 4, 5, 1, 1, 1, 2, 1, 4, 3, 6, 1, 4, 1,
    2, 6, 1, 5, 3, 5, 2, 6, 4, 4, 1, 3, 6, 5, 7, 4, 3, 9, 1, 5, 1,
    2, 4, 3, 1, 2, 5, 2, 6, 4, 7, 8, 3, 5, 2, 5, 1, 1, 1, 4, 1, 4,
    6, 3, 3, 2, 6, 6, 4, 1, 4, 1, 3, 7, 2, 1, 4, 3, 9, 3, 3, 5, 5,
    2, 4, 1, 7, 1, 3, 2, 8, 2, 5, 2, 4, 4, 1, 1, 2, 1, 2, 2, 5, 5,
    1, 3, 2, 2, 1, 7, 9, 4, 4, 3, 2, 3, 5, 2, 5, 5, 9, 1, 4, 1, 2,
    6, 6, 1, 4, 1, 4, 9, 2, 1, 1, 3, 6, 2, 1, 2, 7, 3, 4, 2, 6, 4,
    3, 2, 7, 4, 1, 3, 3, 3, 7, 5, 2, 7, 5, 6, 5, 6, 1, 1, 6, 3, 9,
    1, 1, 1, 1, 8, 5, 3, 1, 1, 9, 3),
    v3 = c(7, 9, 9, 8, 1, 1,
    9, 9, 9, 9, 9, 8, 1, 5, 6, 9, 1, 1, 9, 1, 9, 2, 1, 9, 6, 1, 9,
    1, 1, 8, 9, 9, 9, 5, 9, 9, 8, 1, 6, 9, 9, 9, 9, 9, 9, 5, 9, 9,
    9, 7, 1, 9, 7, 1, 9, 1, 9, 6, 9, 8, 9, 9, 6, 7, 6, 8, 9, 9, 9,
    9, 9, 1, 9, 7, 9, 9, 5, 4, 9, 9, 2, 4, 7, 5, 1, 4, 9, 9, 9, 8,
    9, 8, 9, 1, 4, 9, 8, 9, 7, 9, 9, 9, 9, 7, 7, 5, 1, 2, 1, 9, 9,
    9, 9, 9, 9, 9, 9, 7, 9, 9, 9, 1, 9, 1, 9, 1, 7, 9, 9, 9, 2, 9,
    3, 9, 9, 2, 7, 7, 9, 9, 9, 1, 9, 1, 9, 1, 9, 6, 9, 1, 9, 9, 9,
    9, 9, 7, 1, 9, 8, 6, 1, 4, 9, 9, 1, 1, 9, 9, 2, 9, 9, 6, 9, 9,
    9, 5, 9, 9, 1, 9, 9, 1, 9, 1, 9, 1, 8, 9, 3, 9, 9, 8, 9, 2, 1,
    7, 9, 5, 9, 9, 8, 5, 7, 9, 9, 3, 8, 1, 4, 9, 9, 6, 4, 9, 5, 9,
    6, 9, 9, 5, 1, 3, 9, 9, 9, 9, 1, 9, 1, 9, 3, 9, 9, 9, 5, 9, 7,
    1, 1, 9, 1, 9, 7, 9, 9, 1, 9, 3, 9, 9, 9, 1, 1, 7, 1, 9, 9, 9,
    9, 9, 1, 1, 9, 9, 9, 9, 1, 9, 9, 1, 9, 3, 7, 9, 2, 9, 9, 2, 9,
    9, 6, 9, 9, 9, 1, 9, 2, 1, 1, 9, 3, 3, 9, 5, 9, 9, 9, 9, 8, 6,
    1, 8, 7, 1, 9, 1, 9, 1, 9, 9, 1, 1, 9, 9, 9, 9, 5, 9, 1, 7, 8,
    1, 9, 9, 8, 8, 5)),
    .Names = c("v1", "v2", "v3"),
    row.names = c(NA, 327L),
    questiontype = "PickOneMulti",
    question = "Q4. Frequency of drinking", class = "data.frame")

y <- c(3, 7, 3, 3, 9, 9, 8, 5, 10, 7, 7, 9, 9, 4, 10, 3, 4, 8, 5,
       2, 8, 7, 9, 8, 4, 3, 5, 6, 3, 9, 8, 9, 4, 2, 9, 4, 9, 7, 2, 6,
       9, 7, 9, 6, 7, 3, 5, 6, 6, 7, 2, 9, 5, 3, 6, 4, 9, 4, 10, 2,
       5, 6, 2, 7, 2, 4, 10, 5, 3, 5, 5, 2, 4, 6, 7, 8, 6, 9, 9, 10,
       8, 4, 5, 2, 3, 2, 8, 9, 4, 2, 2, 10, 7, 4, 2, 8, 9, 9, 5, 9,
       2, 2, 7, 5, 2, 4, 2, 2, 4, 10, 8, 7, 5, 6, 6, 5, 2, 6, 9, 8,
       8, 5, 3, 6, 3, 5, 4, 10, 3, 2, 2, 10, 4, 2, 8, 6, 9, 8, 9, 9,
       4, 9, 2, 2, 4, 10, 6, 2, 6, 2, 2, 10, 5, 7, 5, 2, 8, 6, 2, 2,
       4, 3, 3, 3, 3, 4, 4, 7, 6, 5, 8, 9, 8, 8, 8, 9, 6, 5, 3, 3, 6,
       2, 5, 9, 6, 5, 6, 3, 3, 3, 9, 3, 9, 3, 2, 2, 7, 4, 6, 9, 2, 10,
       3, 8, 9, 4, 7, 8, 4, 9, 9, 9, 2, 3, 6, 8, 10, 7, 3, 3, 4, 5,
       3, 10, 10, 6, 6, 10, 2, 10, 2, 8, 6, 9, 2, 9, 9, 8, 9, 5, 9,
       3, 9, 2, 5, 3, 10, 6, 7, 8, 9, 5, 2, 3, 6, 8, 6, 5, 6, 8, 9,
       5, 2, 9, 3, 5, 8, 10, 3, 7, 7, 8, 6, 9, 7, 7, 5, 8, 7, 8, 9,
       2, 3, 10, 7, 8, 4, 10, 9, 10, 3, 4, 9, 4, 4, 9, 9, 8, 6, 5, 7,
       9, 5, 6, 5, 3, 8, 6, 7, 5, 8, 2, 3, 9, 5, 8, 8, 8, 5, 3, 4, 4,
       8, 4, 2, 4, 8)

w <- structure(c(1.02849002849003, 0.587708587708588, 0.587708587708588,
                 1.61619861619862, 1.02849002849003, 0.293854293854294, 0.440781440781441,
                 1.46927146927147, 0.440781440781441, 1.02849002849003, 1.02849002849003,
                 1.02849002849003, 0.440781440781441, 0.734635734635735, 0.587708587708588,
                 0.734635734635735, 0.587708587708588, 0.587708587708588, 1.02849002849003,
                 0.293854293854294, 0.734635734635735, 0.293854293854294, 1.02849002849003,
                 0.881562881562882, 0.587708587708588, 0.734635734635735, 0.587708587708588,
                 0.587708587708588, 0.293854293854294, 1.17541717541718, 0.881562881562882,
                 0.734635734635735, 0.734635734635735, 0.587708587708588, 0.734635734635735,
                 0.440781440781441, 0.587708587708588, 1.61619861619862, 0.293854293854294,
                 0.734635734635735, 0.293854293854294, 0.881562881562882, 0.734635734635735,
                 1.32234432234432, 1.61619861619862, 0.881562881562882, 0.734635734635735,
                 0.734635734635735, 1.61619861619862, 0.734635734635735, 0.587708587708588,
                 1.02849002849003, 1.61619861619862, 1.46927146927147, 0.587708587708588,
                 1.61619861619862, 0.587708587708588, 0.587708587708588, 0.293854293854294,
                 0.440781440781441, 0.734635734635735, 1.46927146927147, 0.440781440781441,
                 0.587708587708588, 1.02849002849003, 0.881562881562882, 0.587708587708588,
                 0.146927146927147, 0.587708587708588, 0.587708587708588, 1.17541717541718,
                 0.146927146927147, 0.587708587708588, 0.293854293854294, 0.734635734635735,
                 0.734635734635735, 1.61619861619862, 0.734635734635735, 0.587708587708588,
                 1.61619861619862, 0.734635734635735, 0.734635734635735, 1.61619861619862,
                 0.587708587708588, 0.293854293854294, 0.881562881562882, 0.587708587708588,
                 1.02849002849003, 1.17541717541718, 0.146927146927147, 1.32234432234432,
                 0.587708587708588, 0.734635734635735, 0.881562881562882, 1.61619861619862,
                 0.734635734635735, 0.881562881562882, 0.293854293854294, 0.440781440781441,
                 1.32234432234432, 0.293854293854294, 0.440781440781441, 1.61619861619862,
                 0.734635734635735, 0.293854293854294, 0.734635734635735, 1.61619861619862,
                 1.46927146927147, 1.02849002849003, 0.587708587708588, 0.881562881562882,
                 0.587708587708588, 0.587708587708588, 1.46927146927147, 0.587708587708588,
                 0.293854293854294, 0.440781440781441, 1.32234432234432, 0.734635734635735,
                 0.734635734635735, 1.02849002849003, 1.17541717541718, 1.17541717541718,
                 0.293854293854294, 1.32234432234432, 0.440781440781441, 0.734635734635735,
                 0.293854293854294, 0.734635734635735, 0.734635734635735, 0.734635734635735,
                 0.293854293854294, 1.61619861619862, 1.17541717541718, 1.02849002849003,
                 0.734635734635735, 0.146927146927147, 0.734635734635735, 0.440781440781441,
                 0.440781440781441, 0.587708587708588, 1.17541717541718, 1.32234432234432,
                 0.587708587708588, 1.02849002849003, 0.146927146927147, 0.734635734635735,
                 0.293854293854294, 1.02849002849003, 0.146927146927147, 0.881562881562882,
                 0.587708587708588, 0.881562881562882, 0.734635734635735, 0.293854293854294,
                 0.440781440781441, 0.587708587708588, 0.293854293854294, 0.293854293854294,
                 0.734635734635735, 0.881562881562882, 1.32234432234432, 1.61619861619862,
                 1.61619861619862, 0.881562881562882, 1.02849002849003, 0.440781440781441,
                 0.440781440781441, 1.61619861619862, 0.881562881562882, 1.61619861619862,
                 0.293854293854294, 1.17541717541718, 0.293854293854294, 1.32234432234432,
                 0.440781440781441, 0.293854293854294, 1.17541717541718, 0.881562881562882,
                 0.734635734635735, 0.293854293854294, 1.32234432234432, 0.587708587708588,
                 1.61619861619862, 1.17541717541718, 0.881562881562882, 0.881562881562882,
                 1.61619861619862, 0.881562881562882, 0.734635734635735, 0.440781440781441,
                 0.734635734635735, 0.440781440781441, 0.881562881562882, 0.587708587708588,
                 0.440781440781441, 1.61619861619862, 1.32234432234432, 0.734635734635735,
                 0.734635734635735, 0.146927146927147, 0.587708587708588, 1.17541717541718,
                 0.440781440781441, 0.734635734635735, 0.881562881562882, 1.17541717541718,
                 0.293854293854294, 1.32234432234432, 1.02849002849003, 1.32234432234432,
                 1.61619861619862, 1.02849002849003, 0.440781440781441, 0.881562881562882,
                 1.61619861619862, 0.587708587708588, 1.17541717541718, 0.881562881562882,
                 0.734635734635735, 1.61619861619862, 1.17541717541718, 0.587708587708588,
                 1.61619861619862, 0.293854293854294, 0.440781440781441, 1.02849002849003,
                 1.61619861619862, 0.587708587708588, 0.734635734635735, 0.881562881562882,
                 0.734635734635735, 0.734635734635735, 0.881562881562882, 0.293854293854294,
                 0.146927146927147, 0.734635734635735, 0.587708587708588, 0.734635734635735,
                 0.881562881562882, 0.146927146927147, 0.881562881562882, 1.17541717541718,
                 1.61619861619862, 1.61619861619862, 1.02849002849003, 1.61619861619862,
                 0.734635734635735, 0.440781440781441, 0.881562881562882, 1.17541717541718,
                 0.881562881562882, 0.734635734635735, 0.734635734635735, 0.734635734635735,
                 1.32234432234432, 0.734635734635735, 1.17541717541718, 0.587708587708588,
                 1.61619861619862, 0.587708587708588, 1.32234432234432, 0.734635734635735,
                 0.293854293854294, 0.734635734635735, 0.440781440781441, 0.440781440781441,
                 0.293854293854294, 0.587708587708588, 0.734635734635735, 0.734635734635735,
                 0.881562881562882, 0.734635734635735, 0.734635734635735, 1.61619861619862,
                 0.587708587708588, 0.881562881562882, 0.293854293854294, 1.02849002849003,
                 0.881562881562882, 0.734635734635735, 0.587708587708588, 0.881562881562882,
                 0.440781440781441, 0.734635734635735, 0.734635734635735, 0.881562881562882,
                 1.61619861619862, 0.440781440781441, 0.587708587708588, 0.587708587708588,
                 0.734635734635735, 0.440781440781441, 0.587708587708588, 1.17541717541718,
                 0.734635734635735, 0.293854293854294, 0.734635734635735, 0.734635734635735,
                 0.734635734635735, 0.587708587708588, 1.61619861619862, 0.881562881562882,
                 0.587708587708588, 0.587708587708588, 0.734635734635735, 0.734635734635735,
                 1.02849002849003, 1.17541717541718, 0.734635734635735, 0.881562881562882,
                 0.587708587708588, 0.881562881562882, 0.881562881562882, 0.881562881562882,
                 0.293854293854294, 0.440781440781441, 0.881562881562882, 0.734635734635735,
                 0.881562881562882, 1.02849002849003, 0.587708587708588, 0.587708587708588,
                 0.587708587708588, 0.734635734635735, 0.587708587708588, 0.440781440781441),
               name = "Q32", label = "Q32. Income")

dat <- cbind(y, X)

test_that("Relative importance linear", {
    ria <- flipRegression:::estimateRelativeImportance(y ~ v1 + v2 + v3, data = dat, weights = NULL,
                                                       type = "Linear", signs = c(1, 1 ,1),
                                                       r.square = 0.0409055316886271,
                                                       variable.names = LETTERS[1:3], robust.se = FALSE,
                                                       show.warnings = TRUE, correction = "None")
    expect_equal(unname(ria$importance[3]), 84.254254422183)
    expect_equal(unname(ria$raw.importance[1]), 0.00427583141764991)
    expect_equal(unname(ria$standard.errors[2]), 0.00639909659943047)
    expect_equal(unname(ria$statistics[3]), 1.67707778306778)
    expect_equal(unname(ria$p.values[1]), 0.601684127723725)
})

test_that("Relative importance linear weighted", {
    ria <- flipRegression:::estimateRelativeImportance(y ~ v1 + v2 + v3, data= dat, weights = w,
                                                       type = "Linear", signs = c(1, 1, 1),
                                                       r.square = 0.0488985219292419, variable.names = LETTERS[1:3],
                                                       robust.se = FALSE, outlier.proportion = 0,
                                                       show.warnings = TRUE, correction = "None")
    expect_equal(unname(ria$importance[3]), 80.657438103125)
    expect_equal(unname(ria$raw.importance[1]), 0.00356269285452153)
    expect_equal(unname(ria$standard.errors[2]), 0.00922207572739253)
    expect_equal(unname(ria$statistics[3]), 1.80433377885404)
    expect_equal(unname(ria$p.values[1]), 0.639743624224031)
})

types <- c("Linear", "Binary Logit", "Ordered Logit", "Poisson", "Quasi-Poisson", "NBD")
output <- "Relative Importance Analysis"

data(bank, package = "flipExampleData")

for (t in types)
    test_that(paste("Relative importance", t),
              expect_error(suppressWarnings(print(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                                                   data = bank, type = t, output = output))), NA))
test_that("Relative importance Multinomial Logit",
          expect_error(suppressWarnings(print(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                                         data = bank, type = "Multinomial Logit", output = output))),
                                        "Type not handled:  Multinomial Logit"))

# Weights
for (t in types)
    test_that(paste("Relative importance weighted", t),
              expect_error(suppressWarnings(print(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                                               data = bank, type = t, output = output,
                                               weights = bank$weight))), NA))
test_that("Relative importance weighted Multinomial Logit",
          expect_error(suppressWarnings(print(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                                           data = bank, type = "Multinomial Logit", output = output,
                                           weights = bank$weight))), "Type not handled:  Multinomial Logit"))

# Filter
test_that("Relative importance filtered",
          expect_error(suppressWarnings(print(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                                               data = bank, type = "Linear", output = output,
                                               subset = bank$ID < 100))), NA))

# Robust standard error
test_that("Relative importance robust SE",
          expect_error(suppressWarnings(print(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                                               data = bank, type = "Linear", output = output,
                                               robust.se = F))), NA))

# Negative sign warning
test_that("Relative importance negative sign",
{
          expect_warning(flipRegression:::estimateRelativeImportance(y ~ v1 + v2 + v3, dat, NULL, "Linear", c(1, -1 ,1),
                                                             0.0409055316886271, variable.names = LETTERS[1:3], correction = "None"),
                         paste0("Negative signs in Relative Importance scores were applied from coefficient signs",
                                " in Linear Regression. To disable this feature, check the Absolute importance",
                                " scores option."))

    res <- Regression(y~v1+v2+v3, dat, output = "Relative Importance Analysis", missing = "Multiple imputation", importance.absolute = TRUE)
    expect_true(all(res$importance$importance > 0))
})

X.factor <- X
X.factor[[1]] <- as.factor(X.factor[[1]])
X.factor[[2]] <- as.factor(X.factor[[2]])
X.factor[[3]] <- as.factor(X.factor[[3]])
dat.factor <- cbind(y, X.factor)

# Factor warning
test_that("Relative importance ordered factor",
          expect_warning(flipRegression:::estimateRelativeImportance(y ~ v1 + v2 + v3, data = dat.factor,
                                                                     weights = NULL, type = "Linear",
                                                                     signs = c(1, -1 ,1),
                                                                     r.square = 0.0409055316886271,
                                                                     variable.names = LETTERS[1:3],
                                                                     correction = "None"),
                         "The following variables have been treated as categorical: v1,v2,v3. This may over-inflate their effects."))

test_that("Relative importance robust SE, dot in formula",
{
    bank$ID <- bank$weight <- NULL
    out <- suppressWarnings(Regression(Overall ~ .,
                                               data = bank, type = "Linear", output = output,
                                       robust.se = F))
    expect_equal(attr(out$terms, "term.labels"), names(bank)[-1L])
})

test_that("Shapley",
{
    bank.no.missing <- bank[!is.na(rowSums(bank)), ]
    bank.no.missing$Interest <- -bank.no.missing$Interest # reverse sign to test signs

    warning.msg <- paste0("Negative signs in Relative Importance scores were applied ",
                          "from coefficient signs in Linear Regression. ",
                          "To disable this feature, check the Absolute importance ",
                          "scores option.")

    expect_warning(result <- computeShapleyImportance(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                                      data = bank.no.missing,
                                      weights = rep(1, nrow(bank.no.missing)),
                                      variable.names = c("Fees", "Interest", "Phone",
                                                         "Branch", "Online", "ATM"),
                                      signs = NULL,
                                      correction = "None"), warning.msg)

    expect_equal(result$raw.importance[1], c(Fees = 0.115570678291964))
    expect_equal(sum(result$raw.importance), 0.4988654351715)
    expect_equal(result$importance[1], c(Fees = 23.1667039133))
    expect_equal(result$standard.errors[1], c(Fees = 0.03113091404327))
    expect_equal(result$statistics[1], c(Fees = 3.712408769345))
    expect_equal(result$p.values[1], c(Fees = 0.0002559481304636))
    expect_equal(result$importance[2], c(Interest = -15.1397373957463))

    expect_warning(result <- computeShapleyImportance(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                                                      data = bank.no.missing,
                                                      weights = bank.no.missing$weight,
                                                      variable.names = c("Fees", "Interest", "Phone",
                                                                         "Branch", "Online", "ATM"),
                                                      signs = NULL,
                                                      correction = "None"), warning.msg)
    expect_equal(result$raw.importance[1], c(Fees = 0.1090244509126))
    expect_equal(sum(result$raw.importance), 0.4936505179077)
    expect_equal(result$importance[1], c(Fees = 22.08535126726))
    expect_equal(result$standard.errors[1], c(Fees = 0.0302990327033463))
    expect_equal(result$statistics[1], c(Fees = 3.59828156826045))
    expect_equal(result$p.values[1], c(Fees = 0.000389921200592241))

    suppressWarnings(print(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                     data = bank, type = "Linear", output = "Shapley Regression")))

    expect_error(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                            data = bank, type = "Binary Logit", output = "Shapley Regression"),
                 "Shapley requires Regression type to be Linear. Set the output to Relative Importance Analysis instead.")

    many.variables <- matrix(rnorm(3000), ncol = 30)
    colnames(many.variables) <- paste0("v", 1:30)
    many.variables <- data.frame(many.variables)
    frml <- formula(paste("v1", "~", paste0("v", 2:30, collapse = " + ")))
    expect_error(result <- computeShapleyImportance(frml,
                                                   data = many.variables,
                                                   weights = NULL,
                                                   variable.names = paste0("v", 2:30),
                                                   signs = NULL,
                                                   correction = "None"),
        "Shapley can run with a maximum of 27 predictors. Set the output to Relative Importance Analysis instead.")
})

test_that("Dummy variable adjustment valid", {
    # Simulate correlated predictors
    set.seed(12321)
    X <- MASS::mvrnorm(n = 200, mu = rep(0, 3), Sigma = matrix(c(1, 0.2, 0.3, 0.2, 1, 0.3, 0.2, 0.2, 1), ncol = 3))
    beta <- c(0.4, 0.3, 0.25)
    r2 <- 0.30

    Y <- X %*% beta + rnorm(n = 200)

    not.missing.data <- data.frame(Y, X)
    Xm <- X
    missing <- sample(1:nrow(X), size = 50, replace = FALSE)
    Xm[, 2][missing] <- NA
    missing.data <- data.frame(Y, Xm)
    for(out in c("Relative Importance Analysis", "Shapley Regression"))
    {
        expect_warning(z <- Regression(Y ~ X1 + X2 + X3, data = not.missing.data, output = out),
                       NA)
        expect_warning(print(z), "Unusual observations detected")
        expect_warning(z <- Regression(Y ~ X1 + X2 + X3, data = missing.data, output = out, missing = "Dummy variable adjustment"),
                       NA)
        expect_warning(print(z), "Unusual observations detected")
    }
})


test_that("DS-2876: Jaccard Output", {
    # Basic Jaccard Coefficient tests
    set.seed(123)
    n <- 10
    x <- rbinom(n, size = 1, prob = 0.5)
    y <- rbinom(n, size = 1, prob = 0.5)
    weights <- w <- runif(n)
    p.x <- mean(x); p.y <- mean(y)

    computed.jaccard <- sum(x & y)/sum(x | y)
    weighted.jaccard <- sum(w[x & y])/sum(w[x | y])
    x.missing <- x; x.missing[5] <- NA; y.missing <- y; y.missing[6] <- NA
    px.missing <- mean(x.missing, na.rm = TRUE); py.missing <- mean(y.missing, na.rm = TRUE);

    # Computed Jaccard tests
    computed.missing.jaccard <- sum(x.missing & y.missing, na.rm = TRUE)/sum(x.missing | y.missing, na.rm = TRUE)
    expect_equal(flipRegression:::singleJaccardCoefficient(x, y), computed.jaccard)
    expect_equal(flipRegression:::singleJaccardCoefficient(x.missing, y.missing), computed.missing.jaccard)
    expect_equal(flipRegression:::singleJaccardCoefficient(x, y, weights = weights), weighted.jaccard)
    # Expectation tests
    computed.mean <- p.x * p.y/(p.x + p.y - p.x * p.y)
    expect_equal(flipRegression:::singleJaccardExpectation(x, y), computed.mean)
    centered.computed <- computed.jaccard - computed.mean
    # Centered values tests
    expect_equal(flipRegression:::singleJaccardCoefficient(x, y, centered = TRUE), centered.computed)
    # Larger test
    set.seed(12321)
    n <- 100
    X <- lapply(1:3, function(x) rbinom(n = n, size = 1, prob = 0.5))
    X <- as.matrix(as.data.frame(X))
    beta <- matrix(c(1, 4, 9, 2), ncol = 1)
    # Expect X2 and Y to be the same value, hence small p-value
    Y <- as.numeric(cbind(1, X) %*% beta >= 12)
    dat <- data.frame(Y, X)
    subset <- sample(c(TRUE, FALSE), size = n, replace = TRUE)
    weights <- runif(n)
    names(dat) <- c("Y", paste0("X", 1:3))
    new.names <- c("Super Y", "Oranges", "Apples", "Grapes")
    names.with.prefix <- paste0("Q4:", new.names)
    # Create dataframe with label attributes for questions
    dat.with.names <- as.data.frame(mapply(function(x, y) {
        attr(x, "label") <- y
        x
    }, x = dat, y = new.names, SIMPLIFY = FALSE))
    dat.with.prefix.names <- as.data.frame(mapply(function(x, y) {
        attr(x, "label") <- y
        x
    }, x = dat, y = names.with.prefix, SIMPLIFY = FALSE))
    # Check Importance output correct
    expect_error(model <- Regression(Y ~ X1 + X2 + X3, data = dat,
                                     output = "Jaccard Coefficient", type = "Linear"),
                 NA)
    expect_error(corrected.model <- Regression(Y ~ X1 + X2 + X3, data = dat,
                                               output = "Jaccard Coefficient", type = "Linear",
                                               correction = "False Discovery Rate"),
                 NA)
    expect_error(weighted.model <- Regression(Y ~ X1 + X2 + X3, data = dat, weights = weights,
                                              output = "Jaccard Coefficient", type = "Linear"),
                 NA)
    expect_error(subsetted.model <- Regression(Y ~ X1 + X2 + X3, data = dat, subset = subset,
                                               output = "Jaccard Coefficient", type = "Linear"),
                 NA)
    expect_error(fancy.names.model <- Regression(Y ~ X1 + X2 + X3, data = dat.with.names, show.labels = TRUE,
                                                 output = "Jaccard Coefficient", type = "Linear"),
                 NA)
    expect_error(prefix.fancy.names.model <- Regression(Y ~ X1 + X2 + X3, data = dat.with.prefix.names, show.labels = TRUE,
                                                        output = "Jaccard Coefficient", type = "Linear"),
                 NA)
    # Check relative importance element exists
    expect_identical(model$importance, model$relative.importance)
    # Model output the same
    expect_equal(model$importance, fancy.names.model$importance, check.attributes = FALSE)
    # Names are used
    expect_identical(names(fancy.names.model$importance$raw.importance), new.names[-1])
    expect_identical(names(fancy.names.model$importance$importance), new.names[-1])
    # Prefix is removed
    expect_identical(names(prefix.fancy.names.model$importance$raw.importance), new.names[-1])
    expect_identical(names(prefix.fancy.names.model$importance$importance), new.names[-1])
    # Expect jaccard coefficients to be the same as computed manually
    expect_equal(model$importance$raw.importance,
                 vapply(dat[-1], flipRegression:::singleJaccardCoefficient, numeric(1), y = dat[[1]]))
    # Relative importance values computed correctly as the relative sizes of the t-statistics
    tests <- lapply(dat[-1], flipRegression:::jaccardTest, y = dat[[1]], weights = rep(1, nrow(dat)))
    t.stats <- vapply(tests, "[[", numeric(1), "t")
    expect_equal(model$importance$importance, 100 * prop.table(t.stats))
    # Check weights applied to coefficients
    expect_equal(weighted.model$importance$raw.importance,
                 vapply(dat[-1], flipRegression:::singleJaccardCoefficient, numeric(1), y = dat[[1]], weights = CalibrateWeight(weights)))
    # subsetted model has correct coefficients
    expect_equal(subsetted.model$importance$raw.importance,
                 vapply(dat[subset, -1], flipRegression:::singleJaccardCoefficient, numeric(1), y = dat[[1]][subset]))
    # Expect p-values to be significant when predictor used in regression
    expect_equal(model$importance$p.values, c(X1 = 0.074807, X2 = 0, X3 = 0.000411), tolerance = 1e-6)
    expect_equal(corrected.model$importance$p.values, flipRegression:::pvalAdjust(c(X1 = 0.074807, X2 = 0, X3 = 0.000411),
                                                                                  "False Discovery Rate"),
                 tolerance = 1e-6, check.attributes = FALSE)
    # Expect p-values to be insignificant when predictor not used
    set.seed(12321)
    insig.dat <- as.data.frame(lapply(rep(100, 4), rbinom, size = 1, prob = 0.5))
    names(insig.dat) <- c("Y", paste0("X", 1:3))
    expect_error(insig.model <- Regression(Y ~ ., data = insig.dat, output = "Jaccard Coefficient"), NA)
    expect_true(all(insig.model$importance$p.values > 0.05))
    expect_equal(insig.model$importance$p.values, c(X1 = 0.594830988348535, X2 = 0.62906440688862, X3 = 0.249175392296114))
    expect_equal(model$importance$p.values, c(X1 = 0.074807, X2 = 0, X3 = 0.000411), tolerance = 1e-6)
    # Missing data tests (see above Jaccard computations and the following)
    dat.with.missing <- dat
    dat.with.missing[1, 2] <- NA
    expect_error(Regression(Y ~ X1 + X2 + X3, data = dat.with.missing,
                            output = "Jaccard Coefficient", missing = "Error if missing data"),
                 "The data contains missing values. Change the 'missing' option to run the analysis.", fixed = TRUE)
    expect_error(excluded.missing.model <- Regression(Y ~ X1 + X2 + X3, data = dat.with.missing,
                                                      output = "Jaccard Coefficient", missing = "Exclude cases with missing data"), NA)
    expect_equal(excluded.missing.model$importance$sample.size, c(X1 = n - 1, X2 = n - 1, X3 = n - 1))
    expect_error(partial.missing.model <- Regression(Y ~ X1 + X2 + X3, data = dat.with.missing,
                                                     output = "Jaccard Coefficient", missing = "Use partial data (pairwise correlations)"), NA)
    expect_equal(partial.missing.model$importance$sample.size, c(X1 = n - 1, X2 = n, X3 = n))
    dat.with.missing <- as.data.frame(lapply(dat, function(x) {
       x[sample(c(TRUE, FALSE), size = length(x), replace = TRUE)] <- NA
       x
    }))
    expect_error(Regression(Y ~ X1 + X2 + X3, data = dat.with.missing, missing = "Error if missing data"),
                 "The data contains missing values. Change the 'missing' option to run the analysis.", fixed = TRUE)
    # Check predict method works with missing data
    expect_error(predictions <- predict(excluded.missing.model), NA)
    expect_true(sum(is.na(predictions)) == 1)
    expect_error(predictions <- predict(partial.missing.model),
                 "'predict' not available when 'missing' = Use partial data (pairwise correlations)",
                 fixed = TRUE)
    # Check model works with interaction
    dat.with.interaction <- dat
    dat.with.interaction$int <- factor(sample(LETTERS[1:3], size = n, replace = TRUE))
    expect_error(int.model <- Regression(Y ~ X1 + X2 + X3, data = dat.with.interaction,
                                         interaction = int, output = "Jaccard Coefficient"),
                 NA)
    # Check interaction with missing values is handled
    dat.with.missing.interaction <- dat.with.interaction
    missing.int <- dat.with.interaction$int
    missing.int[sample(c(TRUE, FALSE), size = nrow(dat.with.missing.interaction), replace = TRUE, prob = c(1, 5))] <- NA
    dat.with.missing.interaction$int <- missing.int
    expect_error(int.model <- Regression(Y ~ X1 + X2 + X3, data = dat.with.missing.interaction,
                                         interaction = int, output = "Jaccard Coefficient"),
                 NA)
    # Allow categorical outcome variables for Jaccard
    dat$Y <- factor(sample(LETTERS[1:2], size = nrow(dat), replace = TRUE))
    dat$Y3 <- factor(sample(LETTERS[1:3], size = nrow(dat), replace = TRUE))
    # Allow categorical variable with two levels
    expect_error(category.2 <- Regression(Y ~ X1 + X2, data = dat, output = "Jaccard Coefficient"), NA)
    # Check the output equivalent where outcome is numeric binary
    dat$Yb <- unclass(dat$Y) - 1
    expect_error(num.binary <- Regression(Yb ~ X1 + X2, data = dat, output = "Jaccard Coefficient"), NA)
    expect_identical(category.2$importance, num.binary$importance)
    # Check the outcome variable is dichotomized into a numeric binary variable
    expect_warning(dichot <- Regression(Y3 ~ X1 + X2, data = dat, output = "Jaccard Coefficient"),
                   "Y3 has been dichotimized into <= A & >= B", fixed = TRUE)
    expect_setequal(dichot$model$Y3, c(0, 1))
    # Check output equivalent to numeric binary outcome
    dat$Yb <- as.numeric(dat$Y3 %in% LETTERS[2:3])
    expect_error(num.binary <- Regression(Yb ~ X1 + X2, data = dat, output = "Jaccard Coefficient"), NA)
    expect_identical(dichot$importance, num.binary$importance)
})

test_that("DS-2876: Correlation Output", {
    # Output is computed using the flipStatistics::CorrelationsWithSignificance function
    set.seed(12321)
    n <- 100
    X <- lapply(1:3, function(x) rnorm(n = n))
    X <- as.matrix(as.data.frame(X))
    beta <- matrix(c(1, 4, 9, 2), ncol = 1)
    # Expect X2 and Y to be the same value, hence small p-value
    Y <- cbind(1, X) %*% beta + rnorm(n)
    dat <- data.frame(Y, X)
    subset <- sample(c(TRUE, FALSE), size = n, replace = TRUE)
    weights <- runif(n)
    names(dat) <- c("Y", paste0("X", 1:3))
    new.names <- c("Super Y", "Oranges", "Apples", "Grapes")
    names.with.prefix <- paste0("Q4:", new.names)
    # Create dataframe with label attributes for questions
    dat.with.names <- as.data.frame(mapply(function(x, y) {
        attr(x, "label") <- y
        x
    }, x = dat, y = new.names, SIMPLIFY = FALSE))
    dat.with.prefix.names <- as.data.frame(mapply(function(x, y) {
        attr(x, "label") <- y
        x
    }, x = dat, y = names.with.prefix, SIMPLIFY = FALSE))
    # Check Importance output correct
    expect_error(model <- Regression(Y ~ X1 + X2 + X3, data = dat,
                                     output = "Correlation", type = "Linear"),
                 NA)
    expect_error(corrected.model <- Regression(Y ~ X1 + X2 + X3, data = dat,
                                               output = "Correlation", type = "Linear",
                                               correction = "False Discovery Rate"),
                 NA)
    expect_error(weighted.model <- Regression(Y ~ X1 + X2 + X3, data = dat, weights = weights,
                                              output = "Correlation", type = "Linear"),
                 NA)
    expect_error(subsetted.model <- Regression(Y ~ X1 + X2 + X3, data = dat, subset = subset,
                                               output = "Correlation", type = "Linear"),
                 NA)
    expect_error(sub.and.weighted.model <- Regression(Y ~ X1 + X2 + X3, data = dat, subset = subset, weights = weights,
                                                      output = "Correlation", type = "Linear"),
                 NA)
    expect_error(fancy.names.model <- Regression(Y ~ X1 + X2 + X3, data = dat.with.names, show.labels = TRUE,
                                                 output = "Correlation", type = "Linear"),
                 NA)
    expect_error(prefix.fancy.names.model <- Regression(Y ~ X1 + X2 + X3, data = dat.with.prefix.names, show.labels = TRUE,
                                                        output = "Correlation", type = "Linear"),
                 NA)
    # Check relative importance element exists
    expect_identical(model$importance, model$relative.importance)
    # Model output the same
    expect_equal(model$importance, fancy.names.model$importance, check.attributes = FALSE)
    # Names are used
    expect_identical(names(fancy.names.model$importance$raw.importance), new.names[-1])
    expect_identical(names(fancy.names.model$importance$importance), new.names[-1])
    # Prefix is removed
    expect_identical(names(prefix.fancy.names.model$importance$raw.importance), new.names[-1])
    expect_identical(names(prefix.fancy.names.model$importance$importance), new.names[-1])
    # Check caclulated values agree with dependency
    basic.correlation.output <- flipStatistics::CorrelationsWithSignificance(dat, weights = rep(1, n), spearman = FALSE)
    subsetted.correlation.output <- flipStatistics::CorrelationsWithSignificance(dat[subset, ], weights = rep(1, sum(subset)), spearman = FALSE)
    weighted.correlation.output <- flipStatistics::CorrelationsWithSignificance(dat, weights = weights, spearman = FALSE)
    sub.and.weighted.correlation.output <- flipStatistics::CorrelationsWithSignificance(dat[subset, ], weights = weights[subset], spearman = FALSE)
    # Expect Correlation output to be the same as computed manually using the helper function above
    expect_equal(model$importance$raw.importance, basic.correlation.output$cor[-1, 1])
    # Relative importance values computed correctly
    expect_equal(model$importance$importance, 100 * prop.table(model$importance$raw.importance))
    # Check weights, subsets and both applied to coefficients
    expect_equal(weighted.model$importance$raw.importance, weighted.correlation.output$cor[-1, 1])
    expect_equal(subsetted.model$importance$raw.importance, subsetted.correlation.output$cor[-1, 1])
    expect_equal(sub.and.weighted.model$importance$raw.importance, sub.and.weighted.correlation.output$cor[-1, 1])
    # Check p-values are consistent
    expect_equal(model$importance$p.values, basic.correlation.output$p[-1, 1])
    expect_equal(weighted.model$importance$p.values, weighted.correlation.output$p[-1, 1])
    expect_equal(subsetted.model$importance$p.values, subsetted.correlation.output$p[-1, 1])
    expect_equal(sub.and.weighted.model$importance$p.values, sub.and.weighted.correlation.output$p[-1, 1])
    expect_equal(corrected.model$importance$p.values, flipRegression:::pvalAdjust(basic.correlation.output$p[-1, 1], "False Discovery Rate"))
    # Expect p-values to be insignificant when predictor not used
    set.seed(12321)
    insig.dat <- as.data.frame(lapply(rep(100, 4), rnorm))
    names(insig.dat) <- c("Y", paste0("X", 1:3))
    expect_error(insig.model <- Regression(Y ~ ., data = insig.dat, output = "Correlation"), NA)
    expect_true(all(insig.model$importance$p.values > 0.05))
    expect_equal(insig.model$importance$p.values, c(X1 = 0.809554824521213, X2 = 0.0616307880261012, X3 = 0.22312934720278))
    # Missing data tests
    dat.with.missing <- dat
    dat.with.missing[1, 2] <- NA
    subset <- rep(c(TRUE, FALSE), c(90,  10))
    expect_error(Regression(Y ~ X1 + X2 + X3, data = dat.with.missing,
                            output = "Correlation", missing = "Error if missing data"),
                 "The data contains missing values. Change the 'missing' option to run the analysis.", fixed = TRUE)
    expect_error(excluded.missing.model <- Regression(Y ~ X1 + X2 + X3, data = dat.with.missing,
                                                      output = "Correlation", missing = "Exclude cases with missing data"), NA)
    expect_equal(excluded.missing.model$importance$sample.size, c(X1 = n - 1, X2 = n - 1, X3 = n - 1))
    expect_error(partial.missing.model <- Regression(Y ~ X1 + X2 + X3, data = dat.with.missing,
                                                     output = "Correlation", missing = "Use partial data (pairwise correlations)"), NA)
    expect_error(subset.partial.missing.model <- Regression(Y ~ X1 + X2 + X3, data = dat.with.missing, subset = subset,
                                                             output = "Correlation", missing = "Use partial data (pairwise correlations)"), NA)
    expect_error(weighted.subset.partial.missing.model <- Regression(Y ~ X1 + X2 + X3, data = dat.with.missing,
                                                                     subset = subset, weights = weights,
                                                                     output = "Correlation",
                                                                     missing = "Use partial data (pairwise correlations)"), NA)
    expect_equal(partial.missing.model$importance$sample.size, c(X1 = n - 1, X2 = n, X3 = n))
    dat.with.missing <- as.data.frame(lapply(dat, function(x) {
        x[sample(c(TRUE, FALSE), size = length(x), replace = TRUE)] <- NA
        x
    }))
    expect_error(Regression(Y ~ X1 + X2 + X3, data = dat.with.missing, missing = "Error if missing data"),
                 "The data contains missing values. Change the 'missing' option to run the analysis.", fixed = TRUE)
    # Check predict method works with missing data
    expect_error(predictions <- predict(excluded.missing.model), NA)
    expect_true(sum(is.na(predictions)) == 1)
    expect_error(predictions <- predict(partial.missing.model),
                 "'predict' not available when 'missing' = Use partial data (pairwise correlations)",
                 fixed = TRUE)
    # Check model works with interaction
    dat.with.interaction <- dat
    dat.with.interaction$int <- factor(sample(LETTERS[1:3], size = n, replace = TRUE))
    expect_error(int.model <- Regression(Y ~ X1 + X2 + X3, data = dat.with.interaction,
                                         interaction = int, output = "Correlation"),
                 NA)
    expect_error(subset.int.model <- Regression(Y ~ X1 + X2 + X3, data = dat.with.interaction,
                                                subset = subset,
                                                interaction = int, output = "Correlation"),
                 NA)
    expect_error(subset.weight.int.model <- Regression(Y ~ X1 + X2 + X3, data = dat.with.interaction,
                                                       subset = subset, weights = weights,
                                                       interaction = int, output = "Correlation",
                                                       missing = "Use partial data (pairwise correlations)"),
                 NA)
    # Check interaction with missing values is handled
    dat.with.missing.interaction <- dat.with.interaction
    missing.int <- dat.with.interaction$int
    missing.int[sample(c(TRUE, FALSE), size = nrow(dat.with.missing.interaction), replace = TRUE, prob = c(1, 5))] <- NA
    dat.with.missing.interaction$int <- missing.int
    expect_error(int.model <- Regression(Y ~ X1 + X2 + X3, data = dat.with.missing.interaction,
                                         interaction = int, output = "Correlation"),
                 NA)
    expect_error(int.model.with.partial.data <- Regression(Y ~ X1 + X2 + X3, data = dat.with.missing.interaction,
                                                           interaction = int, output = "Correlation",
                                                           missing = "Use partial data (pairwise correlations)"),
                 NA)
    expect_error(sub.int.model.with.partial.data <- Regression(Y ~ X1 + X2 + X3, data = dat.with.missing.interaction,
                                                           interaction = int, output = "Correlation", subset = subset,
                                                           missing = "Use partial data (pairwise correlations)"),
                 NA)
    # Check categorical variables are coerced to numeric in Correlation outputs
    dat.with.categ <- dat
    dat.with.categ$X4 <- factor(sample(1:10, replace = TRUE, size = nrow(dat)))
    expect_warning(categ.pred <- Regression(Y ~ X1 + X2 + X3 + X4, data = dat.with.categ,
                                            type = "Linear", output = "Correlation"),
                   "The variable X4 has been converted\\.$", perl = TRUE)
    # Computed importance uses factor coerced to numeric
    dat.categ.to.numeric <- lapply(dat.with.categ, unclass)
    expected.cor <- vapply(dat.categ.to.numeric[-1], function(x) cor(x, dat.categ.to.numeric[[1L]]), numeric(1))
    expect_equal(categ.pred$importance$raw.importance, expected.cor)
    categ.pred$importance$raw.importance
    # Labels shown in warning during conversion
    dat.with.fancy.categ <- dat.with.categ
    attr(dat.with.fancy.categ$X4, "label") <- "Some factor"
    expect_warning(Regression(Y ~ X1 + X2 + X3 + X4, data = dat.with.fancy.categ,
                              type = "Linear", output = "Correlation"),
                   "The variable Some factor \\(X4\\) has been converted\\.$", perl = TRUE)
})

test_that("Check error message when Categorical predictors used in RIA", {
    data(bank, package = "flipExampleData")
    # Expect no error if numeric predictors used
    expect_error(Regression(Overall ~ Fees + Interest, data = bank,
                            output = "Relative Importance Analysis",
                            missing = "Dummy variable adjustment"),
                 NA)
    # Expect error if factor or ordered factor used
    bank$Interest <- factor(bank$Interest)
    expect_error(Regression(Overall ~ Fees + Interest, data = bank,
                            output = "Relative Importance Analysis",
                            missing = "Dummy variable adjustment"),
                 paste0("Dummy variable adjustment method for missing data is not supported for categorical ",
                        "predictor variables in Relative Importance Analysis. Please remove the categorical ",
                        "predictors: 'Interest' and re-run the analysis"), fixed = TRUE)
    attr(bank$Interest, "label") <- "Interest charged by the bank"
    expect_error(Regression(Overall ~ Fees + Interest, data = bank,
                            output = "Relative Importance Analysis",
                            missing = "Dummy variable adjustment",
                            show.labels = TRUE),
                 paste0("Dummy variable adjustment method for missing data is not supported for categorical ",
                        "predictor variables in Relative Importance Analysis. Please remove the categorical ",
                        "predictors: 'Interest charged by the bank' and re-run the analysis"), fixed = TRUE)
    bank2 <- bank[complete.cases(bank), ]
    # Should not error but throw a warning when complete cases are used (dummy variable adjustment redundant)
    expect_warning(Regression(Overall ~ Fees + Interest, data = bank2,
                              output = "Relative Importance Analysis",
                              missing = "Dummy variable adjustment"),
                   "The following variables have been treated as categorical: Interest", fixed = TRUE)
    # Should not throw error if factor used but not dummy adjusted.
    bank2 <- bank
    bank2 <- bank2[!is.na(bank2$Interest), ]
    expect_warning(Regression(Overall ~ Fees + Interest, data = bank2,
                              output = "Relative Importance Analysis",
                              missing = "Dummy variable adjustment"),
                   "The following variables have been treated as categorical: Interest", fixed = TRUE)
})

test_that("DS-2990: Check aliased predictors removed before being passed to RIA/Shapley", {
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
    dat$cat1 <- factor(rep(c(1, 2, 3, 4), c(10, 10, 20, 60)), labels = LETTERS[1:4])
    dat$cat2 <- factor(rep(c(1, 2, 3, 4, 5), c(10, 10, 20, 30, 30)), labels = LETTERS[1:5])
    fancy.label.dat <- dat
    for (i in seq_along(dat))
        attr(fancy.label.dat[[i]], "label") <- paste0("Fancy label of ", names(dat)[i])
    expect_error(ria <- Regression(Y ~ X.1 + X.2 + X.3, data = dat, output = "Relative Importance Analysis"), NA)
    expect_warning(print(ria), "^Unusual observations detected")
    # Only the aliased numeric predictors and aliased levels of the categorical predictor are shown
    expect_warning(ria <- Regression(Y ~ ., data = dat, output = "Relative Importance Analysis"),
                   paste0("The following variable(s) are colinear with other variables and no ",
                          "coefficients have been estimated: 'X.4', 'cat2B', 'cat2C', 'cat2E'"),
                   fixed = TRUE)
    # Aliased variables are removed from Importance Analysis
    expect_equal(names(ria$importance$raw.importance),
                 c("X.1", "X.2", "X.3", "cat1B", "cat1C", "cat1D"))
    expect_error(print(ria), NA)
    # Same for Shapley Regression
    expect_warning(ria <- Regression(Y ~ ., data = dat, output = "Shapley Regression"),
                   paste0("The following variable(s) are colinear with other variables and no ",
                          "coefficients have been estimated: 'X.4', 'cat2B', 'cat2C', 'cat2E'"),
                   fixed = TRUE)
    # Aliased variables are removed from Importance Analysis
    expect_equal(names(ria$importance$raw.importance),
                 c("X.1", "X.2", "X.3", "cat1B", "cat1C", "cat1D"))
    expect_error(print(ria), NA)
    expect_warning(ria <- Regression(Y ~ ., data = fancy.label.dat, output = "Relative Importance Analysis", show.labels = TRUE),
                   paste0("The following variable(s) are colinear with other variables and no ",
                          "coefficients have been estimated: 'Fancy label of X.4', 'Fancy label of cat2: B', ",
                          "'Fancy label of cat2: C', 'Fancy label of cat2: E'"),
                   fixed = TRUE)
    expect_equal(names(ria$importance$raw.importance),
                 c("X.1", "X.2", "X.3", "cat1: B", "cat1: C", "cat1: D"))
    # Entire numeric predictor and entire categorical predictor removed for the RIA
    expect_error(print(ria), NA)
    ordered.logit.dat <- dat
    y.to.ord <- factor(cut(dat$Y, breaks = c(-Inf, quantile(dat$Y, prob = c(0.25, 0.5, 0.75)), Inf),
                           labels = LETTERS[1:4]), ordered = TRUE)
    ordered.logit.dat$Y <- y.to.ord
    expect_error(ria <- Regression(Y ~ X.1 + X.2 + X.3, data = ordered.logit.dat,
                                   output = "Relative Importance Analysis", type = "Ordered Logit"),
                 NA)
    expect_error(print(ria), NA)
    expect_warning(ria <- Regression(Y ~ ., data = ordered.logit.dat,
                                     output = "Relative Importance Analysis", type = "Ordered Logit"),
                   "^Some variable\\(s\\) are colinear")
    expect_equal(names(ria$importance$raw.importance),
                 c("X.1", "X.2", "X.3", "cat1B", "cat1C", "cat1D"))
    expect_error(print(ria), NA)
    # Test Dummy variable in Ordered Logit
    X.1.miss <- dat$X.1
    X.1.miss[sample(c(TRUE, FALSE), size = nrow(dat), prob = c(1, 10), replace = TRUE)] <- NA
    dat$X.1.miss <- X.1.miss
    ordered.logit.dat$X.1.miss <- X.1.miss
    expect_warning(ria <- Regression(Y ~ X.1.miss + X.2 + X.3 + X.4 + cat1 + cat2, data = ordered.logit.dat,
                                     output = "Relative Importance Analysis", type = "Ordered Logit",
                                     missing = "Dummy variable adjustment"),
                   "^Some variable\\(s\\) are colinear")
    # Aliased removed from importance analysis
    expect_equal(names(ria$importance$raw.importance),
                 c("X.1.miss", "X.2", "X.3", "cat1B", "cat1C", "cat1D"))
    expect_error(print(ria), NA)
    # Check crosstab interaction
    groups <- factor(rep(1:3, c(15, 15, 70)), labels = paste0("Group ", 1:3))
    dat$groups <- groups
    expect_warning(ria <- Regression(Y ~ X.1 + X.2 + X.3 + X.4 + cat1 + cat2, data = dat,
                                     output = "Relative Importance Analysis", interaction = dat$groups),
                   paste0("The following variable(s) are colinear with other variables and no ",
                          "coefficients have been estimated: 'X.4', 'cat2B', 'cat2C', 'cat2E'"),
                   fixed = TRUE)
    expect_equal(names(ria$importance$raw.importance),
                 c("X.1", "X.2", "X.3", "cat1B", "cat1C", "cat1D"))
    expect_error(print(ria), NA)
    # Checked ordered logit in this case
    expect_warning(ria <- Regression(Y ~ X.1 + X.2 + X.3 + X.4 + cat1 + cat2, data = ordered.logit.dat,
                                     output = "Relative Importance Analysis", interaction = dat$groups,
                                     type = "Ordered Logit"),
                   "^Some variable\\(s\\) are colinear with other variables")
    expect_equal(names(ria$importance$raw.importance),
                 c("X.1", "X.2", "X.3", "cat1B", "cat1C", "cat1D"))
    expect_error(print(ria), NA)
})
