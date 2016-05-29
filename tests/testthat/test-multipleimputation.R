#
# data(bank, package = "flipExampleData")
#
# # Single imputation.
# method = "Imputation (replace missing values with estimates)" #"Multiple imputation"}.
#
# Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, type = "NBD", method = method)
#
# # Multiple imputation.
#
# library(mice)
# tempData2 <- mice(bank, m = 10, seed = 23)
# modelFit2 <- with(tempData2,lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM))
# modelFit3 <- with(tempData2,Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, type = "NBD"))
# summary(pool(modelFit2))
# summary(pool(modelFit3))
#
# csummary(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank))
#
# #
# # # Regression with partial correlations.
# # data(bank)
# # missing <- "Use partial data (pairwise correlations)"
# # detail = TRUE
# # type = "Linear"
# # bank$sb <- bank$weight > 1 & !is.na(bank$weight)
# #
# # # Unfiltered
# # summary(LinearRegressionFromCorrelations(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank))
# #
# # # Filtered
# # summary(LinearRegressionFromCorrelations(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$sb, weights = rep(1, nrow(bank))))
# #
# # # Weighted
# # bank$weight0 <- bank$weight
# # bank$weight0[is.na(bank$weight)] <- 0
# # z = LinearRegressionFromCorrelations(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$weight0)
# # summary(z)
# #
# # # Weighted and filtered
# # z = LinearRegressionFromCorrelations(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$sb,  weights = bank$weight0)
# # summary(z)
# #
# #
# # X <- bank[, c("Fees","Interest","Phone","Branch","Online","ATM")]
# # Y <- bank[, c("Overall")]
# # cors <- cor(XandY <- cbind(X, Y), use = "pairwise.complete.obs")
# # n.rows <- nrow(XandY)
# # ns <- n.rows - apply(XandY, 2, function(x) sum(is.na(x)))
# #
# #
# #
# # .sd(X, w = bank$weight)
# #
# #
# # r <- flipU::CovarianceAndCorrelationMatrix(bank[,c("Overall","Fees","Interest","Phone","Branch","Online","ATM")], bank$weight0, TRUE, TRUE)
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# # psych::setCor(7, 1:6, cors, n.obs = 340)
# # z <- flipMultivariates:::betasFromCorrelations(cors)
# #
# #
# #
# # regressionFromCorrelations(Y[complete.cases(bank)], X[complete.cases(bank),])
# #
# # ns <- crossprod(!is.na(XandY))
# # ns <- ns[lower.tri(ns)]
# # 1/mean(1/ns)
# # 1/mean(1/ns)
# #
# #
# #
# # summary(lm(Overall~Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = complete.cases(bank)))
# #
# # library(boot)
# # z = lm(Overall~Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = complete.cases(bank))
# # Boot(z, f = coef, R = 9999, method = "case")
# # b.se <- regressionFromCorrelations(Y[complete.cases(bank)], X[complete.cases(bank),], b = 1999)
# # unstandardizeBetas(b, X[complete.cases(bank),], Y[complete.cases(bank)])
# #
# #
# #
# # XandY <- cbind(X, Y)
# # R <- cor(XandY, use = "pairwise.complete.obs")
# # swept <- swp(R, 1:6)
# # b <- swept[7, 1:6]
# # residual.variance <- swept[7, 7]^2
# # betasFromCorrelations(R)
# #
# # regessionFromCorrelations(R)
# #
# #
# #
# #
# #
# #
# #
# # sum(!is.na(Y))
# #
# #
# #
# #
# #
# #
# # n.rows - apply(XandY, 2, function(x) sum(is.na(x)))
# #
# #
# #
# #
# # residual.se <- sqrt(residual.variance / 340)
# # residual.se * r
# #
# # total.variance <- var(Y)
# # R2 <-
# #     unstandardizeBetas(b, X, Y)
# #
# #
# #
# # set
# # lm(Y~X)
# #
# #
# # set.seed(4)
# # n <- 100
# # X <- matrix(runif(n * 5), ncol = 5)
# # Beta <- 1:ncol(X)
# # Y <- X %*% Beta + rnorm(n)
# # R <- cor(cbind(X, Y))
# # unstandardizeBetas(dempsterSweep(R, 1:5)[6, 1:5], X, Y)
# # lm(Y~X)
# #
# #
# #
# #
# #
# #
# #
# # class(zz) <- "summary.lm"
# # print.summary.lm(zz)
# # flipMultivariates:::print.RegressionCorrelationsSummary(zz)
# #
# # zLS = lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)
# #
# # zSummaryLS <- summary(zLS)
# #
# # bank$s <- unclass(bank$Overall) / sd(unclass(bank$Overall), na.rm = TRUE)
# # summary(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank))$sigma
# #
# # summary(lm(s ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank))$sigma * (sd(unclass(bank$Overall), na.rm = TRUE))
# #
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing, type = type, detail = detail)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing, type = type, detail = detail)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, missing = missing, type = type, detail = detail)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = sb, missing = missing, type = type, detail = detail)
# #
# #
# # # binary logit.
# # data(bank)
# # missing <- "Use partial data (pairwise correlations)"
# #
# # missing <- "Imputation (replace missing values with estimates)"
# # missing = "Error if missing data"
# # type = "Ordered Logit"
# # wgt <- bank$ID
# # attr(wgt, "label") <- "Bank ID"
# # sb <- bank$ID > 100
# # attr(sb, "label") <- "Bank ID greater than 100"
# # type = "Linear"
# # detail = FALSE
# # z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing, type = type, detail = detail)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing, type = type, detail = detail)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, missing = missing, type = type, detail = detail)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = sb, missing = missing, type = type, detail = detail)
# #
# #
# # z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = "Use partial data (pairwise correlations)")
# #
# # type = "Ordered"
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing, type = type)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing, type = type)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, missing = missing, type = type)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = sb, missing = missing, type = type)
# #
# # type = "NBD"
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing, type = type)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing, type = type)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, missing = missing, type = type)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = sb, missing = missing, type = type)
# #
# #
# #
# # # type type
# # data(bank)
# # Regression(Overall ~ Fees, data = bank, missing = "Imputation (replace missing values with estimates)")
# #
# #
# # zdata <- bank#data.frame(Overall = Overall, Fees = Fees)
# # Regression(Overall ~ Fees, zdata)
# #
# # Regression(log(Overall) ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)
# #
# # type = "Ordered Logit"
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, robust.se = TRUE)
# #
# #
# # missing <- "Exclude cases with missing data"
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, missing = missing)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = sb, missing = missing)
# #
# # missing <- "Use partial data (pairwise correlations)"
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, missing = missing)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = sb, missing = missing)
# #
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = TRUE, weights = NULL, missing = missing)
# #
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing)
# # zbank <- subset(bank, wgt > 100 & !is.na(wgt))
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = zbank, missing = missing)
# # summary(zbank)
# #
# #
# #
# # print(dim(zbank))
# # cv <- cov(zbank, use = "pairwise.complete.obs")
# # psych::setCor(2, 3:8, zbank, std = FALSE)
# #
# # psych::setCor(2, 3:8, cv, std = FALSE, square = TRUE)
# #
# #
# # psych::setCor(2, 3:8, zbank, std = TRUE)$beta
# #
# #
# # psych::setCor(2, 3:8, cv, std = TRUE, square = TRUE)$beta
# #
# #
# # psych::setCor(2, 3:8, zbank, std = FALSE)
# #
# #
# #
# # .pairwise.regression <- psych::setCor
# # n <- length(body(.pairwise.regression))
# # while(as.character(body(.pairwise.regression)[n]) != "setCor.diagram(set.cor, main = main)")
# #     n <- n - 1
# # body(.pairwise.regression)[n] <- NULL
# # .pairwise.regression(2, 3:8, zbank)
# #
# # print(sum(bank,na.rm = TRUE))
# #
# # zzbank = AdjustDataToReflectWeights(zbank, weights)
# # psych::setCor(2, 3:8, zzbank)
# #
# # zzzbank = bank[complete.cases(bank), ]
# # psych::setCor(2, 3:8, zzzbank)$beta
# #
# #
# #
# # psych::setCor(2, 3:8, zzzbank, STD = TRUE)$beta
# #
# # unscaled.data <- zzzbank
# # scaled.data <- as.data.frame(scale(zzzbank))
# # sds.independent = apply(unscaled.data[,3:8], 2, sd, na.rm = TRUE)# / sd(zzzbank[,2], na.rm = TRUE)
# # sds.independent
# # sd.dependent <- sd(unscaled.data[,2], na.rm = TRUE)
# # sd.dependent
# #
# #
# #
# # lm.unscaled <-summary(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = unscaled.data))$coef[-1,1]
# # lm.unscaled
# # lm.scaled <-summary(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = scaled.data))$coef[-1,1]
# # lm.scaled
# #
# # lm.unscaled * (sds.independent / sd.dependent)
# #
# # lm.unscaled
# # lm.unscaled / (sds.independent / sd.dependent)
# #
# # lm.scaled / (sds.independent / sd.dependent)
# # lm.unscaled
# #
# # psych::setCor(2, 3:8, zzzbank, std = FALSE)$beta
# # psych::setCor(2, 3:8, zzzbank, std = TRUE)$beta / (sds.independent / sd.dependent)
# #
# #
# #
# # summary(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = as.data.frame(scale(zzzbank))))$coef
# # psych::setCor(2, 3:8, zzzbank, std = TRUE)$beta
# #
# # covm <- cov(bank)
# #
# #
# # psych::setCor(2, 3:8, bank, std = TRUE)$beta
# #
# #
# # bank.filtered <- subset(bank, wgt > 100)
# # library(psych)
# # psych::setCor(2, 3:8, bank.filtered)$beta
# #
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing)
# #
# # #
# # # ), predictors.index,
# # #         data = estimation.data, std = FALSE)
# # #     partial.coefs <- cbind(lm.cov$beta, lm.cov$se, lm.cov$t, lm.cov$Probability)
# # #     dimnames(partial.coefs) <- list(variable.names[predictors.index],
# # #         c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
# # #     beta <- as.matrix(lm.cov$beta)
# # #     fitted <- as.matrix(estimation.data[, predictors.index]) %*% beta
# # #     intercept <- mean(estimation.data[, outcome.name], na.rm = TRUE) - mean(fitted, na.rm = TRUE)
# # #     fitted <- as.matrix(data[, predictors.index]) %*% beta
# # #     result$flip.fitted.values <- fitted + intercept
# #
# # missing <- "Imputation (replace missing values with estimates)"
# # type = "Ordered"
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing, type = type)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing, type = type)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, missing = missing, type = type)
# # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = sb, missing = missing, type = type)
# #
# # library(MASS)
# # z = polr(factor(Overall) ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank,weights = wgt, subset = sb)
# # dim(fitted.values(z))
