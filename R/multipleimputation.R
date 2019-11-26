#' \code{MultipleImputationCoefficientTable} Creates the table of coefficients from multiple imputation.
#' @param models A list of models created with multiply-imputed data sets.
#' @param large.sample.df Whether to estimate the degrees of freedom for the t-distribution using a
#' large sample or sample sample approach.
#' @references
#' J Barnard and DB Rubin (1999) 'Small-sample degrees of freedom with multiple imputation' Biometrika (1999) 86 (4): 948-955.
#' DB Rubin (1987) 'Multiple Imputation for Nonresponse in Surveys', John Wiley & Sons.
#' @importFrom stats df df.residual pt var coef vcov
#' @export
MultipleImputationCoefficientTable <- function(models, large.sample.df = FALSE)
{
    m <- length(models)
    .coef <- function(object)
    {
        object$coef
    }
    coefs <- sapply(models, .coef)
    vars <- sapply(models, FUN = function(x) diag(vcov(x)))
    coef.mean <- apply(coefs, 1, mean, na.rm = FALSE)
    ses <- multipleImputationStandardErrors(coefs, vars)
    df.c <- df.residual(models[[1]])
    dfs <- multipleImputationDegreesOfFreedom(coefs, vars, df.c, large.sample.df)
    tvals <- coef.mean / ses
    correct <- models[[1]]$correction
    pvals <-  pvalAdjust(2 * pt(abs(tvals), dfs, lower.tail = FALSE), correct)
    results <- cbind(coef.mean, ses, tvals, dfs, pvals)
    row.rm <- which(apply(results, 1, function(x){all(is.na(x))}))
    if (length(row.rm) > 0)
        results <- results[-row.rm,]
    coef.names <- rownames(models[[1]]$summary$coef)
    if(models[[1]]$type == "Multinomial Logit")
    {
        coef.names <- models[[1]]$original$coefnames
        levs <- models[[1]]$original$lev[-1]
        n.levs <- length(levs)
        k <- length(pvals)
        n.coef.names <- length(coef.names)
        coef.names <- rep(coef.names, rep(n.levs, n.coef.names))
        #levs <- levs[rep(1:n.levs, rep(k / n.levs, n.levs))]
        coef.names <- paste(coef.names, levs)
    }
    dimnames(results) <- list(coef.names,
        c("Estimate", "Std. Error", "t value", "Degrees of Freedom", "Pr(>|t|)"))
    results
}


multipleImputationDegreesOfFreedom <- function(coefs, vars, df.complete, large.sample.df = FALSE)
{
    #  https://www.stata.com/manuals13/mi.pdf    Page 71 of the pDF  Rubin 1987
    m <- ncol(coefs)
    B <- apply(coefs, 1, var)
    W <- apply(vars, 1, FUN = mean, na.rm = FALSE)
    r <- (1 + 1 / m) * B / W
    df.large <- (m - 1) * (1 + 1 / r) ^ 2
    if (large.sample.df)
        return(df.large)
    if (missing(df.complete))
        stop("'df.complete': Degrees of freedom of the complete model.")
    T.denom <- W + (1 + 1 / m) * B
    lambda <- (1 + 1 / m) * B / T.denom
    df.obs <- df.complete * (df.complete + 1) * ( 1 - lambda) / (df.complete + 3)
    df.small <- (1 / df.large + 1 / df.obs) ^ -1
    df.small
}

multipleImputationStandardErrors <- function(coefs, vars)
{
    B <- apply(coefs, 1, var)
    W <- apply(vars, 1, FUN = mean, na.rm = FALSE)
    T <- W + (1 + 1 / ncol(coefs)) * B
    sqrt(T)
}

multipleImputationImportance <- function(models, importance.absolute)
{
    coefs <- sapply(models, function(object) object$coef)
    tmp.coefs <- unname(apply(coefs, 1, mean, na.rm = FALSE))
    signs <- if (models[[1]]$type == "Ordered Logit") sign(tmp.coefs[1:(models[[1]]$n.predictors)])
             else sign(tmp.coefs[-1])
    if (importance.absolute)
        signs <- 1

    result <- list()
    correct <- models[[1]]$correction
    models.raw.importance <- sapply(models, function(m) m$importance$raw.importance)
    result$raw.importance <- apply(models.raw.importance, 1, mean, na.rm = FALSE)
    result$importance <- signs * 100 * prop.table(result$raw.importance)
    vars <- sapply(models, function(m) m$importance$standard.errors ^ 2)
    result$standard.errors <- multipleImputationStandardErrors(models.raw.importance, vars)
    df.c <- df.residual(models[[1]])
    result$df <- multipleImputationDegreesOfFreedom(models.raw.importance, vars, df.c, FALSE)
    result$statistics <- signs * result$raw.importance / result$standard.errors
    result$p.values.raw <-  2 * pt(abs(result$statistics), result$df, lower.tail = FALSE)
    result$p.values <- pvalAdjust(result$p.values.raw, correct)
    result$statistic.name <- "t"
    result
}

#' @importFrom stats pchisq
multipleImputationCrosstabInteraction <- function(models, importance)
{
    n <- nrow(models[[1]]$interaction$bb)
    m <- ncol(models[[1]]$interaction$bb)
    split.size <- models[[1]]$interaction$split.size
    correction <- models[[1]]$correction
    res <- list(label = models[[1]]$interaction$label,
                split.size = split.size, pvalue = NA,
                importance = importance,
                original.r2 = mean(sapply(models, function(m){m$interaction$original.r2})),
                full.r2 = mean(sapply(models, function(m){m$interaction$full.r2})))

    bb.all <- sapply(models, function(m){m$interaction$bb})
    bc.all <- sapply(models, function(m){m$interaction$bc})
    ss.all <- sapply(models, function(m){m$interaction$ss^2})
    sc.all <- sapply(models, function(m){m$interaction$sc^2})
    bb <- apply(bb.all, 1, mean, na.rm=T)
    bc <- apply(bc.all, 1, mean, na.rm=T)
    ss <- multipleImputationStandardErrors(bb.all, ss.all)
    sc <- multipleImputationStandardErrors(bc.all, sc.all)
    coef.sign <- compareCoef(matrix(bb, nrow=n), matrix(bc, nrow=n),
                                 matrix(ss, nrow=n), matrix(sc, nrow=n),
                                 split.size[1:m], correction, importance)
    res$coef.pvalues <- coef.sign$pvalues
    res$coef.tstat <- coef.sign$tstat
    if (!is.null(importance))
        res$coef.pFDR <- coef.sign$pFDR

    net.coef.all <- sapply(models, function(m){m$interaction$net.coef})
    net.coef <- apply(net.coef.all, 1, mean)
    bb <- matrix(bb, nrow=n)

    # Report normalised relative importance/Shapley scores but use raw scores for p-values
    if (!is.null(importance))
        bb <- apply(bb, 2, function(x){x/sum(abs(x))*100})

    combined.coefs <- cbind(bb, net.coef)
    colnames(combined.coefs) <- names(split.size)
    res$coefficients <- combined.coefs


    if (!is.null(importance))
        return(res)

    if (models[[1]]$type %in% c("Linear", "Quasi-Poisson"))
    {
        fstat <- mean(sapply(models, function(m){m$interaction$anova.fstat}))
        df1 <- models[[1]]$interaction$anova.df1
        df2 <- models[[1]]$interaction$anova.df2
        res$anova.test <- "F-test"
        res$pvalue <-  pf(fstat, df1, df2, lower.tail=F)
    } else
    {
        cstat <- mean(sapply(models, function(m){m$interaction$anova.dev}))
        df1 <- models[[1]]$interaction$anova.df1
        res$anova.test <- "Chi-square test"
        res$pvalue <- pchisq(cstat, df1, lower.tail=F)
    }
    return(res)
}


# # #
# # # # Alternative formula (http://www.ncbi.nlm.nih.gov/pmc/articles/PMC4029775/)
# # # df.hash = df.large
# # # df.star = df.small
# # #
# # # # mi  package
# # # gamma <- (1 + 1/m) * B/T
# # # df.r <- df.c # pooled_summary$df.residual
# # # v <- df.large# (m - 1) * (1 + m/(m + 1) * W/B)^2
# # # v_obs <- df.obs#(1 - gamma) * (df.r + 1)/(df.r + 3) *  df.r
# # # df.star <- 1/(1/v + 1/v_obs)
# # #
# # # t.stats <- b.mean / sqrt(T)
# # # pt(b.mean / sqrt(T), df.small)
# # # # mice package
# # #
# # # n.groups <- 10
# # # m <- 4
# # # n <- n.groups * m
# # #
# # # dat <- data.frame(y = y <- rnorm(n, 3.3, 1), grp = rep(1:n.groups, rep(m, n.groups)))
# # #
# # # # Computing test by hand.
# # # .ss <- function(x) var(x) * (length(x) - 1)
# # # sst <- .ss(dat$y)
# # # sse <- sum(aggregate(y~id, data = dat,FUN = .ss)[,2])
# # # ssw <- sst - ssw
# # # icc <- m / (m - 1) * ssw / sst
# # # vif <- 1 + (m - 1) * icc
# # #
# # #
# # # se <- sqrt((sd(dat$y)/sqrt(n))^2 * vif)
# # # #SSW <- sum((dat$y - aggregate(y~id, data = dat,FUN = mean)[dat$grp,2])^2)
# # #
# # #
# # # t.test(y, mu = 3)
# # #
# # # mu <- 3
# # # y.less.mu <- y - mu
# # # t.test(y.less.mu, mu = 0)
# # #
# # # # Design based.
# # #
# # # library(survey)
# # # sdesign <- svydesign(id = ~grp, data = dat)
# # # svymean(~ y, design = sdesign)
# # # svyttest(I(y - 3) ~ 0, design = sdesign)
# # #
# # # # Model-based.
# # # summary(lme4::lmer(y.less.mu ~ 1 | grp))
# # #
# # # aov(y ~ factor(grp), data = dat)
# # #
# # # mice.df <- function (m, lambda, dfcom, method)
# # # {
# # #     if (is.null(dfcom)) {
# # #         dfcom <- 999999
# # #         warning("Large sample assumed.")
# # #     }
# # #     lambda[lambda < 1e-04] <- 1e-04
# # #     dfold <- (m - 1)/lambda^2
# # #     dfobs <- (dfcom + 1)/(dfcom + 3) * dfcom * (1 - lambda)
# # #     df <- dfold * dfobs/(dfold + dfobs)
# # #     if (method != "smallsample")
# # #         df <- dfold
# # #     return(df)
# # # }
# # #
# # # tbpool <- function(object, method = "smallsample")
# # # {
# # #     call <- match.call()
# # #     if (!is.mira(object))
# # #         stop("The object must have class 'mira'")
# # #     m <- length(object$analyses)
# # #     fa <- getfit(object, 1)
# # #     if (m == 1) {
# # #         warning("Number of multiple imputations m=1. No pooling done.")
# # #         return(fa)
# # #     }
# # #     analyses <- getfit(object)
# # #     if (class(fa)[1] == "lme" & !requireNamespace("nlme", quietly = TRUE))
# # #         stop("Package 'nlme' needed fo this function to work. Please install it.",
# # #              call. = FALSE)
# # #     if ((class(fa)[1] == "mer" | class(fa)[1] == "lmerMod") &
# # #         !requireNamespace("lme4", quietly = TRUE))
# # #         stop("Package 'lme4' needed fo this function to work. Please install it.",
# # #              call. = FALSE)
# # #     mess <- try(coef(fa), silent = TRUE)
# # #     if (inherits(mess, "try-error"))
# # #         stop("Object has no coef() method.")
# # #     mess <- try(vcov(fa), silent = TRUE)
# # #     if (inherits(mess, "try-error"))
# # #         stop("Object has no vcov() method.")
# # #     if (class(fa)[1] == "mer" | class(fa)[1] == "lmerMod") {
# # #         k <- length(lme4::fixef(fa))
# # #         names <- names(lme4::fixef(fa))
# # #     }
# # #     else if (class(fa)[1] == "polr") {
# # #         k <- length(coef(fa)) + length(fa$zeta)
# # #         names <- c(names(coef(fa)), names(fa$zeta))
# # #     }
# # #     else {
# # #         k <- length(coef(fa))
# # #         names <- names(coef(fa))
# # #     }
# # #     qhat <- matrix(NA, nrow = m, ncol = k, dimnames = list(1:m,
# # #                                                            names))
# # #     u <- array(NA, dim = c(m, k, k), dimnames = list(1:m, names,
# # #                                                      names))
# # #     for (i in 1:m) {
# # #         fit <- analyses[[i]]
# # #         if (class(fit)[1] == "mer") {
# # #             qhat[i, ] <- lme4::fixef(fit)
# # #             ui <- as.matrix(vcov(fit))
# # #             if (ncol(ui) != ncol(qhat))
# # #                 stop("Different number of parameters: class mer, fixef(fit): ",
# # #                      ncol(qhat), ", as.matrix(vcov(fit)): ", ncol(ui))
# # #             u[i, , ] <- array(ui, dim = c(1, dim(ui)))
# # #         }
# # #         else if (class(fit)[1] == "lmerMod") {
# # #             qhat[i, ] <- lme4::fixef(fit)
# # #             ui <- vcov(fit)
# # #             if (ncol(ui) != ncol(qhat))
# # #                 stop("Different number of parameters: class lmerMod, fixed(fit): ",
# # #                      ncol(qhat), ", vcov(fit): ", ncol(ui))
# # #             u[i, , ] <- array(ui, dim = c(1, dim(ui)))
# # #         }
# # #         else if (class(fit)[1] == "lme") {
# # #             qhat[i, ] <- fit$coefficients$fixed
# # #             ui <- vcov(fit)
# # #             if (ncol(ui) != ncol(qhat))
# # #                 stop("Different number of parameters: class lme, fit$coefficients$fixef: ",
# # #                      ncol(qhat), ", vcov(fit): ", ncol(ui))
# # #             u[i, , ] <- array(ui, dim = c(1, dim(ui)))
# # #         }
# # #         else if (class(fit)[1] == "polr") {
# # #             qhat[i, ] <- c(coef(fit), fit$zeta)
# # #             ui <- vcov(fit)
# # #             if (ncol(ui) != ncol(qhat))
# # #                 stop("Different number of parameters: class polr, c(coef(fit, fit$zeta): ",
# # #                      ncol(qhat), ", vcov(fit): ", ncol(ui))
# # #             u[i, , ] <- array(ui, dim = c(1, dim(ui)))
# # #         }
# # #         else if (class(fit)[1] == "survreg") {
# # #             qhat[i, ] <- coef(fit)
# # #             ui <- vcov(fit)
# # #             parnames <- dimnames(ui)[[1]]
# # #             select <- !(parnames %in% "Log(scale)")
# # #             ui <- ui[select, select]
# # #             if (ncol(ui) != ncol(qhat))
# # #                 stop("Different number of parameters: class survreg, coef(fit): ",
# # #                      ncol(qhat), ", vcov(fit): ", ncol(ui))
# # #             u[i, , ] <- array(ui, dim = c(1, dim(ui)))
# # #         }
# # #         else {
# # #             qhat[i, ] <- coef(fit)
# # #             ui <- vcov(fit)
# # #             ui <- mice:::expandvcov(qhat[i, ], ui)
# # #             if (ncol(ui) != ncol(qhat))
# # #                 stop("Different number of parameters: coef(fit): ",
# # #                      ncol(qhat), ", vcov(fit): ", ncol(ui))
# # #             u[i, , ] <- array(ui, dim = c(1, dim(ui)))
# # #         }
# # #     }
# # #     qbar <- apply(qhat, 2, mean)
# # #     print(c("qbar", qbar))
# # #     ubar <- apply(u, c(2, 3), mean)
# # #     #print(c("ubar", ubar))
# # #     e <- qhat - matrix(qbar, nrow = m, ncol = k, byrow = TRUE)
# # #     b <- (t(e) %*% e)/(m - 1)
# # #     #print(c("b", b))
# # #     t <- ubar + (1 + 1/m) * b
# # #     print("t")
# # #     print(t)
# # #     r <- (1 + 1/m) * diag(b/ubar)
# # #     print(c("r", diag(r)))
# # #     lambda <- (1 + 1/m) * diag(b/t)
# # #     print(c("lambda", lambda))
# # #     dfcom <- df.residual(object)
# # #     dfcom <- 755
# # #     print(c("dfcom", dfcom))
# # #     df <- mice.df(m, lambda, dfcom, method)
# # #     print(c("df", df))
# # #     fmi <- (r + 2/(df + 3))/(r + 1)
# # #     names(r) <- names(df) <- names(fmi) <- names(lambda) <- names
# # #     fit <- list(call = call, call1 = object$call, call2 = object$call1,
# # #                 nmis = object$nmis, m = m, qhat = qhat, u = u, qbar = qbar,
# # #                 ubar = ubar, b = b, t = t, r = r, dfcom = dfcom, df = df,
# # #                 fmi = fmi, lambda = lambda)
# # #     oldClass(fit) <- c("mipo", oldClass(object))
# # #     return(fit)
# # # }
# # #
# # #
# # #
# # #
# # # tempData2 <- mice(bank, m = 10, seed = 1223)
# # # modelFit2 <- with(tempData2,lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM))
# # # #modelFit2 <- with(tempData2,Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM))
# # # summary(tbpool(modelFit2))
# # #
# # #
# # #
# # #
# # # imp <- mice(nhanes)
# # # fit <- with(data=imp,exp=lm(bmi~hyp+chl))
# # # summary(mice::pool(fit))
# # #
# # #
# # # imp <- mice(bank, m = 100, seed = 123)
# # # fit <- with(data=imp,exp=lm(Overall~Fees + Interest + Phone + Branch + Online + ATM))
# # # summary(mice::pool(fit))[,4]
# # #
# # #
# # # summary(tbpool(fit))
# # #
# # #
# # # imputations = mi::mi(bank)
# # # analysis <- pool(Overall~Fees + Interest + Phone + Branch + Online + ATM,
# # #                  data = imputations, m = 100)
# # # summary(analysis)
# # #
# # #
# # #
# # #
# # # df.hash <- (m - 1) * (1 + W / ((1 + 1 / m) * B)) ^ 2
# # # lambda <- r / sqrt(r ^ 2 + 1)
# # #
# # #
# # #     (1 - (1 + 1 / m) * B / T) * df.com.star
# # #
# # #
# # # v.0 <- (m - 1) * (1 + 1 / r ^ 2)
# # # v.0
# # #
# # #
# # #
# # # library(mi)
# # # if(!exists("imputations", env = .GlobalEnv)) {
# # #     imputations <- mi:::imputations # cached from example("mi-package")
# # # }
# # # analysis <- pool(ppvtr.36 ~ first + b.marr + income + momage + momed + momrace,
# # #                  data = imputations)
# # # display(analysis)
# # #
# # #
# # #
# # # tbpool
# # #
# # # tempData2 <- mice(bank, m = 10, seed = 1223)
# # # modelFit2 <- with(tempData2,lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM))
# # # #modelFit2 <- with(tempData2,Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM))
# # # summary(mice::pool(modelFit2))
# # #
# # # summary(tbpool(modelFit2))
# # #
# # # library(mi)
# # # imp.mi <- mi::mi(bank)
# # # lm.mi.out <- lm.mi(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, imp.mi)
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # # summary(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank))
# # #
# # #
# # # # Regression with partial correlations.
# # # data(bank)
# # # missing <- "Use partial data (pairwise correlations)"
# # # detail = TRUE
# # # type = "Linear"
# # # bank$sb <- bank$weight > 1 & !is.na(bank$weight)
# # #
# # # # Unfiltered
# # # summary(LinearRegressionFromCorrelations(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank))
# # #
# # # # Filtered
# # # summary(LinearRegressionFromCorrelations(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$sb, weights = rep(1, nrow(bank))))
# # #
# # # # Weighted
# # # bank$weight0 <- bank$weight
# # # bank$weight0[is.na(bank$weight)] <- 0
# # # z = LinearRegressionFromCorrelations(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$weight0)
# # # summary(z)
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # # # Weighted and filtered
# # # z = LinearRegressionFromCorrelations(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$sb,  weights = bank$weight0)
# # # summary(z)
# # #
# # #
# # # X <- bank[, c("Fees","Interest","Phone","Branch","Online","ATM")]
# # # Y <- bank[, c("Overall")]
# # # cors <- cor(XandY <- cbind(X, Y), use = "pairwise.complete.obs")
# # # n.rows <- nrow(XandY)
# # # ns <- n.rows - apply(XandY, 2, function(x) sum(is.na(x)))
# # #
# # #
# # #
# # # .sd(X, w = bank$weight)
# # #
# # #
# # # r <- flipU::CovarianceAndCorrelationMatrix(bank[,c("Overall","Fees","Interest","Phone","Branch","Online","ATM")], bank$weight0, TRUE, TRUE)
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # # psych::setCor(7, 1:6, cors, n.obs = 340)
# # # z <- flipMultivariates:::betasFromCorrelations(cors)
# # #
# # #
# # #
# # # regressionFromCorrelations(Y[complete.cases(bank)], X[complete.cases(bank),])
# # #
# # # ns <- crossprod(!is.na(XandY))
# # # ns <- ns[lower.tri(ns)]
# # # 1/mean(1/ns)
# # # 1/mean(1/ns)
# # #
# # #
# # #
# # # summary(lm(Overall~Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = complete.cases(bank)))
# # #
# # # library(boot)
# # # z = lm(Overall~Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = complete.cases(bank))
# # # Boot(z, f = coef, R = 9999, method = "case")
# # # b.se <- regressionFromCorrelations(Y[complete.cases(bank)], X[complete.cases(bank),], b = 1999)
# # # unstandardizeBetas(b, X[complete.cases(bank),], Y[complete.cases(bank)])
# # #
# # #
# # #
# # # XandY <- cbind(X, Y)
# # # R <- cor(XandY, use = "pairwise.complete.obs")
# # # swept <- swp(R, 1:6)
# # # b <- swept[7, 1:6]
# # # residual.variance <- swept[7, 7]^2
# # # betasFromCorrelations(R)
# # #
# # # regessionFromCorrelations(R)
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # # sum(!is.na(Y))
# # #
# # #
# # #
# # #
# # #
# # #
# # #                 n.rows - apply(XandY, 2, function(x) sum(is.na(x)))
# # #
# # #
# # #
# # #
# # # residual.se <- sqrt(residual.variance / 340)
# # # residual.se * r
# # #
# # # total.variance <- var(Y)
# # # R2 <-
# # # unstandardizeBetas(b, X, Y)
# # #
# # #
# # #
# # # set
# # # lm(Y~X)
# # #
# # #
# # # set.seed(4)
# # # n <- 100
# # # X <- matrix(runif(n * 5), ncol = 5)
# # # Beta <- 1:ncol(X)
# # # Y <- X %*% Beta + rnorm(n)
# # # R <- cor(cbind(X, Y))
# # # unstandardizeBetas(dempsterSweep(R, 1:5)[6, 1:5], X, Y)
# # # lm(Y~X)
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # # class(zz) <- "summary.lm"
# # # print.summary.lm(zz)
# # # flipMultivariates:::print.RegressionCorrelationsSummary(zz)
# # #
# # # zLS = lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)
# # #
# # # zSummaryLS <- summary(zLS)
# # #
# # # bank$s <- unclass(bank$Overall) / sd(unclass(bank$Overall), na.rm = TRUE)
# # # summary(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank))$sigma
# # #
# # # summary(lm(s ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank))$sigma * (sd(unclass(bank$Overall), na.rm = TRUE))
# # #
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing, type = type, detail = detail)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing, type = type, detail = detail)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, missing = missing, type = type, detail = detail)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = sb, missing = missing, type = type, detail = detail)
# # #
# # #
# # # # binary logit.
# # # data(bank)
# # # missing <- "Use partial data (pairwise correlations)"
# # #
# # # missing <- "Imputation (replace missing values with estimates)"
# # # missing = "Error if missing data"
# # # type = "Ordered Logit"
# # # wgt <- bank$ID
# # # attr(wgt, "label") <- "Bank ID"
# # # sb <- bank$ID > 100
# # # attr(sb, "label") <- "Bank ID greater than 100"
# # # type = "Linear"
# # # detail = FALSE
# # # z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing, type = type, detail = detail)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing, type = type, detail = detail)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, missing = missing, type = type, detail = detail)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = sb, missing = missing, type = type, detail = detail)
# # #
# # #
# # # z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = "Use partial data (pairwise correlations)")
# # #
# # # type = "Ordered"
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing, type = type)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing, type = type)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, missing = missing, type = type)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = sb, missing = missing, type = type)
# # #
# # # type = "NBD"
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing, type = type)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing, type = type)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, missing = missing, type = type)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = sb, missing = missing, type = type)
# # #
# # #
# # #
# # # # type type
# # # data(bank)
# # # Regression(Overall ~ Fees, data = bank, missing = "Imputation (replace missing values with estimates)")
# # #
# # #
# # # zdata <- bank#data.frame(Overall = Overall, Fees = Fees)
# # # Regression(Overall ~ Fees, zdata)
# # #
# # # Regression(log(Overall) ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)
# # #
# # # type = "Ordered Logit"
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, robust.se = TRUE)
# # #
# # #
# # # missing <- "Exclude cases with missing data"
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, missing = missing)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = sb, missing = missing)
# # #
# # # missing <- "Use partial data (pairwise correlations)"
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, missing = missing)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = sb, missing = missing)
# # #
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = TRUE, weights = NULL, missing = missing)
# # #
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing)
# # # zbank <- subset(bank, wgt > 100 & !is.na(wgt))
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = zbank, missing = missing)
# # # summary(zbank)
# # #
# # #
# # #
# # # print(dim(zbank))
# # # cv <- cov(zbank, use = "pairwise.complete.obs")
# # # psych::setCor(2, 3:8, zbank, std = FALSE)
# # #
# # # psych::setCor(2, 3:8, cv, std = FALSE, square = TRUE)
# # #
# # #
# # # psych::setCor(2, 3:8, zbank, std = TRUE)$beta
# # #
# # #
# # # psych::setCor(2, 3:8, cv, std = TRUE, square = TRUE)$beta
# # #
# # #
# # # psych::setCor(2, 3:8, zbank, std = FALSE)
# # #
# # #
# # #
# # #     .pairwise.regression <- psych::setCor
# # #     n <- length(body(.pairwise.regression))
# # #     while(as.character(body(.pairwise.regression)[n]) != "setCor.diagram(set.cor, main = main)")
# # #         n <- n - 1
# # #     body(.pairwise.regression)[n] <- NULL
# # #     .pairwise.regression(2, 3:8, zbank)
# # #
# # #     print(sum(bank,na.rm = TRUE))
# # #
# # # zzbank = AdjustDataToReflectWeights(zbank, weights)
# # # psych::setCor(2, 3:8, zzbank)
# # #
# # # zzzbank = bank[complete.cases(bank), ]
# # # psych::setCor(2, 3:8, zzzbank)$beta
# # #
# # #
# # #
# # # psych::setCor(2, 3:8, zzzbank, STD = TRUE)$beta
# # #
# # # unscaled.data <- zzzbank
# # # scaled.data <- as.data.frame(scale(zzzbank))
# # # sds.independent = apply(unscaled.data[,3:8], 2, sd, na.rm = TRUE)# / sd(zzzbank[,2], na.rm = TRUE)
# # # sds.independent
# # # sd.dependent <- sd(unscaled.data[,2], na.rm = TRUE)
# # # sd.dependent
# # #
# # #
# # #
# # # lm.unscaled <-summary(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = unscaled.data))$coef[-1,1]
# # # lm.unscaled
# # # lm.scaled <-summary(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = scaled.data))$coef[-1,1]
# # # lm.scaled
# # #
# # # lm.unscaled * (sds.independent / sd.dependent)
# # #
# # # lm.unscaled
# # # lm.unscaled / (sds.independent / sd.dependent)
# # #
# # # lm.scaled / (sds.independent / sd.dependent)
# # # lm.unscaled
# # #
# # # psych::setCor(2, 3:8, zzzbank, std = FALSE)$beta
# # # psych::setCor(2, 3:8, zzzbank, std = TRUE)$beta / (sds.independent / sd.dependent)
# # #
# # #
# # #
# # # summary(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = as.data.frame(scale(zzzbank))))$coef
# # # psych::setCor(2, 3:8, zzzbank, std = TRUE)$beta
# # #
# # # covm <- cov(bank)
# # #
# # #
# # # psych::setCor(2, 3:8, bank, std = TRUE)$beta
# # #
# # #
# # # bank.filtered <- subset(bank, wgt > 100)
# # # library(psych)
# # # psych::setCor(2, 3:8, bank.filtered)$beta
# # #
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing)
# # #
# # # #
# # # # ), predictors.index,
# # # #         data = estimation.data, std = FALSE)
# # # #     partial.coefs <- cbind(lm.cov$beta, lm.cov$se, lm.cov$t, lm.cov$Probability)
# # # #     dimnames(partial.coefs) <- list(variable.names[predictors.index],
# # # #         c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
# # # #     beta <- as.matrix(lm.cov$beta)
# # # #     fitted <- as.matrix(estimation.data[, predictors.index]) %*% beta
# # # #     intercept <- mean(estimation.data[, outcome.name], na.rm = TRUE) - mean(fitted, na.rm = TRUE)
# # # #     fitted <- as.matrix(data[, predictors.index]) %*% beta
# # # #     result$flip.fitted.values <- fitted + intercept
# # #
# # # missing <- "Imputation (replace missing values with estimates)"
# # # type = "Ordered"
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing, type = type)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing, type = type)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, missing = missing, type = type)
# # # Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = sb, missing = missing, type = type)
# # #
# # # library(MASS)
# # # z = polr(factor(Overall) ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank,weights = wgt, subset = sb)
# # # dim(fitted.values(z))
