#' \code{LinearRegressionFromCorrelations} Linear regression via a sweep operation n the correlation matrix.
#'
#' @param formula An object of class \code{\link{formula}} (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#'   The details of type specification are given under \sQuote{Details}.
#' @param data A \code{\link{data.frame}}.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param ... Additional argments to be past to  \code{\link[psych]{setCor}}.
#' @details Estimates regression using sweep operations on the correlation matrix.
#' The sample size assumed in tests is the smallest pairwise correlation sample size.
#' @importFrom flipU OutcomeName AllVariablesNames
#' @importFrom flipStatistics CovarianceAndCorrelationMatrix  StandardDeviation Mean
#' @importFrom psych setCor
#' @importFrom flipData CheckForPositiveVariance CheckCorrelationMatrix
#' CheckForLinearDependence CalibrateWeight
#' @importFrom stats complete.cases
#' @importFrom flipFormat Labels BaseDescription
#' @export
LinearRegressionFromCorrelations <- function(formula, data = NULL, subset = NULL,
                                                                               weights = NULL, ...)
{
    result <- NULL
    variable.names <- names(data)
    full.variable.names <- Labels(data, show.name = TRUE)
    formula.names <- AllVariablesNames(formula, data)
    indices <- match(formula.names, variable.names)
    outcome.name <- OutcomeName(formula, data)
    outcome.index <- match(outcome.name, variable.names)
    predictors.index <- indices[-match(outcome.name, formula.names)]
    indices <- c(outcome.index, predictors.index)
    factors <- unlist(lapply(data[,indices], is.factor))
    if (any(factors))
        stop(paste0("Factors are not permitted when missing is set to 'Use partial data (pairwise)'.
                    Factors: ", paste(variable.names[indices][factors], collapse = ", ")))
    subset.data <- if(is.null(subset)) data else subset(data, subset)
    n.subset <- nrow(subset.data)
    if (n.subset < length(predictors.index) + 1)
        stop(warningSampleSizeTooSmall())
    n.total <- nrow(data)
    weighted <- !is.null(weights)
    if (n.subset < n.total & weighted)
        weights <- subset(weights, subset)
    y <- subset.data[, outcome.index]
    x <- subset.data[, predictors.index]
    if (weighted)
        weights <- CalibrateWeight(weights)
    y.and.x <- subset.data[, c(outcome.index, predictors.index)]
    k <- ncol(y.and.x)
    pairwise.n <- matrix(NA, k, k)
    for (r in 2:ncol(y.and.x))
        for (c in 1:r)
        {
            pairwise.data <- !is.na(y.and.x[,c]) & !is.na(y.and.x[,r])
            pairwise.n[r, c] <- if(weighted) sum(weights[pairwise.data], na.rm = TRUE) else sum(pairwise.data)
        }
    min.pairwise.n <- min(pairwise.n, na.rm = TRUE)
    cors <- if (weighted)
        CovarianceAndCorrelationMatrix(y.and.x, weights, TRUE, TRUE)
    else
        cor(y.and.x, use = "pairwise.complete.obs")
    # Checking data
    CheckForPositiveVariance(y.and.x)
    CheckCorrelationMatrix(cors)
    CheckForLinearDependence(cors)
    # Doing the computation.
    original <- setCor(1, 2:ncol(cors), data = cors, n.obs = min.pairwise.n, plot = FALSE, z = NULL, ...)
    result$original <- original
    scaled.beta <- as.matrix(original$beta)
    sds <- StandardDeviation(y.and.x, weights)
    sds.independent <- sds[-1]
    result$original$sd.dependent <- sd.dependent <- sds[1]
    beta <- scaled.beta / (sds.independent / sd.dependent)
    se <- original$se / (sds.independent / sd.dependent)
    partial.coefs <- cbind(beta, se, original$t, original$Probability)
    dimnames(partial.coefs) <- list(variable.names[predictors.index],
                                    c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
    fitted <- as.matrix(data[, predictors.index]) %*% beta
    resid.valid <- !is.na(fitted) & complete.cases(data[, predictors.index])
    mean.y <- Mean(y[resid.valid], weights[resid.valid])
    mean.y.hat <- Mean(fitted[resid.valid], weights[resid.valid])
    intercept <- mean.y - mean.y.hat
    result$predicted.values <- result$fitted.values <- fitted + intercept
    partial.coefs <- partial.coefs[c(1,1:nrow(partial.coefs)),]
    partial.coefs[1,] <- c(intercept, NA, NA, NA)
    rownames(partial.coefs)[1] <- "(Intercept)"
    result$coef <- partial.coefs[, 1]
    result$subset <- !is.na(data[outcome.index]) & !is.na(fitted) & (rownames(data) %in% rownames(subset.data))
    # Sample description.
    result$n.observations <- n.total <- nrow(data)
    weight.label <- if(weighted <- !is.null(weights)) Labels(weights) else  ""
    rng <- range(pairwise.n[lower.tri(pairwise.n)], na.rm = TRUE)
    n.min <- rng[1]
    if (n.min == rng[2])
        description <- paste0("n = ", n.min,
                              " cases used in estimation\n")
    else
        description <- paste0("Pairwise correlations have been used to estimate this regression.\n",
                              "Sample sizes for the correlations range from ", n.min, " to ", rng[2], "")
    description <- BaseDescription(description,
                                   n.total, attr(subset, "n.subset"),
                                   n.min, Labels(subset), NULL, "")
    if (!is.null(weights))
        description <- paste0(description, " Standard errors and statistical tests use calibrated weights (",
                              weight.label, ").\n")
    result$sample.description <- description
    result$outcome.index <- outcome.index
    result$n.predictors <- length(predictors.index)
    result$estimation.data <- subset.data
    result$original$coefficients <- partial.coefs
    class(result) <- "RegressionCorrelations"
    result
}

#' @export
df.residual.RegressionCorrelations <- function(object, ...)
{
    object$original$df[2]
}

#' summary.RegressionCorrelations
#'
#' @param object Regression model.
#' @param ... Other arugments.
#' @method summary RegressionCorrelations
#' @importFrom xml2 read_xml
#' @export
summary.RegressionCorrelations <- function(object, ...)
{
    n <- object$estimation.data
    result <- list(coefficients = object$original$coefficients)
    p <- nrow(object$original$coefficients)
    result$sigma <- object$original$residual * object$original$sd.dependent
    result$df <- object$original$df[c(1:2,1)]
    result$fstatistic <- c(value = object$original$F, numdf = object$original$df[1], numdf = object$original$df[2])
    result$r.squared <- object$original$R2
    result$adj.r.squared <- object$original$shrunkenR2
    class(result) <- "RegressionCorrelationsSummary"
    result
}

#' @importFrom stats sd
unstandardizeBetas <- function(scaled.betas, x, y)
{
    x.sd = apply(x, 2, sd, na.rm = TRUE)
    y.sd <- sd(y, na.rm = TRUE)
    scaled.betas / (x.sd / y.sd)
}


betasFromCorrelations <- function(R)
{
    p <- ncol(R) - 1
    indices <- 1:p
    R11 <- R[indices, indices, drop = FALSE]
    R12 <- R[indices, -indices, drop = FALSE]
    R21 <- R[-indices, indices, drop = FALSE]
    R22 <- R[-indices, -indices, drop = FALSE]
    b <- R21 %*% solve(R11)
    ESS <- R22 - b %*% t(R21)
    list(ESS = ESS, b = b)
}

inverseBootstrap <- function(x, probabilities)
{
    n <- nrow(x)
    replicants = sample.int(n, size = n, replace = TRUE, prob = probabilities)
    x[replicants, ]
}

#' @importFrom stats sd cor
# Bootstrapping
regressionFromCorrelations <- function(y, x, weights = rep(1, nrow(x)), b = 999)
{
    data <- as.matrix(cbind(x, y))
    prob = weights / sum(weights)
    betas <- matrix(NA, b, ncol(x))
    for (i in 1:b)
    {
        dat <- inverseBootstrap(data, prob)
        r <- cor(dat, use = "pairwise.complete.obs")
        b <- betasFromCorrelations(r)$b
        betas[i, ] <- b
    }
    apply(betas, 2, sd)
}


# swp <- function(x, indices = 1:ncol(x))
# {
#     p <- ncol(x)
#     g <- x
#     result <- x
#     X11 <- x[indices, indices, drop = FALSE]
#     X12 <- x[indices, -indices, drop = FALSE]
#     X21 <- x[-indices, indices, drop = FALSE]
#     X22 <- x[-indices, -indices, drop = FALSE]
#     B <- X21 %*% solve(X11)
#     result[indices, indices] <- -solve(X11)
#     result[-indices, -indices] <- X22 - B %*% t(X21)
#     result[-indices, indices] <- B
#     result[indices, -indices] <- t(B)
#     result
# }
#
# swp(R, 1:6)
#
# C <-  matrix(c(9, 3,  4, -2,  5,3,  8,  6,  5 , 4, 4,  6,  7,  3,  1,-2,  5,  3 , 9,  2,5,  4,  1,  2,  8), ncol = 5, byrow = TRUE)
# swp(C, 1:3)

#
#
# setCor <- function (y, x, data, z = NULL, n.obs = NULL, use = "pairwise",
#           std = TRUE, square = FALSE, main = "Regression Models", plot = TRUE)
# {
#     require("psych")
#     cl <- match.call()
#     if (is.numeric(y))
#         y <- colnames(data)[y]
#     if (is.numeric(x))
#         x <- colnames(data)[x]
#     if (is.numeric(z))
#         z <- colnames(data)[z]
#     if ((dim(data)[1] != dim(data)[2]) | square) {
#         n.obs = dim(data)[1]
#         if (!is.null(z)) {
#             data <- data[, c(y, x, z)]
#         }
#         else {
#             data <- data[, c(y, x)]
#         }
#         if (!is.matrix(data))
#             data <- as.matrix(data)
#         if (!is.numeric(data))
#             stop("The data must be numeric to proceed")
#         C <- cov(data, use = use)
#         if (std) {
#             m <- cov2cor(C)
#             C <- m
#         }
#         else {
#             m <- C
#         }
#         raw <- TRUE
#     }
#     else {
#         raw <- FALSE
#         if (!is.matrix(data))
#             data <- as.matrix(data)
#         C <- data
#         if (std) {
#             m <- cov2cor(C)
#         }
#         else {
#             m <- C
#         }
#     }
#     nm <- dim(data)[1]
#     xy <- c(x, y)
#     numx <- length(x)
#     numy <- length(y)
#     numz <- 0
#     nxy <- numx + numy
#     m.matrix <- m[c(x, y), c(x, y)]
#     x.matrix <- m[x, x, drop = FALSE]
#     xc.matrix <- m[x, x, drop = FALSE]
#     xy.matrix <- m[x, y, drop = FALSE]
#     xyc.matrix <- m[x, y, drop = FALSE]
#     y.matrix <- m[y, y, drop = FALSE]
#     if (!is.null(z)) {
#         numz <- length(z)
#         zm <- m[z, z, drop = FALSE]
#         za <- m[x, z, drop = FALSE]
#         zb <- m[y, z, drop = FALSE]
#         x.matrix <- x.matrix - za %*% solve(zm) %*% t(za)
#         y.matrix <- y.matrix - zb %*% solve(zm) %*% t(zb)
#         xy.matrix <- xy.matrix - za %*% solve(zm) %*% t(zb)
#         m.matrix <- cbind(rbind(y.matrix, xy.matrix), rbind(t(xy.matrix),
#                                                             x.matrix))
#     }
#     if (numx == 1) {
#         beta <- matrix(xy.matrix, nrow = 1)/x.matrix[1, 1]
#     }
#     else {
#         beta <- solve(x.matrix, xy.matrix)
#         beta <- as.matrix(beta)
#     }
#     yhat <- t(xy.matrix) %*% solve(x.matrix) %*% (xy.matrix)
#     resid <- y.matrix - yhat
#     if (numy > 1) {
#         if (is.null(rownames(beta))) {
#             rownames(beta) <- x
#         }
#         if (is.null(colnames(beta))) {
#             colnames(beta) <- y
#         }
#         R2 <- colSums(beta * xy.matrix)/diag(y.matrix)
#     }
#     else {
#         colnames(beta) <- y
#         R2 <- sum(beta * xy.matrix)/y.matrix
#         R2 <- matrix(R2)
#         rownames(beta) <- x
#         rownames(R2) <- colnames(R2) <- y
#     }
#     px <- principal(x.matrix)
#     keys.x <- diag(as.vector(1 - 2 * (px$loadings < 0)))
#     py <- principal(y.matrix)
#     keys.y <- diag(as.vector(1 - 2 * (py$loadings < 0)))
#     Vx <- sum(keys.x %*% x.matrix %*% t(keys.x))
#     Vy <- sum(keys.y %*% y.matrix %*% t(keys.y))
#     ruw <- colSums(abs(xy.matrix))/sqrt(Vx)
#     Ruw <- sum(diag(keys.x) %*% xy.matrix %*% t(keys.y))/sqrt(Vx *
#                                                                   Vy)
#     if (numy < 2) {
#         Rset <- 1 - det(m.matrix)/(det(x.matrix))
#         Myx <- solve(x.matrix) %*% xy.matrix %*% t(xy.matrix)
#         cc2 <- cc <- T <- NULL
#     }
#     else {
#         if (numx < 2) {
#             Rset <- 1 - det(m.matrix)/(det(y.matrix))
#             Myx <- xy.matrix %*% solve(y.matrix) %*% t(xy.matrix)
#             cc2 <- cc <- T <- NULL
#         }
#         else {
#             Rset <- 1 - det(m.matrix)/(det(x.matrix) * det(y.matrix))
#             if (numy > numx) {
#                 Myx <- solve(x.matrix) %*% xy.matrix %*% solve(y.matrix) %*%
#                     t(xy.matrix)
#             }
#             else {
#                 Myx <- solve(y.matrix) %*% t(xy.matrix) %*% solve(x.matrix) %*%
#                     (xy.matrix)
#             }
#         }
#         cc2 <- eigen(Myx)$values
#         cc <- sqrt(cc2)
#         T <- sum(cc2)/length(cc2)
#     }
#     if (!is.null(n.obs)) {
#         k <- length(x)
#         uniq <- (1 - smc(x.matrix))
#         se.beta <- list()
#         for (i in 1:length(y)) {
#             df <- n.obs - k - 1
#             se.beta[[i]] <- (sqrt((1 - R2[i])/(df)) * sqrt(1/uniq))
#         }
#         se <- matrix(unlist(se.beta), ncol = length(y))
#         colnames(se) <- colnames(beta)
#         rownames(se) <- rownames(beta)
#         se <- t(t(se) * sqrt(diag(C)[y]))/sqrt(diag(xc.matrix))
#         tvalue <- beta/se
#         prob <- 2 * (1 - pt(abs(tvalue), df))
#         SE2 <- 4 * R2 * (1 - R2)^2 * (df^2)/((n.obs^2 - 1) *
#                                                  (n.obs + 3))
#         SE = sqrt(SE2)
#         F <- R2 * df/(k * (1 - R2))
#         pF <- 1 - pf(F, k, df)
#         shrunkenR2 <- 1 - (1 - R2) * (n.obs - 1)/df
#         u <- numx * numy
#         m1 <- n.obs - max(numy, (numx + numz)) - (numx + numy +
#                                                       3)/2
#         s <- sqrt((numx^2 * numy^2 - 4)/(numx^2 + numy^2 - 5))
#         if (numx * numy == 4)
#             s <- 1
#         v <- m1 * s + 1 - u/2
#         R2set.shrunk <- 1 - (1 - Rset) * ((v + u)/v)^s
#         L <- 1 - Rset
#         L1s <- L^(-1/s)
#         Rset.F <- (L1s - 1) * (v/u)
#         df.m <- n.obs - max(numy, (numx + numz)) - (numx + numy +
#                                                         3)/2
#         s1 <- sqrt((numx^2 * numy^2 - 4)/(numx^2 + numy^2 - 5))
#         if (numx^2 * numy^2 < 5)
#             s1 <- 1
#         df.v <- df.m * s1 + 1 - numx * numy/2
#         Chisq <- -(n.obs - 1 - (numx + numy + 1)/2) * log((1 -
#                                                                cc2))
#     }
#     if (is.null(n.obs)) {
#         set.cor <- list(beta = beta, R = sqrt(R2), R2 = R2, Rset = Rset,
#                         T = T, cancor = cc, cancor2 = cc2, raw = raw, residual = resid,
#                         ruw = ruw, Ruw = Ruw, x.matrix = x.matrix, y.matrix = y.matrix,
#                         Call = cl)
#     }
#     else {
#         set.cor <- list(beta = beta, se = se, t = tvalue, Probability = prob,
#                         R = sqrt(R2), R2 = R2, shrunkenR2 = shrunkenR2, seR2 = SE,
#                         F = F, probF = pF, df = c(k, df), Rset = Rset, Rset.shrunk = R2set.shrunk,
#                         Rset.F = Rset.F, Rsetu = u, Rsetv = df.v, T = T,
#                         cancor = cc, cancor2 = cc2, Chisq = Chisq, raw = raw,
#                         residual = resid, ruw = ruw, Ruw = Ruw, x.matrix = x.matrix,
#                         y.matrix = y.matrix, Call = cl)
#     }
#     class(set.cor) <- c("psych", "setCor")
#     if (plot)
#         setCor.diagram(set.cor, main = main)
#     return(set.cor)
# }

