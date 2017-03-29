
computeInteractionCrosstab <- function(result, interaction.name, interaction.label, formula.with.interaction, ...)
{
        # Compute table of coefficients
        tmp.coef <- summary(result$original)$coef[,1]
        num.var <- length(tmp.coef)
        split.labels <- levels(result$estimation.data[,interaction.name])
        num.split <- length(split.labels)
        split.names <- paste0(interaction.name, split.labels)
        split.size <- table(result$estimation.data[,interaction.name])
        var.names <- names(tmp.coef)
        all.names <- sprintf("%s%s", var.names,
                    rep(c("", paste0(":", split.names[-1])), each=length(var.names)))
        all.names <- gsub("(Intercept):", "", all.names, fixed=T)

        weights <- result$weights[result$subset]
        fit2 <- FitRegression(formula.with.interaction, result$estimation.data, result$subset, weights, result$type, result$robust.se, ...)
        atest <- if (result$type %in% c("Linear", "Quasi-Poisson")) "F"
                 else                                        "Chisq"
        atmp <- anova(result$original, fit2$original, test=atest)
        anova.test <- switch(atest, F="F test", Chisq="Chi-square test")
        pvalue <- atmp$Pr[2]

        rsum2 <- summary(fit2$original)
        if (result$robust.se && length(rsum2$coef[,1]) < num.split * num.var)
        {
            warning("Robust SE not used as some coefficients are undefined\n")
            result$robust.se <- FALSE
        }
        rsum2 <- tidySummary(rsum2, fit2$original, result)
        tmp.coef2 <- rsum2$coef[,1]
        coef.tab <- matrix(tmp.coef2[all.names], ncol=num.split)
        coef.tab <- cbind(coef.tab[,1], sweep(coef.tab[,-1], 1, coef.tab[,1], "+"))

        # Only check differences between coefficients if we accept fit2
        coef.sign <- matrix(0, nrow(coef.tab), ncol(coef.tab))
        if (pvalue < 0.05)
        {
            bb <- matrix(NA, num.var, num.split)
            bc <- matrix(NA, num.var, num.split)
            ss <- matrix(NA, num.var, num.split)
            sc <- matrix(NA, num.var, num.split)

            var.ind <- (1:num.var)
            for (j in 1:num.split)
            {
                is.split <- which(result$estimation.data[,interaction.name] == split.labels[j])
                tmp.fit <- FitRegression(result$formula, result$estimation.data[is.split,], NULL, weights, result$type, result$robust.se)
                tmp.coefs <- summary(tmp.fit$original)$coef
                tmpC.fit <- FitRegression(result$formula, result$estimation.data[-is.split,], NULL, weights, result$type, result$robust.se)
                tmpC.coefs <- summary(tmpC.fit$original)$coef
                if (nrow(tmp.coefs) == num.var)
                {
                    bb[,j] <- tmp.coefs[var.ind,1]
                    ss[,j] <- tmp.coefs[var.ind,2]^2
                    bc[,j] <- tmpC.coefs[var.ind,1]
                    sc[,j] <- tmpC.coefs[var.ind,2]^2
                }
            }
            coef.sign <- compareCoef(bb, bc, ss, sc, split.size)
        }
        split.size <- c(split.size, NET=sum(split.size))
        combined.coefs <- cbind(coef.tab, tmp.coef)
        colnames(combined.coefs) <- c(split.labels, "NET")
        rownames(combined.coefs) <- if (result$show.labels) Labels(result$model, var.names)
                                    else             var.names
        result$interaction.label <- interaction.label 
        return(list(name = interaction.name, label = interaction.label,
                    anova.test = anova.test, pvalue = pvalue, fit = fit2$original,
                    coefficients = combined.coefs, coef.sign = coef.sign, split.size = split.size))
    }


compareCoef <- function(bb, bc, ss, sc, nn, alpha = 0.05)
{
    if (any(dim(bb) != dim(ss)))
        stop("Dimensions of bb and ss must be the same\n")
    if (length(nn) != ncol(bb))
        stop("Length of nn should match columns in bb\n")
    res <- matrix(0, nrow=nrow(bb), ncol=ncol(ss))

    nc <- sum(nn) - nn
    for (j in 1:ncol(bb))
    {
        vv <- (ss[,j] + sc[,j])^2/(ss[,j]^2/(nn[j]-1) + sc[,j]^2/(nc[j]-1))
        t.stat <- (bb[,j] - bc[,j])/sqrt(ss[,j] + sc[,j])

        for (i in 1:nrow(bb))
        {
            pp <- 2 * pt(abs(t.stat[i]), vv[i], lower.tail=F)
            if (!is.na(pp) && pp < alpha)
                res[i,j] <- sign(t.stat[i])

            #cat(round(bb[i,j],3), round(t.stat[i], 3), round(pp, 3), ";")
        }
    }
    return (res)
}

