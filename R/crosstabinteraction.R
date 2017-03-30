computeInteractionCrosstab <- function(result, interaction.name, interaction.label, formula.with.interaction, relative.importance, importance.absolute, interaction.pvalue, ...)
{
    net.coef <- summary(result$original)$coef[,1]
    num.var <- length(net.coef)
    split.labels <- levels(result$estimation.data[,interaction.name])
    num.split <- length(split.labels)
    split.names <- paste0(interaction.name, split.labels)
    split.size <- table(result$estimation.data[,interaction.name])
    var.names <- names(net.coef)
    var.labels <- if (result$show.labels) Labels(result$model, var.names)
                  else                    var.names
    weights <- result$weights[result$subset]
    
    anova.test <- ""
    pvalue <- NA
    fit2 <- NULL
    if (!relative.importance)
    {
        fit2 <- FitRegression(formula.with.interaction, result$estimation.data, result$subset, weights, result$type, result$robust.se, ...)
        atest <- if (result$type %in% c("Linear", "Quasi-Poisson")) "F"
                 else                                        "Chisq"
        atmp <- anova(result$original, fit2$original, test=atest)
        anova.test <- switch(atest, F="F test", Chisq="Chi-square test")
        pvalue <- atmp$Pr[2]
    }
    if (relative.importance)
        num.var <- num.var - 1
    
    coef.sign <- matrix(0, num.var, num.split)
    bb <- matrix(NA, num.var, num.split)
    bc <- matrix(NA, num.var, num.split)
    ss <- matrix(NA, num.var, num.split)
    sc <- matrix(NA, num.var, num.split)
 
    if (relative.importance)
    {
        var.labels <- if (result$type == "Ordered Logit") var.labels[1:(num.var-1)] else var.labels[-1]
        signs <- if (importance.absolute) 1 else NA
        net.ri <- estimateRelativeImportance(result$formula, result$estimation.data, NULL, result$type, signs, NA, var.labels, result$robust.se) 
        net.coef <- net.ri$raw.importance

        for (j in 1:num.split)
        {
            is.split <- which(result$estimation.data[,interaction.name] == split.labels[j])
            if (length(unique(result$estimation.data[is.split,1])) < 2 ||
                length(unique(result$estimation.data[-is.split,1])) < 2)
                next

            tmp.ri <- estimateRelativeImportance(result$formula, result$estimation.data[is.split,], weights[is.split], result$type, signs, NA, var.labels, result$robust.se) 
            tmpC.ri <- estimateRelativeImportance(result$formula, result$estimation.data[-is.split,], weights[-is.split], result$type, signs, NA, var.labels, result$robust.se) 

            bb[,j] <- tmp.ri$raw.importance
            ss[,j] <- tmp.ri$standard.errors
            bc[,j] <- tmpC.ri$raw.importance
            sc[,j] <- tmpC.ri$standard.errors
        }

    } else
    {
        for (j in 1:num.split)
        {
            is.split <- which(result$estimation.data[,interaction.name] == split.labels[j])
            if (length(unique(result$estimation.data[is.split,1])) < 2 ||
                length(unique(result$estimation.data[-is.split,1])) < 2)
                next

            tmp.fit <- try(FitRegression(result$formula, result$estimation.data[is.split,], NULL, weights, result$type, result$robust.se))
            tmp.coefs <- tidySummary(summary(tmp.fit$original), tmp.fit$original, result)$coef
            tmpC.fit <- try(FitRegression(result$formula, result$estimation.data[-is.split,], NULL, weights, result$type, result$robust.se))
            tmpC.coefs <- tidySummary(summary(tmpC.fit$original), tmpC.fit$original, result)$coef
            if (nrow(tmp.coefs) == num.var)
            {
                bb[,j] <- tmp.coefs[,1]
                ss[,j] <- tmp.coefs[,2]
                bc[,j] <- tmpC.coefs[,1]
                sc[,j] <- tmpC.coefs[,2]
            }
        }
    }
    coef.sign <- compareCoef(bb, bc, ss^2, sc^2, split.size)
    coef.pvalues <- NULL
    if (interaction.pvalue)
        coef.pvalues <- compareCoef(bb, bc, ss^2, sc^2, split.size, pvalues=TRUE)
    
    # Tidy results for printing
    split.size <- c(split.size, NET=sum(split.size))
    combined.coefs <- cbind(bb, net.coef)
    colnames(combined.coefs) <- c(split.labels, "NET")
    rownames(combined.coefs) <- var.labels
    result$interaction.label <- interaction.label

    return(list(name = interaction.name, label = interaction.label,
                anova.test = anova.test, pvalue = pvalue, fit = fit2$original,
                coefficients = combined.coefs, coef.sign = coef.sign, 
                coef.pvalues = coef.pvalues, split.size = split.size))
}


compareCoef <- function(bb, bc, ss, sc, nn, alpha = 0.05, pvalues=FALSE)
{
    if (any(dim(bb) != dim(ss)))
        stop("Dimensions of bb and ss must be the same\n")
    if (length(nn) != ncol(bb))
        stop("Length of nn should match columns in bb\n")
    res <- matrix(0, nrow=nrow(bb), ncol=ncol(ss))

    nc <- sum(nn) - nn
    pp <- matrix(NA, nrow(bb), ncol(bb))
    for (j in 1:ncol(bb))
    {
        vv <- (ss[,j] + sc[,j])^2/(ss[,j]^2/(nn[j]-1) + sc[,j]^2/(nc[j]-1))
        t.stat <- (bb[,j] - bc[,j])/sqrt(ss[,j] + sc[,j])

        for (i in 1:nrow(bb))
        {
            pp[i,j] <- 2 * pt(abs(t.stat[i]), vv[i], lower.tail=F)
            if (!is.na(pp[i,j]) && pp[i,j] < alpha)
                res[i,j] <- sign(t.stat[i])
            #cat(round(bb[i,j],3), round(t.stat[i], 3), round(sqrt(ss[i,j]), 3), round(pp[i,j], 3), "\n")
        }
    }
    if (pvalues)
        return(pp)
    return (res)
}

