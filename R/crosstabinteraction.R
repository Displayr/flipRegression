computeInteractionCrosstab <- function(result, interaction.name, interaction.label, formula.with.interaction, relative.importance, importance.absolute, interaction.pvalue, internal.loop, ...)
{
    net.coef <- summary(result$original)$coef[,1]
    correction <- result$correction
    if (internal.loop)
        correction <- "None"
    num.var <- length(net.coef)
    split.labels <- levels(result$estimation.data[,interaction.name])
    num.split <- length(split.labels)
    split.names <- paste0(interaction.name, split.labels)
    split.size <- table(result$estimation.data[,interaction.name])
    var.names <- names(net.coef)
    var.labels <- if (result$show.labels) Labels(result$model, var.names)
                  else                    var.names
    names(net.coef) <- var.labels
    names(split.size) <- split.labels

    weights <- result$weights[result$subset]
    original.r2 <- if (result$type == "Linear" & is.null(weights)) summary(result$original)$r.square
                   else 1 - deviance(result$original)/nullDeviance(result)

    res <- list(label = interaction.label, split.size = c(split.size, NET=sum(split.size)), 
                pvalue = NA, original.r2 = original.r2, full.r2 = NA, fit = NULL,
                net.coef = net.coef, relative.importance = relative.importance)

    if (!relative.importance)
    {
        fit2 <- FitRegression(formula.with.interaction, result$estimation.data,
                              result$subset, weights, result$type, result$robust.se, ...)
        atest <- ifelse (result$type %in% c("Linear", "Quasi-Poisson"), "F", "Chisq")
        if (!is.null(weights))
        {
            .design <- fit2$design
            assign(".design", .design, envir=.GlobalEnv)
        }
        atmp <- anova(result$original, fit2$original, test=atest)
        res$anova.output <- atmp
        res$full.r2 <- ifelse (result$type == "Linear" & is.null(weights), summary(fit2$original)$r.square,
                                                        1 - deviance(fit2$original)/nullDeviance(result))
        if (!is.null(weights))
            remove(".design", envir=.GlobalEnv)

        if (!internal.loop)
        {
            res$fit <- fit2$original
            res$anova.test <- switch(atest, F="F test", Chisq="Chi-square test")
            res$pvalue <- if(is.null(weights)) atmp$Pr[2]
                          else atmp$p

        } else
        {
            res$anova.fstat <- atmp[2,5]
            res$anova.dev <- atmp[2,4]
            res$anova.df1 <- -diff(atmp[,1])
            res$anova.df2 <- atmp[2,1]
        }
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
        #net.ri <- estimateRelativeImportance(result$formula, result$estimation.data, NULL, result$type, signs, NA, var.labels, result$robust.se)
        #res$net.coef <- net.ri$raw.importance
        res$net.coef <- result$relative.importance$raw.importance

        for (j in 1:num.split)
        {
            is.split <- which(result$estimation.data[,interaction.name] == split.labels[j])
            if (length(unique(result$estimation.data[is.split,1])) < 2 ||
                length(unique(result$estimation.data[-is.split,1])) < 2)
                next

            tmp.ri <- estimateRelativeImportance(result$formula, result$estimation.data[is.split,], weights[is.split], result$type, signs, NA, var.labels, result$robust.se, !internal.loop, correction)
            tmpC.ri <- estimateRelativeImportance(result$formula, result$estimation.data[-is.split,], weights[-is.split], result$type, signs, NA, var.labels, result$robust.se, !internal.loop, correction)

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

            tmp.fit <- try(FitRegression(result$formula, result$estimation.data[is.split,], NULL, weights[is.split], result$type, result$robust.se))
            tmp.coefs <- tidySummary(summary(tmp.fit$original), tmp.fit$original, result)$coef
            tmpC.fit <- try(FitRegression(result$formula, result$estimation.data[-is.split,], NULL, weights[-is.split], result$type, result$robust.se))
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
    if (internal.loop)
    {
        res$bb <- bb
        res$ss <- ss
        res$bc <- bc
        res$sc <- sc
        return(res)
    }

    res$coef.sign <- compareCoef(bb, bc, ss^2, sc^2, split.size, correction)
    if (interaction.pvalue)
        res$coef.pvalues <- compareCoef(bb, bc, ss^2, sc^2, split.size, correction, pvalues=TRUE)

    combined.coefs <- cbind(bb, res$net.coef)
    colnames(combined.coefs) <- c(split.labels, "NET")
    rownames(combined.coefs) <- var.labels
    res$coefficients <- combined.coefs
    return(res)
}

#' @importFrom stats pt p.adjust
compareCoef <- function(bb, bc, ss, sc, nn, correction, alpha = 0.05, pvalues=FALSE)
{
    if (any(dim(bb) != dim(ss)))
        stop("Dimensions of bb and ss must be the same\n")
    if (length(nn) != ncol(bb))
        stop("Length of nn should match columns in bb\n")
    res <- matrix(0, nrow=nrow(bb), ncol=ncol(ss))

    nc <- sum(nn) - nn
    pp <- matrix(NA, nrow(bb), ncol(bb))
    tt <- matrix(NA, nrow(bb), ncol(bb))
    for (j in 1:ncol(bb))
    {
        vv <- (ss[,j] + sc[,j])^2/(ss[,j]^2/(nn[j]-1) + sc[,j]^2/(nc[j]-1))
        tt[,j] <- (bb[,j] - bc[,j])/sqrt(ss[,j] + sc[,j])

        for (i in 1:nrow(bb))
            pp[i,j] <- 2 * pt(abs(tt[i,j]), vv[i], lower.tail=F)
    }
    pp <- pvalAdjust(pp, correction)
    pp <- matrix(pp, nrow=nrow(bb))
    
    if (pvalues)
        return(pp)

    pp[is.na(pp)] <- 1
    res <- sign(tt) * (pp < alpha)
    return (res)
}

