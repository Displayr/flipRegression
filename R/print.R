
#' @importFrom flipU IsCount
#' @importFrom utils capture.output
#' @importFrom flipFormat FormatAsPValue FormatAsReal FormatAsPercent RegressionTable MultinomialLogitTable ExtractCommonPrefix ImportanceTable CrosstabInteractionTable
#' @importFrom stats printCoefmat
#' @method print Regression
#' @export
print.Regression <- function(x, p.cutoff = 0.05, digits = max(3L, getOption("digits") - 3L), ...)
{
    weighted <- !is.null(x$weights)
    # Checking for unusual observations.
    type.relevant.for.outliers <- x$type != "Ordered Logit" && x$type != "Multinomial Logit"
    partial <- x$missing == "Use partial data (pairwise correlations)"
    output.not.jaccard.or.correlation <- !x$output %in% c("Jaccard Coefficient", "Correlation")
    if (type.relevant.for.outliers && !partial && output.not.jaccard.or.correlation)
    {
        capture.output(unusual <- UnusualObservations(x))
        if (!is.null(unusual))
            warning(unusual)
    }
    # Check for high correlations in predictors and warn if necessary
    if (is.null(x$importance))
        checkVIFAndWarn(x)
    #Testing to see if the variance is non-constant.
    if (x$type == "Linear" && !partial && !weighted && x$robust.se == FALSE && output.not.jaccard.or.correlation)
    {
        bp.test <- ncvTest(x)#BreuschPagan(x$original)
        if (bp.test$p <= 0.05)
        {
            suggest <- if(is.null(x$partial.coefs)) " Or, consider using Robust Standard Errors." else ""
            warning(paste0("A Breusch Pagan Test for non-constant variance has failed (p = ",
                           FormatAsPValue(bp.test$p), "). A plot of the residuals versus the fitted values of the
                           outcome variable may be useful (Regression > Diagnostic > Plot > Residuals vs Fitted).
                           A transformation of the outcome or predictor variables may solve this problem.",
                           suggest, "\n"))
            outcome.variable <- outcomeVariableFromModel(x)
        }
    }
    outcome.variable <- outcomeVariableFromModel(x)
    if (output.not.jaccard.or.correlation)
    {
        if (length(unique(outcome.variable)) == 2 && x$type == "Linear")
            warning(paste0("The outcome variable contains only two unique values. A Binary Logit may be
                         more appropriate."))
        else
        {
            if (x$type == "Linear" & IsCount(outcome.variable) && x$output != "Jaccard Coefficient")
                warning(paste0("The outcome variable appears to contain categories (i.e., the values are ",
                               "non-negative integers). A limited dependent variable regression may be ",
                               "more appropriate (e.g., Ordered Logit for ordered categories, Multinomial ",
                               "Logit for unordered categories, Quasi-Poisson Regression for counts)."))
        }
    }
    # # Creating a nicely formatted text description of the model.
    # aic <- if(partial) NA else AIC(x)
    # rho.2 <- if(partial | x$type == "Linear") NA else McFaddensRhoSquared(x)
    # caption <- x$sample.description
    # caption <- paste0(caption," R-squared: ", FormatAsReal(x$r.squared, 4), "; ")
    # if (!partial)
    #     caption <- paste0(caption,
    #            "Correct predictions: ", FormatAsPercent(Accuracy(x, x$subset, x$weights), 4),
    #            if (is.null(rho.2) | is.na(rho.2)) "" else paste0("; McFadden's rho-squared: ", round(rho.2, 4)),
    #            if (is.na(aic)) "" else paste0("; AIC: ", FormatAsReal(aic, 5), "; "))
    caption <- x$footer
    if (x$test.interaction)
    {
        if (x$output %in% c("R", "ANOVA"))
            warning(sprintf("Output '%s' is not available with Crosstab Interaction", x$output))

        add.regression <- x$type %in% c("Linear", "Poisson", "Quasi-Poisson", "NBD")
        title <- if (!is.null(x$interaction$importance))
        {
            if (x$output %in% c("Jaccard Coefficient", "Correlation"))
                paste0(x$importance.type, ": ", x$outcome.label)
            else
                paste0(x$importance.type, " (", regressionType(x$type), "): ", x$outcome.label)
        }
        else
            paste0(regressionType(x$type), ": ", x$outcome.label)

        if (!is.na(x$interaction$pvalue) && is.numeric(x$interaction$pvalue))
        {
            subtitle <- paste0(x$interaction$anova.test, " for interaction with ", x$interaction$label, ": P-value ", FormatAsPValue(x$interaction$pvalue))
        } else if (!is.null(x$interaction$coef.pFDR))
        {
            p.min <- min(x$interaction$coef.pFDR, na.rm=T)
            p.sig <- ifelse(p.min < 0.05, "significant", "non-significant")
            subtitle <- sprintf("Interaction with %s %s - Smallest p-value (after applying False Discovery Rate): %s", x$interaction$label, p.sig, FormatAsPValue(p.min))
        } else
        {
            subtitle <- paste("Interaction with", x$interaction$label)
        }

        if (!is.null(x$importance))
            caption <- paste(x$importance.footer, " importance scores have been normalized by column; p-values are based on raw importance scores")
        ind <- if (!is.null(x$importance))
            1:nrow(x$interaction$coefficients)
        else
            2:nrow(x$interaction$coefficients)
        res <- ExtractCommonPrefix(rownames(x$interaction$coefficients[ind,]))
        if (!is.na(res$common.prefix))
        {
            rownames(x$interaction$coefficients)[ind] <- res$shortened.labels
            title <- paste0(title, " by ", res$common.prefix)
        }
        relevant.coefs <- !grepDummyVars(row.names(x$interaction$coefficients))

        dt <- CrosstabInteractionTable(x$interaction$coefficients[relevant.coefs, , drop = FALSE],
                                       x$interaction$coef.tstat[relevant.coefs, , drop = FALSE],
                                       x$interaction$coef.pvalues[relevant.coefs, , drop = FALSE],
                                       x$interaction$split.size,
                                       title = title,
                                       footer = caption,
                                       subtitle = subtitle)
        print(dt)
    }
    else if (!is.null(x$importance))
    {
        lbls <- x$importance.labels
        if (!x$output %in% c("Jaccard Coefficient", "Correlation"))
            title <- paste0(x$importance.type ," (", regressionType(x$type), "): ", x$outcome.label)
        else
            title <- paste0(x$importance.type ,": ", x$outcome.label)
        extracted <- ExtractCommonPrefix(lbls)
        if (!is.na(extracted$common.prefix))
        {
            lbls <- extracted$shortened.labels
            title <- paste0(title, " by ", extracted$common.prefix)
        }
        dt <- ImportanceTable(x$importance, lbls, title, footer = x$importance.footer, output.type = x$output)
        print(dt)
    }
    else if (x$output == "R")
    {
        cat(paste0(x$type, " regression\n"))
        if (x$missing == "Multiple imputation")
        {
            printCoefmat(x$summary$coefficients)
            cat(caption)
        }
        else
        {
            x$summary$call <- x$formula
            print(x$summary, ...)
            # if (!is.null(x$original$original))
            #     cat(paste0("Partial-data R-squared ", flipU::FormatAsReal(x$original$original$R2, 4), " (the R-squared and F above are based only on complete cases).\n"))
            cat(caption)
            if (x$robust.se)
                cat("Heteroscedastic-robust standard errors.")
        }
    }
    else if (x$output == "ANOVA")
    {
        if (x$missing == "Multiple imputation")
            warning("ANOVA output is based on only the first imputed dataset.")

        relevant.coefs <- !grepDummyVars(row.names(x$anova))
        attr(x$anova, "footer") <- x$footer
        print(x$anova[relevant.coefs, ])
    }
    else if (x$output == "Effects Plot")
    {
        EffectsPlot(x,
                    max.factor.label.length = x$effects.format$max.label,
                    y.axis.title = x$effects.format$y.axis)
    }
    else
    {    # Pretty table.
        add.regression <- x$type %in% c("Linear", "Poisson", "Quasi-Poisson", "NBD")
        title <- paste0(regressionType(x$type), ": ", x$outcome.label)

        if (x$missing == "Dummy variable adjustment")
        {# Ignore the dummy variables, if they exist
            if (x$type != "Multinomial Logit")
            {
                relevant.coefs <- !grepDummyVars(rownames(x$summary$coefficients))
                coefs <- x$summary$coefficients[relevant.coefs, , drop = FALSE]
                z.statistics <- x$z.statistics[relevant.coefs, , drop = FALSE]
                p.values <- x$p.values[relevant.coefs, , drop = FALSE]
            }
            else
            {
                relevant.coefs <- !grepDummyVars(colnames(x$summary$coefficients))
                coefs <- x$summary$coefficients[, relevant.coefs, drop = FALSE]
                z.statistics <- x$z.statistics[, relevant.coefs, drop = FALSE]
                p.values <- x$p.values[, relevant.coefs, drop = FALSE]
            }

        } else
        {
            coefs <- x$summary$coefficients
            z.statistics <- x$z.statistics
            p.values <- x$p.values
        }

        #statistic.name <- if ("t" == substr(colnames(coefs)[3], 1, 1)) "t" else
        statistic.name <- paste0("<span style='font-style:italic;'>", substr(colnames(coefs)[3], 1, 1) ,"</span>")
        se.name <- if (x$robust.se != FALSE) "Robust SE" else "Standard Error"
        #"<span style='font-style:italic;'>t</span>",
        subtitle <- if (!is.null(x$subtitle)) x$subtitle else ""

        if (x$type != "Multinomial Logit" || x$missing == "Multiple imputation")
        {
            lbls <- rownames(coefs)
            ind <- if (x$type != "Ordered Logit")
                min(length(lbls), 2):length(lbls)
            else
                1:(length(lbls) - length(x$summary$lev) + 1)
            res <- ExtractCommonPrefix(lbls[ind])
            if (!is.na(res$common.prefix))
            {
                rownames(coefs)[ind] <- res$shortened.labels
                title <- paste0(title, " by ", res$common.prefix)
            }
            dt <- RegressionTable(coefs,
                                  title = title,
                                  footer = caption,
                                  se.name = se.name,
                                  statistic.name = statistic.name,
                                  subtitle = subtitle)
            print(dt)
        }
        else
        {
            lbls <- colnames(coefs)
            ind <- 2:length(lbls)
            res <- ExtractCommonPrefix(lbls[ind])
            if (!is.na(res$common.prefix))
            {
                colnames(coefs)[ind] <- res$shortened.labels
                title <- paste0(title, " by ", res$common.prefix)
            }
            dt <- MultinomialLogitTable(coefs,
                                        z.statistics,
                                        p.values,
                                        title = title,
                                        subtitle = subtitle,
                                        footer = caption)
            print(dt)
        }
    }
}


regressionType <- function(type)
{
    add.regression <- type %in% c("Linear", "Poisson", "Quasi-Poisson", "NBD")
    c(paste0(type, (if(add.regression) " Regression" else "")))
}

#' print.Anova
#'
#' Prints the Anova object
#' @param x An \link{Anova} object.
#' @param ... Additional parameters to \code{\link{print.Anova}}
#' @importFrom flipFormat AnovaTable
#' @method print Anova
#' @export
print.Anova <- function(x, ...)
{
    col.names <- colnames(x)
    subtitle <- attr(x, "heading")[1]
    subtitle <- substr(subtitle, 1, nchar(subtitle) - 1)
    subtitle <- paste0(regressionType(attr(x, "type")), ": ", subtitle)
    statistic.name <- if (ncol(x) == 4)  col.names[3] else NULL
    title <- paste("Analysis of Variance: ", attr(x, "outcome.label"))
    if (!is.null(attr(x, "by.label")))
        title <- paste0(title, attr(x, "by.label"))
    dt <- AnovaTable(x,
              title = title,
              footer = attr(x, "footer"),
              subtitle = subtitle)
    print(dt)
}


#' @importFrom stats printCoefmat pt pt pf
#' @method print RegressionCorrelationsSummary
#' @export
print.RegressionCorrelationsSummary <- function(x, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor,
                                                signif.stars = getOption("show.signif.stars"), ...)
{
    printCoefmat(x$coefficients, digits = digits, signif.stars = signif.stars,
                 na.print = "NA", ...)
    df <- x$df
    rdf <- df[2L]
    cat("\nResidual standard error:", format(signif(x$sigma,
                                                    digits)), "on", rdf, "degrees of freedom\n")
    if (!is.null(x$fstatistic)) {
        cat("Multiple R-squared: ", formatC(x$r.squared, digits = digits))
        cat(",\tAdjusted R-squared: ", formatC(x$adj.r.squared, digits = digits), "\nF-statistic:", formatC(x$fstatistic[1L],
                                                                                                            digits = digits), "on", x$fstatistic[2L], "and",
            x$fstatistic[3L], "DF,  p-value:", format.pval(pf(x$fstatistic[1L],
                                                              x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE),
                                                           digits = digits))
        cat("\n")
    }
}

#' @method print Stepwise
#' @export
print.Stepwise <- function(x, ...)
{
    if (x$output == "Final")
    {
        x$model$output <- "Coefficients"
        if (length(x$excluded) > 0)
            x$model$subtitle <- paste("Variables excluded:",
                                      paste0(x$excluded, collapse = ", "))
        print(x$model)
    }
    else
    {
        x$model$output <- "R"
        print(x$model)
        cat("\n\n")
        heading <- attr(x$model$anova, "heading")
        cat(paste(heading[2:length(heading)], collapse = "\n"))
        cat("Overview of steps:")
        cat("\n")
        results.table <- data.frame(AIC = x$model$anova$AIC)
        row.names(results.table) <- as.character(x$model$anova$Step)
        row.names(results.table)[1] <- "Original model"
        print(results.table)
        if (x$output == "All")
        {
            cat("\n\n")
            cat("All steps:")
            cat("\n\n")
            cat(x$steps.output)
        }
    }
}

# Create an HTML widget from the coefficients
# table in a regression summary.
#' @importFrom flipFormat DataTableWithRItemFormat AddSignificanceHighlightingToDataTable
#' @importFrom stats pt qt qnorm fitted fitted.values
createRegressionDataTable <- function(x, p.cutoff, caption = NULL, coeff.digits = 2,
                                      p.digits = 2, coefficient.indices = 1:2,
                                      test.index = 3, p.index = 4,
                                      eps.p = 0.001)
{
  # Given a table of coefficients from a summary of a regression
  # figure out which test has been used and which column the test
  # statistics are found in.
  .findTestInCoefficientTable <- function(coefficient.table) {
    col.names <- colnames(coefficient.table)
    t.col <- which(col.names == "t value")
    z.col <- which(col.names == "z value")
    if (length(t.col) == 0 && length(z.col) == 0)
    {
      test.type <- "none"
      test.column <- NULL

    } else if (length(t.col) > 0 && length(z.col) > 0 || length(t.col) > 1 || length(z.col) > 1) {
      stop("Ambiguous statistical testing information in coefficients table.")
    } else if (length(t.col) > 0) {
      test.type <- "t"
      test.column <- t.col
    } else {
      test.type <- "z"
      test.column <- z.col
    }

    return(list(test.type = test.type, test.column = test.column))
  }

    # Create a formatted array of regression coefficient information that can be passed to an HTMLwidget
    # DataTable.
    .formatRegressionCoefficientMatrix <- function (x, coeff.digits = 2,
                                                  p.digits = 2, coefficient.indices = 1:2,
                                                  test.index = 3, p.index = 4,
                                                  eps.p = 0.001)
    {
        d <- dim(x)
        num.cols <- d[2]

        coefficients <- data.matrix(x)
        tidied.coefficients <- array("", dim = d, dimnames = dimnames(coefficients))
        na.values <- is.na(coefficients)
        normal.indices <- c(coefficient.indices, test.index)
        tidied.coefficients[,normal.indices] <- format(round(coefficients[, normal.indices], digits = coeff.digits), digits = coeff.digits)
        tidied.coefficients[,p.index] <- format.pval(coefficients[, p.index], digits = p.digits, eps = eps.p)

        # Print any NA values
        if (any(na.values))
          tidied.coefficients[na.values] <- "NA"

        return(as.data.frame(tidied.coefficients))
      }

    # Make a pretty table with a caption
    #coefs <- if (x$missing == "Multiple imputation") x$coef.table else x$summary$coefficients
    coefs <- x$summary$coefficients
    # Ordered Logit tables don't come with a p-value column
    # so calculate the p's from the
    if (x$type == "Ordered Logit" & x$missing != "Multiple imputation")
    {
    ps = 2*pt(-abs(coefs[, test.index]), df = x$summary$df.residual)
    coefs = cbind(coefs, ps)
    }
    pretty.coefs <- .formatRegressionCoefficientMatrix(coefs, coeff.digits,
                                                     p.digits, coefficient.indices,
                                                     test.index, p.index,
                                                     eps.p)
    pretty.coefs <- as.data.frame(pretty.coefs, stringsAsFactors = FALSE)

    caption <- paste0(caption, "Results highlighted when p <= " , p.cutoff)


    dt <- DataTableWithRItemFormat(pretty.coefs,
                                        caption = caption,
                                        header.alignments = rep("right", ncol(pretty.coefs)),
                                        page.length = nrow(pretty.coefs),
                                        allow.paging = FALSE,
                                        show.info = FALSE)

    # Highlight significant coefficients
    test.info <- .findTestInCoefficientTable(coefs)
    if (test.info$test.type == "t")
    {
        t.val <- qt(p.cutoff / 2, df = df.residual(x))
        dt <- AddSignificanceHighlightingToDataTable(dt, columns.to.color = 1,
                                                        column.to.check = "t value",#test.info$test.column,
                                                        red.value = t.val, blue.value = -1L * t.val)
    }
    else if (test.info$test.type == "z")
    {
        z.val <- qnorm(p.cutoff / 2)
        dt <- AddSignificanceHighlightingToDataTable(dt, columns.to.color = 1,
                                                            column.to.check = test.info$test.column,
                                                        red.value = z.val, blue.value = -1L * z.val)
    }
    return(dt)
}



#' Computes and inspects the variance inflation factors (VIFs) and throws a warning if they are high
#' @param x A Regression object
#' @importFrom flipU InterceptExceptions
#' @noRd
checkVIFAndWarn <- function(x)
{
    n.coefs <- length(x$original$coef) # partial match to coef needed since partial regression doesnt have coefficients element, only coef
    n.variables <- ncol(x$model)
    # Check enough predictor variables available for VIF, also check no predictors are aliased
    # Safe to do this since aliased predictors checked and if so, warning thrown during Regression creation
    if (n.coefs > 2 && n.variables > 2 && x$missing != "Use partial data (pairwise correlations)" && !any(x$summary$aliased))
    {
        invalid.model.for.vifs <- x$type == "Ordered Logit" || x$type == "Multinomial Logit"
        vifs <- InterceptExceptions(vif(x),
                                    error.handler = function(e) {
                                        if (!(e$message == "'vif' is not computed for models of this type or class." && invalid.model.for.vifs))
                                              warning('Variance Inflation Factors (VIFs) could not be computed for this model: ', e$message)
                                    })
        # Prepare suggestion of RIA for warning scenarios
        suggestion.for.ria <- paste0("Consider conducting a relative importance analysis by selecting the ",
                                     "output to be Relative Importance Analysis.")
        if (x$type == "Linear")
          suggestion.for.ria <- paste0(gsub("\\.$", "", suggestion.for.ria), " or Shapley Regression.")
        # Throw warning about possible multi-collinearity in the case of NaN observed in vifs
        # vifs computed using a ratio calculation. NaNs will be thrown if the numerator and denominator are zero or
        # if the vif is negative (due to numerical instability near zero) and is later square-rooted in vif.
        # Throw general warning since it is difficult to determine where the possible collinearity is evident.
        if (any(is.nan(vifs)) || any(vifs < 0))
        {
            warning("Possible multicollinearity detected in the data. ", suggestion.for.ria)
            vifs <- NULL
        }


        if (!is.null(vifs))
        {
            # Check if the GVIF is used and square it
            if (NCOL(vifs) == 3)
            {
                vifs <- vifs[, 3]^2
                prefix <- "squared Generalized "
            } else
                prefix <- ""
            # Check is the largest calculated VIF is large enough for a warning.
            max.vif <- max(vifs)
            if (max.vif >= 4)
            {
                vifs <- vifs[vifs >= 4]
                # Helper function to create the VIF statements
                .printVIFs <- function(x, prefix, regression.object, dummy = FALSE)
                {
                    if (regression.object$show.labels)
                    {
                        labels <- Labels(regression.object$model, names(x))
                        extracted <- ExtractCommonPrefix(labels)
                        if (!is.na(extracted$common.prefix))
                          labels <- extracted$shortened.labels
                        names(x) <- labels
                    } else
                        labels <- names(x)
                    # Sort output by alphabetical labels
                    x <- sort(x, decreasing = TRUE)
                    printed.values <- paste0(names(x), " = ", FormatAsReal(x, 2), c(rep("; ", length(labels) - 1), ""),
                                             collapse = "")

                    paste0("A high ", prefix, "Variance Inflation Factor ",
                           ifelse(dummy,
                                  paste0("for the added dummy variable", ngettext(length(x), " is ", "s are "),
                                         "observed in the coefficient"),
                                  "is observed in the coefficient"),
                           ngettext(length(x), " for ", "s "),
                           printed.values, ". ")
                }
                dummy.vars <- grepDummyVars(names(vifs))
                vifs.msg <- NULL
                if (any(!dummy.vars))
                    vifs.msg <- c(vifs.msg, .printVIFs(vifs[!dummy.vars], prefix, x))
                if (any(dummy.vars))
                {
                    dummy.vifs <- vifs[dummy.vars]
                    # Remove disambiguation string
                    names(dummy.vifs) <- gsub(".dummy.var_GQ9KqD7YOf$", "", names(dummy.vifs))
                    vifs.msg <- paste0(vifs.msg, .printVIFs(dummy.vifs, prefix, x, dummy = TRUE))
                }
                # Collate message and throw warning.
                explanation.msg <- paste0("A value of 4 or more indicates the confidence interval for the coefficient ",
                                          "is twice as wide as they would be for uncorrelated predictors. A value of ",
                                          "10 or more indicates high multicollinearity. ")
                warning.msg <- paste0(vifs.msg, explanation.msg, suggestion.for.ria)
                warning(warning.msg)
            }
        }
    }
    return(invisible())
}
