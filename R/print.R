
#' @importFrom flipU IsCount
#' @importFrom utils capture.output
#' @importFrom flipFormat FormatAsPValue FormatAsReal FormatAsPercent RegressionTable MultinomialLogitTable ExtractCommonPrefix RelativeImportanceTable CrosstabInteractionTable
#' @importFrom stats printCoefmat
#' @method print Regression
#' @export
print.Regression <- function(x, p.cutoff = 0.05, digits = max(3L, getOption("digits") - 3L), ...)
{
    weighted <- !is.null(x$weights)
    # Checking for unusual observations.
    if (x$type != "Ordered Logit" & x$type != "Multinomial Logit" & x$missing != "Use partial data (pairwise correlations)")
    {
        capture.output(unusual <- UnusualObservations(x))
        if (!is.null(unusual))
            warning(unusual)
    }
    # Testing to see if there is multicollinearity.
    if (length(x$original$coefficients) > 2 & ncol(x$model) > 2 & x$type == "Linear" & x$missing != "Use partial data (pairwise correlations)")
    {
        vifs <- vif(x)
        if (!is.null(vifs))
        {
            max.vif <- max(vifs)
            if (max.vif >= 4)
            {
                pref <- if(x$type == "Linear") "" else "Generalized "
                nms <- rownames(x$summary$coefficients)[-1]
                VIFs <- paste0(nms,": ", FormatAsReal(vifs, 2), c(rep("; ", length(nms) - 1), ""), collapse = "")
                warning(paste0("The ",pref, "Variance Inflation Factor of the coefficients are: ", VIFs,". A value of 4 or more indicates the confidence interval for the coefficient is twice as wide as they would be for uncorrelated predictors. A value of 10 or more indicates high multicollinearity."))
            }
        }
    }
    #Testing to see if the variance is non-constant.
    partial <- x$missing == "Use partial data (pairwise correlations)"
    if (x$type == "Linear" & !partial & !weighted & x$robust.se == FALSE)
    {
        bp.test <- ncvTest(x)#BreuschPagan(x$original)
        if (bp.test$p <= 0.05)
        {
            suggest <- if(is.null(x$partial.coefs)) " Or, consider using Robust Standard Errors." else ""
            warning(paste0("A Breusch Pagan Test for non-constant variance has been failed (p = ",
                           FormatAsPValue(bp.test$p), "). A plot of the residuals versus the fitted values of the outcome variable may be useful (Insert > Advanced > Regression > Plots > Residuals vs Fitted). A transformation of the outcome or predictor variables may solve this problem.",
                           suggest, "\n"))
            outcome.variable <- outcomeVariableFromModel(x)
        }
    }
    outcome.variable <- outcomeVariableFromModel(x)
    if (length(unique(outcome.variable)) == 2 && x$type == "Linear")
        warning(paste0("The outcome variable contains only two unique values. A Binary Logit may be
                       more appropriate."))
    else
    {
        if (x$type == "Linear" & IsCount(outcome.variable))
            warning(paste0("The outcome variable appears to contain categories (i.e., the values are non-negative integers). A limited dependent variable regression may be more appropriate (e.g., Ordered Logit for ordered categories, Multinomial Logit for unordered categories, Quasi-Poisson Regression for counts)."))
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
        add.regression <- x$type %in% c("Linear", "Poisson", "Quasi-Poisson", "NBD")
        title <- paste0(regressionType(x$type), ": ", x$outcome.label)
        if (is.na(x$interaction$pvalue))
            subtitle <- paste("Interaction with", x$interaction$label)
        else
            subtitle <- paste0(x$interaction$anova.test, " for interaction with ", x$interaction$label, ": P-value ", FormatAsPValue(x$interaction$pvalue))
        dt <- CrosstabInteractionTable(x$interaction$coefficients,
                                       x$interaction$coef.sign,
                                       x$interaction$split.size,
                                       title = title,
                                       footer = caption,
                                       subtitle = subtitle)
        print(dt)
    }
    else if (!is.null(x$relative.importance))
    {
        lbls <- extractVariableCoefficientNames(x)
        title <- paste0("Relative Importance Analysis (", regressionType(x$type), "): ", x$outcome.label)
        extracted <- ExtractCommonPrefix(lbls)
        if (!is.na(extracted$common.prefix))
        {
            lbls <- extracted$shortened.labels
            title <- paste0(title, " by ", extracted$common.prefix)
        }
        dt <- RelativeImportanceTable(x$relative.importance, lbls, title, footer = x$relative.importance.footer)
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
        attr(x$anova, "footer") <- x$footer
        print(x$anova)
    }
    else
    {    # Pretty table.
        add.regression <- x$type %in% c("Linear", "Poisson", "Quasi-Poisson", "NBD")
        title <- paste0(regressionType(x$type), ": ", x$outcome.label)
        coefs <- x$summary$coefficients
        #statistic.name <- if ("t" == substr(colnames(coefs)[3], 1, 1)) "t" else
        statistic.name <- paste0("<span style='font-style:italic;'>", substr(colnames(coefs)[3], 1, 1) ,"</span>")
        se.name <- if (x$robust.se != FALSE) "Robust SE" else "Standard Error"
        #"<span style='font-style:italic;'>t</span>",
        subtitle <- if (!is.null(x$subtitle)) x$subtitle else ""

        if (x$type != "Multinomial Logit" || x$missing == "Multiple imputation")
        {
            lbls <- rownames(coefs)
            ind <- if (x$type != "Ordered Logit")
                2:length(lbls)
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
                                        x$z.statistics,
                                        x$p.values,
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
        if (x$direction == "Backward")
        {
            var.names <- sapply(as.character(x$model$anova$Step[-1]), function(x) substr(x, 3, nchar(x)))
            x$model$subtitle <- paste("Variables excluded:", paste0(var.names, collapse = ", "))
        }
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



