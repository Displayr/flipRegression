#' @importFrom flipU IsCount
#' @importFrom formattable percent comma
#' @importFrom utils capture.output
#' @importFrom flipFormat FormatAsPValue
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
    if (ncol(x$model) > 2 & x$type == "Linear" & x$missing != "Use partial data (pairwise correlations)")
    {
        vifs <- vif(x)
        if (!is.null(vifs))
        {
            max.vif <- max(vifs)
            if (max.vif >= 4)
            {
                pref <- if(x$type == "Linear") "" else "Generalized "
                nms <- rownames(x$summary$coefficients)[-1]
                VIFs <- paste0(nms,": ", round(vifs, 2), c(rep("; ", length(nms) - 1), ""), collapse = "")
                warning(paste0("The ",pref, "Variance Inflation Factor of the coefficients are: ", VIFs,". A value of 4 or more indicates the confidence interval for the coefficient is twice as wide as they would be for uncorrelated predictors. A value of 10 or more indicates high multicollinearity."))
            }
        }
    }
    #Testing to see if the variance is non-constant.
    partial <- x$missing == "Use partial data (pairwise correlations)"
    if (x$type == "Linear" & !partial & !weighted)
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
            warning(paste0("The outcome variable appears to contain count data (i.e., the values are non-negative integers). A limited dependent variable regression may be more appropriate (e.g., Quasi-Poisson Regression, Ordered Logit)."))
    }
    # Creating a nicely formatted text description of the model.
    requireNamespace("formattable")
    aic <- if(partial) NA else AIC(x)
    rho.2 <- if(partial | x$type == "Linear") NA else McFaddensRhoSquared(x)
    caption <- x$sample.description
    caption <- if (partial)
         paste0(caption," R-Squared: ", round(x$original$original$R2, 4), "; ")
    else
         paste0(caption," R-Squared: ", round(GoodnessOfFit(x)$value, 4),
                          "; Correct predictions: ", percent(Accuracy(x)),
                          if (is.null(rho.2) | is.na(rho.2)) "" else paste0("; McFadden's rho-squared: ", round(rho.2, 4)),
                          if (is.na(aic)) "" else paste0("; AIC: ",comma(aic), "; "))
    if (x$detail)
    {
        cat(paste0(x$type, " regression\n"))
        print(x$summary, ...)
        # if (!is.null(x$original$original))
        #     cat(paste0("Partial-data R-squared ", flipU::FormatAsReal(x$original$original$R2, 4), " (the R-squared and F above are based only on complete cases).\n"))
        cat(caption)
        if (x$robust.se)
            cat("Heteroscedastic-robust standard errors.")
    }
    else
    {
        caption <- c(paste0(x$type, " Regression; ", caption))
        dt <- createRegressionDataTable(x, p.cutoff = p.cutoff, caption = caption)
        print(dt)
    }
}

#' @importFrom stats printCoefmat pt pt
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


# Create an HTML widget data table (package DT) from the coefficients
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
  coefs <- x$summary$coefficients
  # Ordered Logit tables don't come with a p-value column
  # so calculate the p's from the
  if (x$type == "Ordered Logit")
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
  } else if (test.info$test.type == "z") {
    z.val <- qnorm(p.cutoff / 2)
    dt <- AddSignificanceHighlightingToDataTable(dt, columns.to.color = 1,
                                                        column.to.check = test.info$test.column,
                                                        red.value = z.val, blue.value = -1L * z.val)
  }

  return(dt)
}
