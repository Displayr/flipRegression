
#' @importFrom flipU IsCount
#' @importFrom utils capture.output
#' @importFrom flipFormat FormatAsPValue FormatAsReal FormatAsPercent
#' @importFrom stats printCoefmat
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
                VIFs <- paste0(nms,": ", FormatAsReal(vifs, 2), c(rep("; ", length(nms) - 1), ""), collapse = "")
                warning(paste0("The ",pref, "Variance Inflation Factor of the coefficients are: ", VIFs,". A value of 4 or more indicates the confidence interval for the coefficient is twice as wide as they would be for uncorrelated predictors. A value of 10 or more indicates high multicollinearity."))
            }
        }
    }
    #Testing to see if the variance is non-constant.
    partial <- x$missing == "Use partial data (pairwise correlations)"
    if (x$type == "Linear" & !partial & !weighted & !x$robust.se)
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
            warning(paste0("The outcome variable appears to contain categories (i.e., the values are non-negative integers). A limited dependent variable regression may be more appropriate (e.g., Quasi-Poisson Regression, Ordered Logit, Multinomial Logit)."))
    }
    # Creating a nicely formatted text description of the model.
    aic <- if(partial) NA else AIC(x)
    rho.2 <- if(partial | x$type == "Linear") NA else McFaddensRhoSquared(x)
    caption <- x$sample.description
    caption <- paste0(caption," R-squared: ", FormatAsReal(x$r.squared, 4), "; ")
    if (!partial)
        caption <- paste0(caption,
               "Correct predictions: ", FormatAsPercent(Accuracy(x, x$subset, x$weights), 4),
               if (is.null(rho.2) | is.na(rho.2)) "" else paste0("; McFadden's rho-squared: ", round(rho.2, 4)),
               if (is.na(aic)) "" else paste0("; AIC: ", FormatAsReal(aic, 5), "; "))
    if (x$detail) # Detailed text output.
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
    else # Pretty table.
    {
        add.regression <- x$type %in% c("Linear", "Poisson", "Quasi-Poisson", "NBD")
        title <- c(paste0(x$type, (if(add.regression) " Regression" else ""), ": ",
                          x$outcome.label))
        coefs <- x$summary$coefficients
        t <- "t" == substr(colnames(coefs)[3], 1, 1)
        caption <- paste0(caption, "results highlighted when p <= " , p.cutoff)
        dt <- PrettyRegressionTable(coefs,
                                    t,
                                    title = title,
                                    #subtitle = x$call,
                                    footer = caption)
        #dt <- createRegressionDataTable(x, p.cutoff = p.cutoff, caption = caption)
        print(dt)
    }
}


#' PrettyRegressionTable
#'
#' Creates a pretty formattable table.
#' @param coefficient.table A table of regression coefficients, standard errors, z or t statistics, and p-values.
#' @param t \code{TRUE} if t-statistics, and \code{FALSE} if z-statistics.
#' @param footer Text to place in the footer of the table.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @references This is based on code written by Kenton Russell.
#' @importFrom rmarkdown html_dependency_jquery html_dependency_bootstrap
#' @importFrom formattable format_table formatter digits style gradient csscolor as.htmlwidget formattable
#' @importFrom htmltools tags tagList browsable attachDependencies HTML
#' @export
PrettyRegressionTable <- function(coefficient.table, t, footer, title = "", subtitle = "")
{
    robust.se <- colnames(coefficient.table)[2] == "Robust SE"
    # Set the number of decimails
    fixedDigits <- function(x, n = 2) {
        formatC(x, digits = n, format = "f")
    }
    # FOrmat the p-values.
    pFormatter <- formatter(
        "span",
        style = p ~ ifelse(p <= 0.05, style(font.weight = "bold"), NA),
        p ~ {
            p.formatted <- fixedDigits(p, 3)
            p.formatted <- gsub(x = p.formatted, pattern="^(-?)0", replacement="\\1")
            p.formatted[p < 0.001] <- "< .001"
            p.formatted
        }
    )

    # Add tiles to t- and z- statistics.
    .colorScale <- function(x)
    { # Creates a color range where 0 is white
        abs.x <- abs(x)
        abs.x[abs.x < 1.959964] <- 0
        abs.x[is.na(x)] <- 0
        min.x <- min(abs.x)
        lower <- if (min.x == 0) "white" else
             gradient(c(0, min.x, abs.x),"white", "orange")[2]
        csscolor(gradient(abs.x, lower, "orange"))
    }
    tFormatter <- formatter(
        "span",
        style = x ~ style(
            display = "block",
            padding = "0 4px", `border-radius` = "4px",
            `background-color` = .colorScale(x)
        ),
        ~ fixedDigits(t, 2)
    )
    estimateFormatter <- formatter(
        "span",
        style = ~ ifelse(
            p <= 0.05 & t < 0,
            "color:red",
            ifelse(p <= 0.05 & t > 0, "color:blue", NA)
        ),
        Estimate ~ fixedDigits(Estimate, 2)
    )

    coef.df <- data.frame(coefficient.table, check.names=FALSE)
    colnames(coef.df)[3:4] <- c("t","p")
    test.statistic <- if (t) "t" else "z"
    subtitle.format <- if (subtitle == "") NULL
    else tags$h5(
        class=".h5",
        style="color:green; text-align:left;",
        subtitle)
    title.format <- if (title == "") NULL else tags$h3(
        class=".h3",
        style="color:blue; text-align:center;",
        title)

    tbl <- format_table(
        coef.df,
        col.names = c(
            "Estimate",
            (if(robust.se) "Robust<br/>SE" else "Standard<br/>Error"),
            paste0("<span style='font-style:italic;'>", test.statistic, "</span>"),
            "<span style='font-style:italic;'>p</span>"
        ),
        table.attr = paste0(
            'class = "table table-condensed"',
            'style = "margin:0; border-bottom: 2px solid; border-top: 2px solid; font-size:90%;"',
            sep = " "
        ),
        align = rep("r",5),
        caption = tagList(
            title.format,
            subtitle.format,
            tags$caption(
                style="caption-side:bottom;font-style:italic; font-size:90%;",
                footer
            )
        ),
        # formatters = list(
        #     Estimate = estimateFormatter,
        #     "Std. Error" = x~digits(x,2),
        #     t = tFormatter,
        #     p = pFormatter
        # )
        formatters = if(robust.se)
                list(
                    Estimate = estimateFormatter,
                    "Robust SE" = x~digits(x,2),
                    t = tFormatter,
                    p = pFormatter
                )
            else
                list(
                    Estimate = estimateFormatter,
                    "Std. Error" = x~digits(x,2),
                    t = tFormatter,
                    p = pFormatter
                )

    )


    browsable(
        attachDependencies(
            tagList(
                HTML(tbl)),
            list(
                html_dependency_jquery(),
                html_dependency_bootstrap("default")
            )
        )
    )


    # this is a really ugly way to return a htmlwidget
    #  I will have to spend some time thinking through this.
    # start by setting up a dummy formattable
    ftw <- as.htmlwidget(formattable(data.frame()))
    # and replace the html with our formatted html from above
    ftw$x$html <- HTML(tbl)
    ftw

}


#' @importFrom stats printCoefmat pt pt pf
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

#' @export
print.Stepwise <- function(x, ...)
{
    if (x$output == "Final")
    {
        x$model$detail <- x$model$type == "Multinomial Logit"
        print(x$model)
    }
    else
    {
        x$model$detail <- TRUE
        print(x$model)
        cat("\n\n")
        heading <- attr(x$model$anova, "heading")
        cat(paste(heading[2:length(heading)], collapse = "\n"))
        cat("Overview of steps:")
        cat("\n")
        results.table <- data.frame(AIC = x$mode$anova$AIC)
        row.names(results.table) <- levels(x$mode$anova$Step)
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



