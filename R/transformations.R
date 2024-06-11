#' Adjust P-values for multiple comparisons using FDR
#' @description This method gives different values from those outputted by \code{p.adjust} with \code{method = "fdr"}.
#'    This difference is that our method uses a single scaling factor to multiply the p-values. However, conclusions
#'    (i.e. significant or not) does not change.
#' @param p Vector of unadjusted p-values.
#' @param alpha Overall significance level.
#' @export
PValueAdjustFDR <- function(p, alpha = 0.05)
{
    p <- as.numeric(p)
    p.adj <- rep(NA, length(p))
    nna <- !is.na(p)

    p <- p[nna]
    ord <- order(p, decreasing = FALSE)
    n <- length(p)

    ii <- which(p[ord] * n / (1:n) < alpha)
    i <- if (length(ii) == 0) 1 else max(ii)
    p.adj[nna] <- pmin(1, n / i * p)
    p.adj
}

pvalAdjust <- function(p, correction)
{
    correction <- switch(
        correction,
        "None" = "none",
        "False Discovery Rate" = "fdr",
        "Benjamini & Yekutieli" = "BY",
        "Bonferroni" = "bonferroni",
        "Hochberg" = "hochberg",
        "Holm" = "holm",
        "Hommel" = "hommel",
        correction
    )
    if (correction == "fdr")
        return(PValueAdjustFDR(p))
    p.adjust(p, correction)
}
