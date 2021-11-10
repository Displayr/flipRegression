
#' @importFrom graphics plot
#' @export
plot.Regression <- function(x, which = 1, ...)
{
  checkAcceptableModel(x, c("lm", "glm"),"This plot")
  res <- plot(x$original, which = which, ...)
  class(res) <- c(class(res), "visualization-selector")
  res
}
