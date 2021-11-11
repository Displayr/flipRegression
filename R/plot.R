
#' @importFrom graphics plot
#' @export
plot.Regression <- function(x, which = 1, ...)
{
  checkAcceptableModel(x, c("lm", "glm"),"This plot")
  plot(x$original, which = which, ...)
}
