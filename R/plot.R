
#' @importFrom graphics plot
#' @export
plot.Regression <- function(x,  ...)
{
  checkAcceptableModel(x, c("lm", "glm"),"This plot")
  plot(x$original, ...)
}
