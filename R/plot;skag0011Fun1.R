#' Plots for MLRModInfo
#'
#' @param x x data for default plot
#' @param y y data for default plot
#' @param ... more arguments
#'
#' @return 4 plots in one window
#' @export
#'
#' @examples
#' \dontrun{ plot(MLRModInfo(QUALITY ~ TEMP, PRODQUAL))}
plot.skag0011Fun1 <- function(x, y , ...){
  par(mfrow = c(2,2))
  plot(x$NewModel, main = "NEW Model Plots", ...)
}
