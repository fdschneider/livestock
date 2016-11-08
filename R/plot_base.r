#' Base plot of growth and mortality over vegetation cover
#'
#' @param ...
#'
#' @return plots a blank coordinate system without content with predefined axis
#'   labels and limits. All graphical parameters can be set just as for plot().
#' @export
#'
plot_base <- function(..., ylab = "plant mortality/growth", xlab = "vegetation cover", ylim= c(0,.25)) {
  plot(NA,NA,
       ylab = ylab, xlab = xlab,
       xlim = c(0,1), ylim= ylim,
       bty = "n",
       xaxs = "i" , yaxs = "i",
       ...)
}
