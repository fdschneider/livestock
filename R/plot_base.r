#' Base plot of growth and mortality over vegetation cover
#'
#' @param ...
#'
#' @return
#' @export
#'
plot_base <- function(...) {
  plot(NA,NA,
       ylab = "plant mortality/growth", xlab = "vegetation cover",
       xlim = c(0,1), ylim= c(0,.25),
       bty = "n",
       xaxs = "i" , yaxs = "i",
       ...)
}
