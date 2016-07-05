#' Title
#'
#' @param x
#' @param min
#' @param max
#'
#' @return
#' @export
limit <- function(x, min = 0, max = 0.25) {
  x[x < min] <- NA
  x[x > max] <- NA
  return(x)
}
