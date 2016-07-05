#' Title
#'
#' @param input
#' @param model
#' @param times
#' @param parms
#'
#' @return
#' @export
#'
#' @examples
#' p <- set_parms(livestock$parms, set = list(b = 0.2, f = 0.9, p = 0.8))
#' out <- run_ode(ini_rho(0.5,0.45), livestock$pair, parms = p)
#' plot(out[,1:2], type = "l", ylim = c(0,1))
#'
#' out <- run_ode(ini_rho(0.5), livestock$meanfield, parms = p)
#' lines(out[,1:2])

run_ode <- function(y, func, times = 1.05^seq(0,100,1), parms, ...) {
  out <- deSolve::ode(y = y, func = func, times = times, parms = parms, ...)
  out[out < 1e-06] <- 0
  return(round(out,6))
  return(as.data.frame(out))
}
