#' Title
#'
#' @param input
#' @param model
#' @param times
#' @param parms
#'
#' @return
#' @import deSolve
#' @export
#'
#' @examples
#'
#' sim(ini_rho(0.2,0.2), livestock)

sim <- function(input, model, times = exp(seq(0,4,length = 100))-1, parms = model$defparms, method = "ode45") {
 out <-  ode(y = input, func = model$odesys, times, parms, method = method)

 out[out < 1e-6] <- 0
 return(round(out,6))
 return(as.data.frame(out))
}
