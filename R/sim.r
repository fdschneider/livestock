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
#'
#' sim(ini_rho(0.2), livestock)

sim <- function(input, model, times = exp(seq(0,4,length = 100))-1, parms = model$defparms) {
 out <-  ode(y = input, func = model$odesys, times, parms)


 out[out < 1e-6] <- 0
 return(round(out,6))
 return(as.data.frame(out))
}
