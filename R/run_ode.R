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

sim <- function(input, model, times =  exp(seq(0,4,length = 100))-1, parms = parms) {
 out <-  ode(y = ini_rho(input$rho_1), func = model, times, parms)
 return(as.data.frame(out))
}
