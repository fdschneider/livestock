#' Title
#'
#' @param rho_1
#' @param q_01
#' @param parms
#' @param set
#'
#' @return
#' @export

growth <- function(rho_1, q_01 = "auto", parms = livestock$defparms) {

  out <- with(parms, r*(b + (1-b)*f*q_01)*rho_1^(1+alpha)*(1-rho_1/(K * (1-c * q_01) ) ))

  out[out < 0] <- 0
  return(out)
}
