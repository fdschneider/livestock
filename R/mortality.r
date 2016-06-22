#' calculate plant mortality at a given cover and local cover
#'
#' @param rho_1
#' @param q_11
#' @param parms
#' @param set
#'
#' @return
#' @export
#'
mortality <- function(rho_1, q_11 = 1, parms = livestock$defparms) {

  with(parms, (m * rho_1) +( (a + v*q_11 )*(1-p*q_11) *rho_1^(1+q)*L)/(1+(a + v*q_11 )*(1-p*q_11)*h*(rho_1^(1+q)) ) )
}
