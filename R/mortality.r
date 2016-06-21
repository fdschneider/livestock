#' Title
#'
#' @param rho_1
#' @param q_11
#' @param parms
#' @param set
#'
#' @return
#' @export
#'
mortality <- function(rho_1, q_11 = 1, parms = livestock$defparms, set = list(NA)) {

  parms.names <- names(parms)
  set.names <- names(set)
  m.names <- sort(unique(c(parms.names, set.names)))

  parms_temp <- sapply(m.names, function(i) {
    if (i %in% set.names) set[[i]]
    else parms[[i]]
  }, simplify = FALSE)

  with(parms_temp, (m * rho_1) +( (a + v*q_11 )*(1-p*q_11) *rho_1^(1+q)*L)/(1+(a + v*q_11 )*(1-p*q_11)*h*(rho_1^(1+q)) ) )

}
