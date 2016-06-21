#' Title
#'
#' @param rho_1
#' @param q_01
#' @param parms
#' @param set
#'
#' @return
#' @export

growth <- function(rho_1, q_01 = "auto", parms = livestock$defparms, set = list(NA)) {

  if(q_01[1] == "auto") q_01 <- 1


  parms.names <- names(parms)
  set.names <- names(set)
  m.names <- sort(unique(c(parms.names, set.names)))

  parms_temp <- sapply(m.names, function(i) {
    if (i %in% set.names) set[[i]]
    else parms[[i]]
  }, simplify = FALSE)

  out <- with(parms_temp, r*(b + (1-b)*f*q_01)*rho_1^(1+alpha)*(1-rho_1/(K * (1-c * q_01) ) ))

  out[out < 0] <- 0
  return(out)
}
