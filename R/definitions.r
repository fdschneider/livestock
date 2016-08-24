#' title: Model definitions
#'
#' @usage
#'
#' death(rho, parms)
#' colonization(rho, parms)
#'
#' @details defining chances of death and colonization per location, depending on the global and local vegetation cover.
#'
#' @export

death <- function(rho, parms = livestock$parms) {

  # substitutions
  a = parms$a + parms$v * q_11(rho)/rho[[1]] #* (1 - parms$p) * q_11(rho)
  a = a * rho[[1]]^(parms$q)  # density dependent search efficiency: fr type III
  h = parms$h #^(1 + parms$p * q_11(rho))
  L = parms$L * (1 -  parms$p * q_11(rho) )

  # individual death rate
  out <-  parms$m  + a * L / (1 + a * h * rho[[1]])

  out[out < 0] <- 0
  return(as.vector(out))

}

#' @export
mortality <- function(rho, parms = livestoch$parms) {
  death(rho, parms) * rho[[1]]
}

#' @export

colonization <- function(rho, parms = livestock$parms)  {

  # substitutions
  b = parms$b + (1- parms$b) * parms$f * q_01(rho)  # facilitation
  r = parms$r * rho[[1]]^parms$alpha   # water runoff
  K = parms$K *  (1 - parms$c * q_01(rho))

  # individual colonization rate
  out <-   r * rho[[1]] * b * (1 - rho[[1]]/K ) / (1-rho[[1]])

  out[out < 0] <- 0
  return(as.vector(out))
}

#' @export
growth <- function(rho, parms = livestock$parms)  {
  colonization(rho, parms) * (1-rho[[1]])
}
