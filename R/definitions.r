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
  a = parms$a + parms$v * q_11(rho) * 1 - parms$p * q_11(rho)
  a = a * rho[1]^(parms$q)  # density dependent search efficiency: fr type III
  h = parms$h
  L = parms$L

  # individual death rate
  out <-  a * L / (1 + a * h * rho[1])

  out[out < 0] <- 0
  return(as.vector(out))

}



#' @export

colonization <- function(rho, parms = livestock$parms)  {

  # substitutions
  b = parms$b + (1- parms$b) * parms$f * q_01(rho)  # facilitation
  r = parms$r * rho[1]^parms$alpha   # water runoff
  K = parms$K *  1 - parms$c * q_01(rho)

  # individual colonization rate
  out <-   r * b * (1 - rho[1]/K )

  out[out < 0] <- 0
  return(as.vector(out))
}
