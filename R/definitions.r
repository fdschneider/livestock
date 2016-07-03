#' title: Model definitions
#'
#' @export

mortality <- function(rho, parms = livestock$parms) {

  out <-   with(parms, m * rho[1] + ( a(rho, parms) * p(rho, parms) * rho[1]^(1 + q) * L)/(1 + a(rho, parms) * p(rho, parms) * h(rho,parms) * rho[1]^(1 + q)) )

  out[out < 0] <- 0
  return(as.vector(out))

}


#' @export
a <- function(rho, parms) { with(parms, a + v * q_11(rho[1],rho[2]) ) }

#' @export
h <- function(rho, parms) { with(parms, h ) }

#' @export
p <- function(rho, parms) { with(parms, 1 - p * q_11(rho[1],rho[2]) ) }

#' @export
L <- function(rho, parms) { with(parms, L) }


#' @export

growth0 <- function(rho, parms = livestock$parms)  {

  out <-   with(parms,
                r * (b + (1 - b) * f(rho, parms)) * rho[1]^( 1 + alpha) * (1 - rho[1]/(K * competition(rho, parms) ) ))

  out[out < 0] <- 0
  return(as.vector(out))
}

#' @export

f <- function(rho, parms) {  parms$f * q_01(rho[1],rho[2]) }

#' @export

competition <- function(rho, parms) {  1 - parms$c * q_01(rho[1],rho[2])  }

