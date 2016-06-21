#' @export

d_rho <- function(rho, parms, z = 4) {
  with(parms, r* b* rho * (1 - rho/K) - m * rho - (a * rho * L)/(1 + a * h * rho) )
}


#' @export
noymeir <- list(
 defparms = list(
   m = 0.05,
   r = 1,
   b = 0.9,
   K = 0.9,
   a = 0.6,
   h = 100,
   L = 5
 ),
 odesys <- function (t, rho, parms = parms) {
   list( d_rho(rho, parms)  )
 }
)

