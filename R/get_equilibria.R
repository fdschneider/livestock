#' Title
#'
#' @param odesys
#' @param parms
#' @param t_max
#' @param method
#'
#' @return
#' @export
#'
#' @examples
#'
#' get_equilibria(livestock$pair, ini_rho(rho_1 = 0.9999), livestock$defparms)
#' get_equilibria(livestock$meanfield, ini_rho(rho_1 = 0.9999), livestock$defparms)

get_equilibria <- function(odesys,
                           y,
                           parms,
                           t_max = 1000,
                           method = "ode45") {
  # simulate cover at t_1000 starting from high cover to get vegetated steady state
  hi <- as.numeric(run_ode(y, func = odesys, times = c(1,t_max), parms = parms, method = method)[2,-1])

  # simulate cover at t_1000 starting from high cover to get vegetated steady state
  lo <- as.numeric(run_ode(y = ini_rho(1-y[1]), func = odesys, times = c(1,t_max), parms = parms, method = method)[2,-1])

  out <- list(lo = lo, hi = hi,  mid = NA)

  # simulate unstable equilibrium and draw point
  for(i in 1:10) {

    mid <- (lo+hi)/2
    runmodel_mid <- as.data.frame(run_ode(mid, func = odesys, times = c(0,0.2), parms = parms, method = method))[2,-1]

    if(runmodel_mid[1] > mid[1]) { hi <- mid } else { lo <- mid}

  }
  out$mid <- mid
  return(out)
}
