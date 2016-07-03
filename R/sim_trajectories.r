#' Title
#'
#' @param model
#' @param parms
#' @param rho_1_ini
#' @param times
#' @param method
#'
#' @return
#' @export
#' @examples
#'
#' p <- set_parms(livestock$defparms, set = list(b = 0.9, c = 0.2, f = 0, p = 0, alpha = 0.2))
#' sim_trajectories(livestock, parms = p)
#'
#'

sim_trajectories<- function(model,
                          parms = model$defparms,
                          rho_1_ini = seq(0,1, length = 11),
                          times = c(0,1000),
                          func =  model$pair,
                          method = "ode45") {


  # draw trajectories of mortality and growth
  ini <- list(
    rho_1 = rho_1_ini,
    rho_11 = c(0, NA),
    rho_10 = NA,
    rho_00 = NA,
    rho_0 = NA

  )

  ini <- expand.grid(ini)


  ini$rho_11[is.na(ini$rho_11)] <- ini$rho_1[is.na(ini$rho_11)]


  for(x in 1:nrow(ini)) {
    temp <- unlist(expand_rho(c(ini[[x,1]], ini[[x,2]])))

    #if(ini[x,]$rho_11 == 0) {
    while(any(is.na(temp)) & ini[[x,2]] <= ini[[x,1]]) {
      ini[[x,2]] <-  ini[[x,2]]+0.005
      temp <- expand_rho(c(ini[[x,1]], ini[[x,2]]))
    }

    ini[x,] <- temp
  }


  ini$m_ini <- mortality(ini$rho_1, ini$rho_11/ini$rho_1, parms)
  ini$g_ini <- growth(ini$rho_1, ini$rho_10/ini$rho_0, parms)

  ini <- subset(ini, !is.na(m_ini) & !is.na(ini$g_ini) &  ini$g_ini >= 0)
  ini <- cbind(ID = 1:nrow(ini),ini)


  foreach(iteration = ini$ID, .packages = c("deSolve")) %dopar% {

    rho_starting <- unlist(ini[iteration, 2:3])

    # running the ode-solver
    runmodel <- deSolve::ode(rho_starting, func = func, times = 1.05^seq(0,100,1), parms = parms, method = method)

    return(as.data.frame(runmodel))
  } -> output

  return(output)
}
