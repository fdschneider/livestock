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
#' library(foreach)
#' p <- set_parms(livestock$parms, set = list(b = 0.2,f = 0.8))
#' sim_trajectories(livestock, parms = p)
#'
#'

sim_trajectories<- function(model,
                          parms = model$parms,
                          rho_1_ini = seq(0,1, length = 11),
                          times = c(0,1000),
                          func =  model$pair,
                          method = "ode45") {


  # draw trajectories of mortality and growth
  ini <- list(
    rho_1 = c(rho_1_ini),
    rho_11 = c(0, NA)

  )

  ini <- expand.grid(ini)


  ini$rho_11[is.na(ini$rho_11)] <- ini$rho_1[is.na(ini$rho_11)] # replace NA-placeholder: assume maximal clustering, i.e. all vegetated cells have occupied neighbors. rho_11 == rho_1,

  rho <- ini_rho(ini$rho_1, ini$rho_11)
#
#   for(x in 1:nrow(ini)) {
#     temp <- unlist(expand_rho(c(ini[[x,1]], ini[[x,2]])))
#
#     #if(ini[x,]$rho_11 == 0) {
#     while(any(is.na(temp)) & ini[[x,2]] <= ini[[x,1]]) {
#       ini[[x,2]] <-  ini[[x,2]]+0.005
#       temp <- expand_rho(c(ini[[x,1]], ini[[x,2]]))
#     }
#
#     ini[x,] <- temp
#   }

  ini$q_11 <- q_11(rho) # <- data.frame(rho_1 = rho$rho_1, rho_11 = rho$rho_11)
  ini$m_ini <- mortality(rho, parms)
  ini$g_ini <- growth(rho, parms)

  ini <- cbind(ID = 1:nrow(ini),ini)
  ini <- subset(ini, !is.na(m_ini) & !is.na(ini$g_ini) &  ini$g_ini >= 0)



  foreach(iteration = ini$ID, .packages = c("deSolve")) %dopar% {

    rho_starting <- ini_rho(ini$rho_1[ini$ID == iteration],ini$rho_11[ini$ID == iteration])

    # running the ode-solver
    runmodel <- run_ode(rho_starting, func = func, times = 1.05^seq(0,100,1), parms = parms, method = method)
    colnames(runmodel) <- c("time", "rho_1", "rho_11")
    return(as.data.frame(runmodel))
  } -> output

  return(output)
}
