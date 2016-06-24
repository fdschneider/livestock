#' Title
#'
#' @param model
#' @param parms
#' @param over
#' @param xrange
#' @param res
#' @param ini
#' @param t_max
#' @param method
#'
#' @return
#' @export
#'
#' @examples
#'
#' library(foreach)
#' library(doSNOW)
#'
#' workerlist <- c(rep("localhost", times = 3))
#' cl <- makeSOCKcluster(workerlist)
#' registerDoSNOW(cl)
#'
#' p <- set_parms(livestock$defparms, set = list(b = 0.9, c = 0.2, f = 0, p = 0, alpha = 0.2))
#' sim_bifurcations(livestock, over = "b", res = 31, parms = p)
#'
#' stopCluster(cl)
#'
sim_bifurcations <- function(
          model,
          parms = model$defparms,
          over = "b",
          xrange = c(0,1),
          res = 31,
          ini = c(0.9, 0.0001),
          t_max = 150,
          func = model$pair,
          method = "ode45"
          ) {

  parms[[over]] <- seq(xrange[1],xrange[2],length = res)
  parms$rho_ini <- ini

  iterations <- expand.grid(parms)
  iterations <- cbind(ID = 1:dim(iterations)[1],iterations)

  iterations$b <- as.numeric(as.character(iterations$b))
  iterations$L <- as.numeric(as.character(iterations$L))

  foreach(iteration = iterations$ID, .packages = c("deSolve", "livestock"), .combine = rbind) %dopar% {

    model_parms <- as.list(iterations[iteration,])

    rho_starting <- ini_rho(model_parms$rho_ini)

    # running the ode-solver
    runmodel <- run_ode(rho_starting, func = model$pair, times = c(0,t_max), parms = model_parms, method = method)

    return(tail(runmodel,1))
  } -> output

  output <- cbind(iterations,output) # CHECK: produces a warning

  return(output)

}
