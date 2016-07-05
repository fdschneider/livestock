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
#' @param colors
#' @param new
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
#' p <- set_parms(livestock$parms, set = list(b = 0.3))
#' plot_bifurcation(livestock, over = "L", xrange = c(0,3), parms = p)
#'
#' stopCluster(cl)
#'
#'
plot_bifurcation <- function(
                      model,
                      parms = model$parms,
                      over = "b",
                      xrange = c(0,1),
                      res = 201,
                      ini = c(0.9, 0.0001),
                      t_max = 150,
                      method = "ode45",
                      xlab = over,
                      colors = c("#000000","#009933"),
                      new = FALSE
                    ) {

  equilibria <- sim_bifurcations(model, over = over, xrange = xrange, ini = ini, t_max = t_max, res = res, parms = parms, method = method)

  plot(equilibria$rho_1 ~ equilibria[,over],
       xlab = xlab, ylab = "vegetation cover",
       pch = 20, cex = 0.66,
       ylim = c(0,1))


  parms[[over]] <- seq(xrange[1],xrange[2],length = res)
  parms$rho_ini <- ini

  iterations <- expand.grid(parms)
  iterations <- cbind(ID = 1:dim(iterations)[1],iterations)

  iterations$b <- as.numeric(as.character(iterations$b))
  iterations$L <- as.numeric(as.character(iterations$L))

  # draw mean-field estimate of unstable equilibrium (threshold)

  upper <- equilibria[equilibria$rho_ini == ini[1],][which(round(equilibria[equilibria$rho_ini == ini[1],]$rho_1,4) != round(equilibria[equilibria$rho_ini == ini[2],]$rho_1,4)),]
  lower <- equilibria[equilibria$rho_ini == ini[2],][which(round(equilibria[equilibria$rho_ini == ini[2],]$rho_1,4) != round(equilibria[equilibria$rho_ini == ini[1],]$rho_1,4)),]

  if(nrow(upper)>0) {
    foreach(i = upper[,over], .combine = rbind, .packages = c("deSolve") ) %dopar% {

      model_parms <- upper[ upper[, over] == i,]

      hi_1 <- upper[upper[, over] == i,]$rho_1
      lo_1 <- lower[lower[, over] == i,]$rho_1
      hi_11 <- upper[upper[, over] == i,]$rho_11
      lo_11 <- lower[lower[, over] == i,]$rho_11

      for(j in 1:10) {

        rho_ini <- ini_rho( (hi_1+lo_1)/2 )

        # running the ode-solver

        runmodel <- run_ode(rho_ini, func = model$pair, times = c(0,1.5), parms = model_parms, method = method)

        if(runmodel[2,"rho_1"] < runmodel[1,"rho_1"] ) {
          lo_1 <- (hi_1+lo_1)/2
          #lo_11 <- (hi_11+lo_11)/2
        } else {
          hi_1 <- (hi_1+lo_1)/2
          #hi_11 <- (hi_11+lo_11)/2
        }

      }

      return(tail(runmodel,1))
    } -> output_unstable
  }

    output_unstable <- cbind(upper[,1:16],output_unstable)
    points(output_unstable$rho_1~ output_unstable[,over], pch = 20, cex = 0.66, ylim = c(0,1), col = "grey60")

    output <-  list(stable = equilibria, unstable = output_unstable)
  return(output)

}









