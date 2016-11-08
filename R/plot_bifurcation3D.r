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
#' library(rgl)
#' p <- set_parms(livestock$defparms, set = list(b = 0.9, c = 0.2, f = 0, p = 0, alpha = 0.2))
#' plot_bifurcation3D(livestock, parms = p, res = 31)
#'
plot_bifurcation3D <- function(
                      model,
                      parms = model$parms,
                      over = "b",
                      xrange = c(0,1),
                      res = 21,
                      ini = c(0.9, 0.0001),
                      t_max = 150,
                      method = "ode45",
                      colors = c("#000000","#009933"),
                      new = FALSE
                    ) {

  plot3d(NA,NA,NA,
         xlim = c(0,1), ylim = c(0,1), zlim = c(0,1),
         xlab = "pressure", ylab = "vegetation cover", zlab = "local vegetation cover",
         type = "n", box = TRUE)
  rgl.bg(fogtype = "exp2", color = "white")


  equilibria <- sim_bifurcations(model, over = over, xrange = xrange, ini = ini, t_max = t_max, res = res, parms = parms, method = method)

  rgl.points(equilibria[,over],
             equilibria$rho_1,
             q_11(ini_rho(equilibria$rho_1, equilibria$rho_11)), color = "black", size = 8)

  parms[[over]] <- seq(xrange[1],xrange[2],length = res)
  parms$rho_ini <- ini

  iterations <- expand.grid(parms)
  iterations <- cbind(ID = 1:dim(iterations)[1],iterations)

  iterations$b <- as.numeric(as.character(iterations$b))
  iterations$L <- as.numeric(as.character(iterations$L))

  foreach(iteration = iterations$ID, .packages = c("deSolve", "livestock", "foreach")) %dopar% {

    sim_trajectories(model, parms = iterations[iteration,],
                              rho_1_ini = seq(0,0.99, length = 11),
                              times = c(0,150))

    } -> trajectories


  for(iteration in seq(iterations$ID[1], tail(iterations$ID,1),2)) {

    sapply(trajectories[[iteration]], function(x){
      rgl.linestrips(rep(iterations[iteration,]$b, times = length(x$rho_1)),
                     x$rho_1,
                     q_11(x$rho_1, x$rho_11),
                     col = "black")

    }
    )

  }




}









