#' Draw meanfield model attractor
#'
#' @param model
#' @param times
#' @param parms
#' @param method
#' @param rho
#' @param colors
#'
#' @return
#' @import rgl
#' @export
#' @examples
#'
#' p <- set_parms(livestock$defparms, set = list(b = 0.2, c = 0.2, f = 0.9, p = 0.99))
#' plot_pairapproximation3D(livestock, parms = p)

plot_pairapproximation3D <- function(
  model,
  parms = model$defparms,
  rho_1_ini = seq(0,1, length = 21),
  rho_11_ini = seq(0,1, length = 11),
  times = c(0,1000),
  method = "ode45",
  colors = c("#000000","#009933")
) {

  # open new base plot if none exists
  #rgl.open()

  plot3d(NA,NA,NA,
         xlim = c(0,1), ylim = c(0,0.25), zlim = c(0,1),
         xlab = "vegetation cover", ylab = "growth/mortality", zlab = "local vegetation cover",
         type = "n", box = TRUE)
  rgl.bg(fogtype = "exp2", color = "white")


  # draw trajectories of mortality and growth
  output <- sim_trajectories(model, parms, rho_1_ini = rho_1_ini, times = times, method = method)

  # visualize trajectories to the attractor

  sapply(output, function(x){
    rgl.linestrips(x$rho_1,
                   mortality(x$rho_1, q_11(x$rho_1, x$rho_11), parms),
                   q_11(x$rho_1, x$rho_11),
                   col = "black")
    #arrows(tail(x$rho_1,2)[1],tail(mortality(x$rho_1, x$rho_11/x$rho_1, parms),2)[1],tail(x$rho_1,1),tail(mortality(x$rho_1, x$rho_11/x$rho_1, parms),1), length = 0.1 )

    rgl.linestrips(x$rho_1,
                   growth(x$rho_1, q_01(x$rho_1, x$rho_11), parms),
                   q_11(x$rho_1, x$rho_11),
                   col = "#009933")
    #arrows(tail(x$rho_1,2)[1],tail(growth(x$rho_1, (x$rho_1-x$rho_11)/(1-x$rho_1), parms),2)[1],tail(x$rho_1,1),tail(growth(x$rho_1, (x$rho_1-x$rho_11)/(1-x$rho_1), parms),1), length = 0.1 , col = "#009933")

  }
  )


  eq <- get_equilibria(model$pair, y = model$template, parms = parms, method = method, t_max = 130)

  # draw points
  rgl.spheres(
    c(eq$lo[1],eq$hi[1]),
    growth(c(eq$lo[1],eq$hi[1]), c(q_01(eq$lo[1], eq$lo[2]), q_01(eq$hi[1], eq$hi[2])), parms),
    c(q_11(eq$lo[1], eq$lo[2]), q_11(eq$hi[1], eq$hi[2])),
    radius = 0.01,
    xpd = TRUE, pch = 20, cex = 2)
  #points(eq$mid[1],growth(eq$mid[1],eq$mid[1],parms), xpd = TRUE, pch = 21, cex = 1.5, bg = "white")

}





