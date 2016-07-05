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
#' p <- set_parms(livestock$parms, set = list(b = 0.3, f= 0.5))
#' plot_pairapproximation3D(livestock, parms = p)

plot_pairapproximation3D <- function(
  model,
  parms = model$parms,
  rho_1_ini = seq(0,1, length = 21),
  rho_11_ini = seq(0,1, length = 11),
  times = c(0,1000),
  method = "ode45",
  colors = c("#000000","#009933"),
  meanfield = FALSE,
  ...
) {

  # open new base plot if none exists
  #rgl.open()

  plot3d(NA,NA,NA,
         xlim = c(0,1), ylim = c(0,1), zlim = c(0,0.25),
         xlab = "vegetation cover", ylab = "local vegetation cover", zlab = "growth/mortality",
         type = "n", box = TRUE, ...)
  rgl.bg(fogtype = "exp2", fog = TRUE, color = "white")


  # draw trajectories of mortality and growth
  output <- sim_trajectories(model, parms, rho_1_ini = rho_1_ini, times = times, method = method)

  # visualize trajectories to the attractor


  sapply(output, function(x){
    rho <- ini_rho(x$rho_1, x$rho_11)
    rgl.linestrips(rho$rho_1,
                   q_11(rho),
                   limit(rho$rho_1*death(rho, parms)),
                   col = colors[1])

    rgl.linestrips(rho$rho_1,
                   q_11(rho),
                   limit((1-rho$rho_1)*colonization(rho, parms)),
                   col =  colors[2])

  }
  )


  eq <- get_equilibria(model$pair, y = model$template, parms = parms, method = method, t_max = 130)

  # draw points
  rho <- ini_rho(c(eq$lo[1], eq$hi[1]), c(eq$lo[2], eq$hi[2]))
  rgl.spheres(
    rho[[1]],
    q_11(rho),
    limit(rho[[1]]*death(rho, parms)),
    radius = 0.01,
    xpd = TRUE, pch = 20, cex = 2)

  if(meanfield == TRUE) {

    # plot meanfield
    rho_1 <- seq(0,1,length = 100)
    rgl.linestrips(rho_1,
                   rho_1,
                   limit(rho_1*death(ini_rho(rho_1), parms)),
                   col = "black", lwd =2)

    rgl.linestrips(rho_1,
                   rho_1,
                   limit((1-rho_1)*colonization(ini_rho(rho_1), parms)),
                   col = "#009933", lwd = 2)
  }
}





