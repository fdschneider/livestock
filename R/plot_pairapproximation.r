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
#' @export
#' @examples
#'
#' p <- set_parms(livestock$parms, set = list(b = 0.1, f = 0.9, p = 0.99))
#' plot_pairapproximation(livestock, parms = p)

plot_pairapproximation <- function(
  model,
  parms = model$parms,
  rho_1_ini = seq(0,1, length = 11),
  rho_11_ini = seq(0,1, length = 11),
  times = c(0,1000),
  method = "ode45",
  rho = seq(0,1,length = 100),
  colors = c("#000000","#009933"),
  new = FALSE
) {

  # open new base plot if none exists
  if(dev.cur() == 1 | new == TRUE) plot_base()

  # draw trajectories of mortality and growth
  output <- sim_trajectories(model = model, parms = parms, rho_1_ini = rho_1_ini, times = times, method = method)

  # visualize trajectories to the attractor
  sapply(output, function(x){
    rho <- ini_rho(x$rho_1, x$rho_11)
    mort <- limit(rho$rho_1*death(rho, parms))
    lines(rho$rho_1,  mort)
    arrows(tail(rho$rho_1,2)[1],tail(mort,2)[1],tail(rho$rho_1,1),tail(mort,1), length = 0.1 )

    grow <- limit((1-rho$rho_1)*colonization(rho, parms))
    lines(rho$rho_1, grow, col = "#009933")
    arrows(tail(rho$rho_1,2)[1],tail(grow,2)[1],tail(rho$rho_1,1),tail(grow,1), length = 0.1 , col = "#009933")

  }
  )

  eq <- get_equilibria(model$pair, y = model$template, parms = parms, method = method, t_max = 130)
  rho <- ini_rho(c(eq$lo[1],eq$hi[1]),c(eq$lo[2],eq$hi[2]))
  # draw points
  points(rho$rho_1, rho$rho_1*death(rho, parms), xpd = TRUE, pch = 20, cex = 2)
  #points(eq$mid[1],growth(eq$mid[1],eq$mid[1],parms), xpd = TRUE, pch = 21, cex = 1.5, bg = "white")


}





