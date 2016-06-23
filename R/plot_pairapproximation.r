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
#' @import deSolve
#' @import foreach
#' @export
#' @examples
#'
#' p <- set_parms(livestock$defparms, set = list(b = 0.1, f = 0.9, p = 0.99))
#' plot_pairapproximation(livestock, parms = p, new = TRUE)

plot_pairapproximation <- function(
  model,
  parms = model$defparms,
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
  output <- sim_attractor(model, parms, rho_1_ini = rho_1_ini, times = times, method = method)

  # visualize trajectories to the attractor
  sapply(output, function(x){

    lines(x$rho_1, mortality(x$rho_1, x$rho_11/x$rho_1, parms))
    arrows(tail(x$rho_1,2)[1],tail(mortality(x$rho_1, x$rho_11/x$rho_1, parms),2)[1],tail(x$rho_1,1),tail(mortality(x$rho_1, x$rho_11/x$rho_1, parms),1), length = 0.1 )

    lines(x$rho_1, growth(x$rho_1, (x$rho_1-x$rho_11)/(1-x$rho_1), parms), col = "#009933")
    arrows(tail(x$rho_1,2)[1],tail(growth(x$rho_1, (x$rho_1-x$rho_11)/(1-x$rho_1), parms),2)[1],tail(x$rho_1,1),tail(growth(x$rho_1, (x$rho_1-x$rho_11)/(1-x$rho_1), parms),1), length = 0.1 , col = "#009933")

    #return(c(rho_1 = tail(x$rho_1,1), G =  tail(growth(x$rho_1, x$rho_10/x$rho_0, parms),1)  , C = tail(mortality(x$rho_1, x$rho_11/x$rho_1, parms),1)) )
  }
  )

  eq <- get_equilibria(model$pair, y = model$template, parms = parms, method = method, t_max = 130)

  # draw points
  points(c(eq$lo[1],eq$hi[1]), growth(c(eq$lo[1],eq$hi[1]), (c(eq$lo[1],eq$hi[1])-c(eq$lo[2],eq$hi[2]))/(1-c(eq$lo[1],eq$hi[1])), parms), xpd = TRUE, pch = 20, cex = 2)
  #points(eq$mid[1],growth(eq$mid[1],eq$mid[1],parms), xpd = TRUE, pch = 21, cex = 1.5, bg = "white")


}





