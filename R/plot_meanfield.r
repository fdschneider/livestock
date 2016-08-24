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
#' @export
#' @examples
#'
#' plot_meanfield(livestock)
#'
plot_meanfield <- function(
  model,
  parms = model$defparms,
  times = c(0,1000),
  method = "ode45",
  rho = seq(0,1,length = 100),
  colors = c("#000000","#009933"),
  new = FALSE
  ) {

    # open new base plot if none exists
    if(dev.cur() == 1 | new == TRUE) plot_base()

    # draw functions of mortality and growth
    lines(rho, mortality(ini_rho(rho), parms), col = colors[1], lwd = 2)
    lines(rho, growth(ini_rho(rho), parms), col = colors[2], lwd = 2)

    eq <- get_equilibria(y = model$template, func = model$meanfield, parms = parms)

    # draw points
    points(c(eq$lo[1],eq$hi[1]), growth(c(eq$lo[1],eq$hi[1]), c(eq$lo[1],eq$hi[1]), parms), xpd = TRUE, pch = 20, cex = 2)
    points(eq$mid[1],growth(eq$mid[1],eq$mid[1],parms), xpd = TRUE, pch = 21, cex = 1.5, bg = "white")


}





