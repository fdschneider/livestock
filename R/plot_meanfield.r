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
  func = model$meanfield,
  parms = model$parms,
  times = c(0,1000),
  method = "ode45",
  rho_x = seq(0,1,length = 100),
  col = c("#000000","#009933"),
  new = FALSE
  ) {

    # open new base plot if none exists
    if(dev.cur() == 1 | new == TRUE) plot_base()

    # draw functions of mortality and growth
    lines(rho_x, mortality(ini_rho(rho_x), parms), col = col[1], lwd = 2)
    lines(rho_x, growth(ini_rho(rho_x), parms), col = col[2], lwd = 2)

    eq <- get_equilibria(y = model$template, func = func, parms = parms)

    # draw points
    points(c(eq$lo[1],eq$hi[1]), growth(ini_rho(c(eq$lo[1],eq$hi[1]), c(eq$lo[1],eq$hi[1])), parms), xpd = TRUE, pch = 20, cex = 2)
    points(eq$mid[1],growth(ini_rho(eq$mid[1],eq$mid[1]),parms), xpd = TRUE, pch = 21, cex = 1.5, bg = "white")


}





