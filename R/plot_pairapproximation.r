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
#' p <- set_parms(livestock$defparms, set = list(b = 0.2, f = 0.9, p = 0.8))
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
  ini <- list(
    rho_1 = rho_1_ini,
    rho_11 = c(0, NA),
    rho_10 = NA,
    rho_00 = NA,
    rho_0 = NA

  )

  ini <- expand.grid(ini)


  ini$rho_11[is.na(ini$rho_11)] <- ini$rho_1[is.na(ini$rho_11)]


  for(x in 1:nrow(ini)) {
    temp <- unlist(expand_rho(c(ini[[x,1]], ini[[x,2]])))

    #if(ini[x,]$rho_11 == 0) {
    while(any(is.na(temp)) & ini[[x,2]] <= ini[[x,1]]) {
      ini[[x,2]] <-  ini[[x,2]]+0.005
      temp <- expand_rho(c(ini[[x,1]], ini[[x,2]]))
    }

    ini[x,] <- temp
  }


  ini$m_ini <- mortality(ini$rho_1, ini$rho_11/ini$rho_1, parms)
  ini$g_ini <- growth(ini$rho_1, ini$rho_10/ini$rho_0, parms)

  ini <- subset(ini, !is.na(m_ini) & !is.na(ini$g_ini) &  ini$g_ini >= 0)
  ini <- cbind(ID = 1:nrow(ini),ini)


  foreach(iteration = ini$ID, .packages = c("deSolve")) %dopar% {

    rho_starting <- unlist(ini[iteration, 2:3])

    # running the ode-solver
    runmodel <- deSolve::ode(rho_starting, func = model$pair, times = 1.05^seq(0,100,1), parms = parms, method = method)

    return(as.data.frame(runmodel))
  } -> output

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





