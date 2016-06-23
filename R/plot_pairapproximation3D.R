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
#' @import rgl
#' @export
#' @examples
#'
#' p <- set_parms(livestock$defparms, set = list(b = 0.1, f = 0.7, p = 0.8))
#' plot_pairapproximation3D(livestock, parms = p)

plot_pairapproximation3D <- function(
  model,
  parms = model$defparms,
  rho_1_ini = seq(0,1, length = 11),
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
  ini <- list(
    rho_1 = rho_1_ini,
    rho_11 = rho_11_ini,
    rho_10 = NA,
    rho_00 = NA,
    rho_0 = NA

  )

  ini <- expand.grid(ini)


  #ini$rho_11[is.na(ini$rho_11)] <- ini$rho_1[is.na(ini$rho_11)]


  for(x in 1:nrow(ini)) {
    temp <- unlist(expand_rho(c(ini[[x,1]], ini[[x,2]])))

    #if(ini[x,]$rho_11 == 0) {
    #while(any(is.na(temp)) & ini[[x,2]] <= ini[[x,1]]) {
    #  ini[[x,2]] <-  ini[[x,2]]+0.005
    #  temp <- expand_rho(c(ini[[x,1]], ini[[x,2]]))
    #}

    ini[x,] <- temp
  }

  ini <- subset(ini, !is.na(rho_1))

  ini$m_ini <- mortality(ini$rho_1, ini$rho_11/ini$rho_1, parms)
  ini$g_ini <- growth(ini$rho_1, ini$rho_10/ini$rho_0, parms)

  ini <- subset(ini, !is.na(m_ini) & !is.na(ini$g_ini) &  ini$g_ini >= 0)
  ini <- cbind(ID = 1:nrow(ini),ini)

  foreach(iteration = ini$ID, .packages = c("deSolve")) %dopar% {

    rho_starting <- unlist(ini[iteration, 2:3])

    # running the ode-solver
    runmodel <- run_ode(rho_starting, func = model$pair, times = seq(0,150,length = 300), parms = parms, method = method)

    return(as.data.frame(runmodel))
  } -> output

  sapply(output, function(x){
    rgl.linestrips(x$rho_1,
                   mortality(x$rho_1, q_11(x$rho_1, x$rho_1), parms),
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





