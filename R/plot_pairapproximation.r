#' Draw meanfield model attractor
#'
#' @param model
#' @param times
#' @param parms
#' @param method
#' @param rho
#' @param colors
#'
#' @import foreach
#'
#' @return
#' @export
#' @examples
#'
#' p <- set_parms(livestock$parms, set = list(b = 0.1, f = 0.5, p = 0.9, L = 1.5))
#' par(mfrow = c(1,2))
#' plot_pairapproximation(livestock, parms = p) -> out
#' plot_pairapproximation(out, parms = p, side = "plain")

plot_pairapproximation <- function(
  model,
  parms = model$parms,
  side = "rho",
  rho_1_ini = seq(0,1, length = 11),
  rho_11_ini = seq(0,1, length = 11),
  times = c(0,1000),
  method = "ode45",
  rho = seq(0,1,length = 100),
  colors = c("#000000","#009933"),
  #fog = TRUE,
  new = TRUE,
  ...
) {

  #par(mfrow = c(1,3))
  # open new base plot if none exists
  if(dev.cur() == 1 | new == TRUE) plot_base(ylim = switch(side, plain = c(0,1), c(0,0.25) ),
                                             ylab = switch(side, plain = "local cover", "plant mortality/growth" ),
                                             xlab = switch(side, q = "local cover","vegetation cover"), ...)

  # draw trajectories of mortality and growth
  if(class(model) == "attractor") {
    trajectories <- model$trajectories
  } else {
    trajectories <- sim_trajectories(model = model, parms = parms, rho_1_ini = rho_1_ini, times = times, method = method)
  }

  # visualize trajectories to the attractor
  sapply(trajectories, function(x){
    rho <- ini_rho(x$rho_1, x$rho_11)
    mort <- limit(mortality(rho, parms))
    grow <- limit(growth(rho, parms))
    q_11_vec <- q_11(rho)
    #fog_m <- highlight(q_11_vec, colrange = c(paste0(colors[1],"88"),colors[1]))
    #fog_g <- highlight(q_11_vec, colrange = c(paste0(colors[2],"88"),colors[2]))


    switch(side,
        rho = {
          lines(rho$rho_1,  mort)
          #arrows(tail(rho$rho_1,2)[1],tail(mort,2)[1],tail(rho$rho_1,1),tail(mort,1), length = 0.1 )
          lines(rho$rho_1, grow, col = "#009933")
          #arrows(tail(rho$rho_1,2)[1],tail(grow,2)[1],tail(rho$rho_1,1),tail(grow,1), length = 0.1 , col = "#009933")

          },
        q = {
          lines(q_11_vec,  mort)
          #arrows(tail(q_11_vec,2)[1],tail(mort,2)[1],tail(q_11_vec,1),tail(mort,1), length = 0.1 )
          lines(q_11_vec, grow, col = "#009933")
          #arrows(tail(q_11_vec,2)[1],tail(grow,2)[1],tail(q_11_vec,1),tail(grow,1), length = 0.1 , col = "#009933")

        },
        plain = {
          lines(rho$rho_1,  q_11_vec)
          #arrows(tail(rho$rho_1,2)[1],tail(q_11_vec,2)[1],tail(rho$rho_1,1),tail(q_11_vec,1), length = 0.1 )
          lines(rho$rho_1+0.002, q_11_vec+0.002, col = "#009933")
          #arrows(tail(rho$rho_1,2)[1],tail(q_11_vec,2)[1],tail(rho$rho_1,1),tail(q_11_vec,1), length = 0.1 , col = "#009933")

        }

    )


  }
  )


  if(class(model) == "attractor") {
    eq <- model$eq
  } else {
    eq <- get_equilibria(y = model$template, func = model$pair, parms = parms, method = method, t_max = 130)
  }

    rho_steady <- ini_rho(c(eq$lo[1],eq$hi[1]),c(eq$lo[2],eq$hi[2]))
  q_steady <- q_11(rho_steady)
  switch(side,
         rho = {
           points(rho_steady$rho_1, mortality(rho_steady, parms = parms), xpd = TRUE, pch = 20, cex = 2)

         },
         q = {
           points(q_steady, mortality(rho_steady, parms = parms), xpd = TRUE, pch = 20, cex = 2)

         },
         plain = {
           points(rho_steady$rho_1, q_steady, xpd = TRUE, pch = 20, cex = 2)

         }

  )


  output <- list(trajectories = trajectories,
                 eq = eq
                 )
  class(output) <- "attractor"
  return(output)

}





