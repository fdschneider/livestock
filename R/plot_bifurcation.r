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
plot_bifurcation <- function(
                      model,
                      parms = model$defparms,
                      over = "b",
                      xrange = c(0,1),
                      res = 11,
                      ini = c(0.9, 0.0001),
                      t_max = 150,
                      method = "ode45",
                      colors = c("#000000","#009933"),
                      new = FALSE
                    ) {


  parms[[over]] <- seq(xrange[1],xrange[2],length = res)
  parms$rho_ini <- ini

  iterations <- expand.grid(parms)
  iterations <- cbind(ID = 1:dim(iterations)[1],iterations)

  iterations$b <- as.numeric(as.character(iterations$b))
  iterations$L <- as.numeric(as.character(iterations$L))

  foreach(iteration = iterations$ID, .packages = c("deSolve"), .combine = rbind) %dopar% {

    model_parms <- as.list(iterations[iteration,])

    rho_starting <- ini_rho(model_parms$rho_ini)

    # running the ode-solver
    runmodel <- deSolve::ode(rho_starting, func = model$pair, times = c(0,t_max), parms = model_parms, method = method)

    return(tail(runmodel,1))
  } -> output

  output <- cbind(iterations,output)

  upper <- output[output$rho_ini == ini[1],][which(round(output[output$rho_ini == ini[1],]$rho_1,4) != round(output[output$rho_ini == ini[2],]$rho_1,4)),]
  lower <- output[output$rho_ini == ini[2],][which(round(output[output$rho_ini == ini[2],]$rho_1,4) != round(output[output$rho_ini == ini[1],]$rho_1,4)),]

  if(nrow(upper)>0) {
    foreach(i = upper[,over], .combine = rbind, .packages = c("deSolve") ) %dopar% {

      model_parms <- upper[ upper[, over] == i,]

      hi_1 <- upper[upper[, over] == i,]$rho_1
      lo_1 <- lower[lower[, over] == i,]$rho_1
      hi_11 <- upper[upper[, over] == i,]$rho_11
      lo_11 <- lower[lower[, over] == i,]$rho_11

      for(j in 1:10) {

        rho_ini <- ini_rho( (hi_1+lo_1)/2 , (hi_11+lo_11)/2 )

        # running the ode-solver

        runmodel <- deSolve::ode(rho_ini, func = model$pair, times = c(0,1.5), parms = model_parms, method = method)

        if(runmodel[2,"rho_1"] < runmodel[1,"rho_1"] ) {
          lo_1 <- (hi_1+lo_1)/2
          lo_11 <- (hi_11+lo_11)/2
        } else {
          hi_1 <- (hi_1+lo_1)/2
          hi_11 <- (hi_11+lo_11)/2
        }

      }

      return(tail(runmodel,1))
    } -> output_unstable
  }

    output_unstable <- cbind(upper[,1:16],output_unstable)


  return(output)

}









