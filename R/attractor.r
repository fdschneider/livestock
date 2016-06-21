#' Title
#'
#' @param model_parms
#' @param rho_1_ini
#' @param rho_11_ini
#' @param meanfield
#' @param pairapprox
#' @param localvals
#' @param ...
#'
#' @return
#' @import deSolve
#' @export
#'
#' @example
#'
#' attractor(livestock)
#'

attractor <- function(model, parms = model$defparms, rho_1_ini = seq(0,1, length = 21), rho_11_ini = seq(0,1, length = 11), meanfield = TRUE, pairapprox = FALSE, localvals = FALSE, colpal = list(mort = c("#000000","#00000040","#00000030","#00000020"), grow = c("#009933","#00993340","#00993330","#00993320") ), ...) {

  plot(NA,NA, ylab = "plant mortality/growth", xlim = c(0,1), ylim= c(0,.25), bty = "n", xaxs = "i" , yaxs = "i", ...)

  rho <- seq(0,1,length = 100)

  if(meanfield) {

    lines(rho,mortality(rho, rho, parms), col = colpal$mort[1], lwd = 2)
    lines(rho,growth(rho, rho, parms), col = colpal$grow[1], lwd = 2)


    runmodel_high <- as.data.frame(ode(y = 0.99, func = model$odesys, times = c(0,1000), parms = parms, method = "ode45"))

    points(runmodel_high[2,2],growth(runmodel_high[2,2], runmodel_high[2,2], parms), xpd = TRUE, pch = 20, cex = 2)

    runmodel_low <- as.data.frame(ode(y = 0.0001, func = model$odesys, times = c(0,1000), parms = parms, method = "ode45"))

    points(runmodel_low[2,2],growth(runmodel_low[2,2],runmodel_low[2,2],parms), xpd = TRUE, pch = 20, cex = 2)

    lo <- runmodel_low[2,2]
    hi <- runmodel_high[2,2]

    for(i in 1:10) {

      mid <- (lo+hi)/2
      runmodel_mid <- as.data.frame(ode(mid, func = model$odesys, times = c(0,0.2), parms = parms, method = "ode45"))

      if(runmodel_mid[2,2] > mid) { hi <- mid } else { lo <- mid}

    }


    points(mid,growth(mid,mid,parms), xpd = TRUE, pch = 21, cex = 1.5, bg = "white")


  }

  if(pairapprox) {

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
      temp <- ini_rho(ini[x,]$rho_1, ini[x,]$rho_11)

      #if(ini[x,]$rho_11 == 0) {
      while(any(is.na(temp)) & ini[x,]$rho_11 <=ini[x,]$rho_1) {
        ini[x,]$rho_11 <-  ini[x,]$rho_11+0.005
        temp <- ini_rho(ini[x,]$rho_1, ini[x,]$rho_11)
      }

      ini[x,]$rho_1 <- temp[1]
      ini[x,]$rho_11 <- temp[2]
      ini[x,]$rho_10 <- temp[3]
      ini[x,]$rho_00 <- temp[4]
      ini[x,]$rho_0 <- temp[5]
    }


    ini$m_ini <- C(ini$rho_1, ini$rho_11/ini$rho_1, model_parms)
    ini$g_ini <- G(ini$rho_1, ini$rho_10/1-ini$rho_0, model_parms)

    ini <- subset(ini, !is.na(m_ini) & !is.na(ini$g_ini) &  ini$g_ini >= 0)
    ini <- cbind(ID = 1:nrow(ini),ini)


    foreach(iteration = ini$ID, .packages = c("deSolve")) %dopar% {

      rho_starting <- ini[iteration, 2:6]

      # running the ode-solver
      runmodel <- runODE_spex(as.numeric(rho_starting),  model_parms, times = 1.1^seq(0,30,1))

      return(runmodel)
    } -> output

    steady <-  as.data.frame(t(sapply(output, function(x){

      lines(x$rho_1, C(x$rho_1, x$rho_11/x$rho_1, model_parms))
      arrows(tail(x$rho_1,2)[1],tail(C(x$rho_1, x$rho_11/x$rho_1, model_parms),2)[1],tail(x$rho_1,1),tail(C(x$rho_1, x$rho_11/x$rho_1, model_parms),1), length = 0.04 )
      lines(x$rho_1, G(x$rho_1, x$rho_10/x$rho_0, model_parms), col = "#009933")
      arrows(tail(x$rho_1,2)[1],tail(G(x$rho_1, x$rho_10/x$rho_0, model_parms),2)[1],tail(x$rho_1,1),tail(G(x$rho_1, x$rho_10/x$rho_0, model_parms),1), length = 0.04 , col = "#009933")

      return(c(rho_1 = tail(x$rho_1,1), G =  tail(G(x$rho_1, x$rho_10/x$rho_0, model_parms),1)  , C = tail(C(x$rho_1, x$rho_11/x$rho_1, model_parms),1)) )
    }
    ) )
    )

    ## plot steady states

    high_equ <- as.data.frame(ode(y = ini_rho(0.9), func = odesys_spex, times = c(0,1000), parms = model_parms, method = "ode45") )[2,]

    points(high_equ$rho_1, C(high_equ$rho_1, high_equ$rho_11/high_equ$rho_1, model_parms), pch = 20,cex = 2, xpd = TRUE)

    low_equ <- as.data.frame(ode(y = ini_rho(0.0001), func = odesys_spex, times = c(0,1000), parms = model_parms, method = "ode45") )[2,]

    points(low_equ$rho_1, C(low_equ$rho_1,low_equ$rho_11/low_equ$rho_1, model_parms), pch = 20, cex = 2, xpd = TRUE)

  }

  if(localvals) {


    lines(rho,C(rho, 1, model_parms), col= colpal$mort[3])
    lines(rho,C(rho, 0.75, model_parms), col= colpal$mort[3])
    lines(rho,C(rho, 0.5, model_parms), col= colpal$mort[2])
    lines(rho,C(rho, 0.25, model_parms), col= colpal$mort[3])
    lines(rho,C(rho, 0, model_parms), col= colpal$mort[4])

    lines(rho, G(rho, 1, model_parms), col = colpal$grow[4])
    lines(rho,G(rho, 0.75, model_parms), col = colpal$grow[3])
    lines(rho,G(rho, 0.5, model_parms), col = colpal$grow[2])
    lines(rho,G(rho, 0.25, model_parms), col = colpal$grow[3])
    lines(rho,G(rho, 0, model_parms), col = colpal$grow[4])

  }


}
