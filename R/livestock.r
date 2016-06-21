#' @export

d_rho_1 <- function(rho, parms) {

  growth <- with(parms,
                 r * (b + (1 - b) * f * (rho[1] - rho[2])/(1- rho[1]) ) * rho[1]^( 1 + alpha) * (1 - rho[1]/(K * (1-c*(rho[1] - rho[2])/(1- rho[1])) ) ))
  if(growth <= 0 | is.na(growth)) {growth <- 0}

  mortality <- with(parms, m * rho[1] + ( (a + v*rho[2]/rho[1]) * (1 - p * rho[2]/rho[1]) * rho[1]^( 1 + q) * L)/(1 + (a + v*rho[2]/rho[1]) * (1 - p * rho[2]/rho[1]) * h  * rho[1]^( 1 + q)) )
  if(mortality <= 0 | is.na(mortality)) {mortality <- 0}

  return(growth - mortality)
}


#' @export

d_rho_11 <- function(rho,  parms) {

  growth <- with(parms,
                 2* (rho[1] - rho[2]) * r * (b + (1 - b) * f * (rho[1] - rho[2])/(1- rho[1]) ) * rho[1]^( 1 + alpha) * (1 - rho[1]/(K * (1-c*(rho[1] - rho[2])/(1- rho[1])) ) ) / (1-rho[1]))
  if(growth <= 0 | is.na(growth)) growth <- 0

  mortality <- with(parms, 2 * rho[2] * m  + 2 * rho[2] * ( (a + v*rho[2]/rho[1]) * (1 - p * rho[2]/rho[1])  * rho[1]^( 1 + q) * L )/(1 +(a + v*rho[2]/rho[1]) * (1 - p * rho[2]/rho[1])  * h  * rho[1]^( 1 + q))  )
  if(mortality <= 0 | is.na(mortality)) mortality <- 0

  return(growth - mortality)
}



#' @export

livestock <- list(
  defparms = list(
    r = 1,
    b = 1,
    K = 1,
    alpha = 0,
    c = 0,
    f = 0,
    m = 0.05,
    a = 0.3,
    h = 50,
    L = 5,
    q = 0,
    v = 0,
    p = 0
  ),

  odesys = function(t, rho, parms = model_parms, type = c("pair", "mean", "ca")) {
  if(rho[1] < 1e-7 ) {
    rho <- ini_rho(0,0)
  }

  rho_1 <- d_rho_1(rho, parms)
  rho_11 <- d_rho_11(rho, parms)

  list(c(
    rho_1 = rho_1,
    rho_11 = rho_11#,
    #changes of other pairs and singletons are not calculated
    #rho_10 = 0,
    #rho_00 = 0,
    #rho_0 = 1-rho_1
  )  )
}
)
