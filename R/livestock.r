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

d_rho_mean <- function(rho, parms) {
  growth <- with(parms, r *  rho[1]^( 1 + alpha) * (b + (1-b) * f * rho[1])  * (1 - rho[1]/(K * (1-c*rho[1]) ) ))
  if(growth <= 0| is.na(growth)) {growth <- 0}

  mortality <- with(parms,  m * rho[1] + ( (a+v*rho[1]) * (1-p*rho[1]) * rho[1]^( 1 + q) * L )/(1 + (a+v*rho[1]) * (1-p*rho[1]) * h * rho[1]^( 1 + q)) )
  if(mortality <= 0 | is.na(mortality)) {mortality <- 0}

  return(growth - mortality)

}

#' @export

livestock <- list(
  template = ini_rho(rho_1 = 0.9999),
  parms = list(
    r = 1,
    b = 1,
    K = 1,
    alpha = 0,
    c = 0,
    f = 0,
    m = 0.05,
    a = 5,
    h = 10,
    L = 0.5,
    q = 0,
    v = 0,
    p = 0
  ),
  pair = function(t, rho, parms = model_parms) {
    if(rho[1] < 1e-6) {
      rho <- ini_rho(0,0)
      out <- list(c(
        rho_1 = 0,
        rho_11 = 0
      )  )
    } else {
      rho_1 <- d_rho_1(rho, parms)
      rho_11 <- d_rho_11(rho, parms)
      out <- list(c(
        rho_1 = rho_1,
        rho_11 = rho_11
      )  )
    }
    return(out)
  },

  meanfield = function(t, rho, parms = model_parms) {

    delta <- d_rho_mean(rho, parms )

    list(c(
          rho_1 = delta,
         rho_11 = delta
         ))
  }

)
