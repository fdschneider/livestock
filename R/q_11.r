q_11 <- function(rho_1, rho_11, zlim = c(0,1)) {
  out <- rho_11/rho_1
  out[out < zlim[1] | is.na(out)] <- 0
  out[out > zlim[2]] <-  zlim[2]
  return(out)

}

q_01 <-  function(rho_1, rho_11, zlim = c(0,1)) {
  out <- (rho_1-rho_11)/(1-rho_1)
  out[out < zlim[1] | is.na(out)] <- 0
  out[out > zlim[2]] <-  zlim[2]
  return(out)

}
