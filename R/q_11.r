#' Transfer cover object into average local cover of vegetated cells, q_11.
#'
#' @param rho
#'
#' @return
#' @export
#'
#' @examples
#'
#' rho <- ini_rho(0.5,0.45)
#' q_11(rho)
#'

q_11 <- function(rho, rho_11 = NULL) {
  if(!is.null(rho_11)) {
    rho <- ini_rho(rho, rho_11)
  } else {
    stop("provide object of class 'cover' or second argument for rho_11!")
  }
  out <- rho[2]/rho[1]
  return(as.vector(out))

}

#q_11 <- function(rho_1, rho_11) {
#  out <- rho_11/rho_1
#  out[out < zlim[1] | is.na(out)] <- 0
#  out[out > zlim[2]] <-  zlim[2]
#  return(as.vector(out))
#
#}

#' Transfer cover object into average local cover of empty cells, q_01.
#'
#' @param rho
#'
#' @return
#' @export
#' @examples
#'
#' rho <- ini_rho(0.5,0.45)
#' q_01(rho)
#'

q_01 <-  function(rho, rho_11 = NULL) {
  if(!class(rho) == "cover" & !is.null(rho_11)) {
    rho <- ini_rho(rho, rho_11)
    } else {
    stop("provide object of class 'cover' or second argument for rho_11!")
  }

  out <- (rho[1]-rho[2])/(1-rho[1])
  return(as.vector(out))
}


#q_01 <-  function(rho_1, rho_11, zlim = c(0,1)) {
#  out <- (rho_1-rho_11)/(1-rho_1)
#  out[out < zlim[1] | is.na(out)] <- 0
#  out[out > zlim[2]] <-  zlim[2]
#  return(as.vector(out))
#
#}
