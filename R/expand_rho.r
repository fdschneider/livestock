#' Title
#'
#' @param rho
#'
#' @return
#' @export
#'
expand_rho <- function(rho) {

  if(length(rho) == 1) { rho[2] <- rho[1]^2 }

  out <- list(rho_1 = rho[1],
       rho_11 = rho[2],
       rho_01 = rho[1]-rho[2],
       rho_00 = 1-2*rho[1]+rho[2],
       rho_0 = 1-rho[1]
       )

  if(any(out < 0)) {
    out <- c(
      rho_1 = NA,
      rho_11 = NA,
      rho_10 = NA,
      rho_00 = NA,
      rho_0 = NA
    )
  }

  return(out)
}
