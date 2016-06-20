#' Title
#'
#' @param rho_1
#' @param rho_11
#' @param cc single numerical value greater than 0. Clustering coefficient,
#'   describing the ratio between local cover and global cover. A value greater 1 denotes strong clustering.
#'
#' @return
#' @export
#'
#' @examples
#'
#' ini_rho(0.9, cc = 1.2)
#'
ini_rho <- function(rho_1, rho_11 = NULL, cc = NULL) {
  if(is.null(rho_11[1]) & is.null(cc[1])) {rho_11 <- rho_1*rho_1}
  if(is.null(rho_11[1]) & !is.null(cc[1])) {rho_11 <- cc*rho_1}

    out <- c(
      rho_1 = rho_1,
      rho_11 = rho_11,
      rho_10 = rho_1-rho_11,
      rho_00 = 1-2*rho_1+rho_11,
      rho_0 = 1-rho_1
    )

    if(any(out < 0)) {
      out <- c(
      rho_1 = NA,
      rho_11 = NA,
      rho_10 = NA,
      rho_00 = NA,
      rho_0 = NA
    )
    warning("The value of requested clustering is not compatible with the requested total vegetation cover: please provide a lower local cover or clustering value.")
      }


  return(out)
}
