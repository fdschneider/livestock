#' Title
#'
#' @param rho_1 A single numerical value between 0 and 1. Total vegetation cover, i.e. the probability of a cell to be vegetated.
#' @param rho_11 A single numerical value between 0 and 1. Average local vegetation cover of vegetated cells, i.e.  the conditional probability of finding vegetated cells in the 4-cell neighborhood given that the focal cell is vegetated. It defaults to NULL which leads to an assumption of neutral clustering, i.e. \code{rho_11 == rho_1}.
#' @param cc A single numerical value greater than 0. Clustering coefficient,
#'   describing the ratio between average local cover \code{rho_11}, and global cover \code{rho_1}. A value greater 1 denotes strong clustering. It defaults to NULL which leads to an assumption of neutral clustering, i.e. \code{rho_11 == rho_1}.
#'
#' @return A named vector of
#' @export
#'
#' @examples
#'
#' ini_rho(0.9, cc = 1.2)
#'
#'

ini_rho <- function(rho_1, rho_11 = NULL, cc = NULL) {
  if(is.null(rho_11[1]) & is.null(cc[1])) {rho_11 <- rho_1^2}
  if(is.null(rho_11[1]) & !is.null(cc[1])) {rho_11 <- cc*rho_1^2}

    out <- c(
      rho_1 = rho_1,
      rho_11 = rho_11#,
      #rho_10 = rho_1-rho_11,
      #rho_00 = 1-2*rho_1+rho_11,
      #rho_0 = 1-rho_1
    )

    if(any(out < 0)) {
      out <- c(
      rho_1 = NA,
      rho_11 = NA#,
      #rho_10 = NA,
      #rho_00 = NA,
      #rho_0 = NA
    )
    warning("The value of requested clustering is not compatible with the requested total vegetation cover: please provide a lower local cover or clustering value.")
      }


  return(out)
}
