#' Title
#'
#' @param parms
#' @param set
#'
#' @return
#' @export
#'
#' @examples
#'
#' p <- set_parms(livestock$defparms, set = list(b = 0.2, f = 0.9))
#'
set_parms <- function(parms, set = list(NA)) {

  parms.names <- names(parms)
  set.names <- names(set)
  m.names <- sort(unique(c(parms.names, set.names)))

  parms_new <- sapply(m.names, function(i) {
    if (i %in% set.names) set[[i]]
    else parms[[i]]
  }, simplify = FALSE)

  return(parms_new)
}
