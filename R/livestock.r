
#' @title Livestock resilience model
#'
#' @description A model of resilience in arid rangelands.
#' @usage
#' library(livestock)
#'
#'
#' library(caspr)
#' ca(l, livestock, parms = p)
#'
#' @param b environmental quality
#' @param m intrinsic plant mortality, i.e. inverse of av. lifespan
#' @param r max. regeneration rate of plants
#' @param K carrying capacity of the landscape (i.e. global competition)
#' @param f local facilitation
#' @param c local competition
#' @param L livestock density
#' @param a attack rate of livestock
#' @param h handling time of livestock
#' @param q hill-exponent of livestock
#' @param p associational resistance against livestock grazing
#' @param v attractant-decoy effect of plants to livestock
#'
#' @author F.D. Schneider
#' @family models
#' @details
#' An unpublished, generalized model of positive and negative local feedbacks in arid rangelands, including mechanisms such as local facilitation, competition, associational resistance and attractant-decoy.
#'
#' @export

"livestock"

livestock <- list(
  name = "Livestock resilience model",
  ref = NA , # a bibliographic reference,
  states = c("1", "0"),
  cols = c("black", "white"),
  template = ini_rho(rho_1 = 0.9999),
  parms = list(
    r = 1,
    b = 1,
    K = 1,
    alpha = 0,
    c = 0,
    f = 0,
    m = 0.05,
    a = 10,
    h = 20,
    L = 1,
    q = 0,
    v = 0,
    p = 0
  ),
  pair = function(t, rho, parms = model_parms) {

    # insert equations here
    delta_1 = (1-rho[[1]]) * colonization(rho, parms) - rho[[1]] * death(rho, parms)
    delta_11 = 2 * (rho[[1]]-rho[[2]]) * colonization(rho, parms) - 2 * rho[[2]] * death(rho, parms)

    if(rho[[1]] < 1e-6) {
      out <- list(c(
        rho_1 = 0,
        rho_11 = 0
      )  )
    } else {
      out <- list(c(
        rho_1 = delta_1,
        rho_11 = delta_11
      )  )
    }
    return(out)
  },
  meanfield = function(t, rho, parms = model_parms) {

    rho <- ini_rho(rho[[1]])
    delta_1 <- (1-rho[[1]])*colonization(rho, parms) - rho[[1]]*death(rho, parms)

    if(rho[[1]] < 1e-6) {
      out <- list(c(
        rho_1 = 0,
        rho_11 = 0
      )  )
    } else {
      out <- list(c(
        rho_1 = delta_1,
        rho_11 = delta_1
      )  )
    }
    return(out)
  },
  update = function(x_old, parms, subs = 12) {

    x_new <- x_old

    for(s in 1:subs) {

      # define update procedures depending on parms

      # model specific part:
      # 1 - setting time-step parameters
      rho_one <- sum(x_old$cells == "1")/(x_old$dim[1]*x_old$dim[2]) # get initial vegetation cover
      q_one_one <- neighbors(x_old, "1")/4  # count local density of occupied fields for each cell

      # 2 - drawing random numbers
      rnum <- runif(x_old$dim[1]*x_old$dim[2]) # one random number between 0 and 1 for each cell

      # 3 - setting transition probabilities
      growth <- with(parms, colonization(ini_rho(rho_one, q_11 = q_one_one), parms) *1/subs)  # recolonisation rates of all cells

      growth[growth < 0] <- 0

      death <- with(parms, death(ini_rho(rho_one, q_11 = q_one_one), parms) *1/subs)   # set probability of death for each cell

      death[death < 0] <- 0

      # check for sum of probabilities to be inferior 1 and superior 0
      if(any(c(growth, death) > 1 )) warning(paste("a set probability is exceeding 1 in time step", i, "! decrease delta!!!"))
      #if(any(c(growth, death) < 0)) warning(paste("a set probability falls below 0 in time step", i, "! balance parameters!!!"))

      # 4 - apply transition probabilities

      x_new$cells[which(x_old$cells == "0" & rnum <= growth)] <- "1"
      x_new$cells[which(x_old$cells == "1" & rnum <= death)] <- "0"

      # 5- store x_new as next x_old

      x_old <- x_new

    }

    return(x_new)

  }

)

class(livestock) <- c("ca_model", "list")
