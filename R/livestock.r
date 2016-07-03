
#' @export

d_rho_1 <- function(rho, parms) {

  growth <- with(parms,
                 r * (b + (1 - b) * f * q_01(rho[1],rho[2]) ) * rho[1]^( 1 + alpha) * (1 - rho[1]/(K * (1-c*q_01(rho[1],rho[2])) ) ))
  if(growth <= 0 | is.na(growth)) {growth <- 0}

  mortality <- with(parms, m * rho[1] + ( (a + v*q_11(rho[1],rho[2])) * (1 - p * q_11(rho[1],rho[2])) * rho[1]^( 1 + q) * L)/(1 + (a + v*rho[2]/rho[1]) * (1 - p * q_11(rho[1],rho[2])) * h  * rho[1]^( 1 + q)) )
  if(mortality <= 0 | is.na(mortality)) {mortality <- 0}

  return(growth - mortality)
}


#' @export

d_rho_11 <- function(rho,  parms) {

  growth <- with(parms,
                 2* (rho[1] - rho[2]) * r * (b + (1 - b) * f * q_01(rho[1],rho[2]) ) * rho[1]^( 1 + alpha) * (1 - rho[1]/(K * (1-c*q_01(rho[1],rho[2])) ) ) / (1-rho[1]))
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
  },
  update = function(x_old, parms, subs = 12) {

    x_new <- x_old

    if(length(parms$b) > 1) {
      climate <- parms$b[i]
    } else {
      climate <- parms$b
    }

    if(parms$sigma > 0)  climate <- climate * abs(rnorm(1, 1, parms$sigma))

    for(s in 1:subs) {

      # define update procedures depending on parms

      # model specific part:
      # 1 - setting time-step parameters
      rho_one <- sum(x_old$cells == "1")/(x_old$dim[1]*x_old$dim[2]) # get initial vegetation cover
      q_one_one <- neighbors(x_old, "1")/4  # count local density of occupied fields for each cell

      # 2 - drawing random numbers
      rnum <- runif(x_old$dim[1]*x_old$dim[2]) # one random number between 0 and 1 for each cell

      # 3 - setting transition probabilities
      growth <- with(parms, (r * (climate + (1-climate)*f*q_one_one) * rho_one^(1 + alpha) * ( 1 - (rho_one / (K * (1-c*q_one_one) ))) / (1 - rho_one))  *1/subs)  # recolonisation rates of all cells

      growth[growth < 0] <- 0

      death <- with(parms,       (m + ( (a+ v*q_one_one) * (1-p*q_one_one) * L * rho_one^(1+q) )/( 1 + (a+ v*q_one_one) * (1-p*q_one_one) * h * rho_one^(1+q) )) *1/subs)   # set probability of death for each cell

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
