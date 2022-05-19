#' Forest Growth Model
#' 
#' This function computes forest growth in kg of carbon.
#' 
#' @param C size of forest (units: kg Carbon)
#' @param parameters list of parameters, including: r, g, K, cct
#' @param parameters$r early exponential growth rate (units: kg C/year)
#' @param parameters$g linear growth rate once canopy closure has been reached (units: kg C/year)
#' @param parameters$K carrying capacity (units: kg Carbon)
#' @param parameters$cct canopy closure threshold (units: kg Carbon)
#' @return change in forest growth

dforest = function(time, C, parameters) {
  
  # depending on canopy closure threshold (cct), use one of the following forest growth functions:
  if (C < parameters$cct) {
    dgrowth = parameters$r * C
  }
  else {
    if (C >= parameters$cct) {
      dgrowth = parameters$g * (1 - C/parameters$K)
    }
  }
  
  return(list(dgrowth))
}
