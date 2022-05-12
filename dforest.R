#' Forest Growth Model
#' 
#' @param C  size of forest (units: kg Carbon)
#' @param cct canopy closure threshold (units: kg Carbon)
#' @param parameters list of parameters, including: r, g, K
#' @param parameters$r early exponential growth rate
#' @parms parameters$g linear growth rate once canopy closure has been reached
#' @parms parameters$K carrying capacity (units: kg Carbon)
#' @return change in forest growth
#'
dforest = function(time, C, cct, parameters) {
  
  # depending on canopy closure threshold, use one of the following forest growth functions
  if (C < cct) {
    dgrowth = parameters$r * C
  }
  else {
    if (C >= cct) {
      dgrowth = parameters$g * (1 - C/parameters$K)
    }
  }
  
  return(list(dgrowth))
}
