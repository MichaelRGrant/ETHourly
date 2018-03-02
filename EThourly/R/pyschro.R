#' Calculate the psychrometric constant
#' 
#' @param P numeric value for the atmospheric pressure in kPa
#' @export

pyschro <- function(P, cp=1.013e3, epsilon=0.622, lambda=2.45){
    pysc <- 0.000665*P
    return(pysc)
}