#' Calculate the slope of the saturation vapor pressure-temperature curve
#' 
#' @param Tm mean temperature in C
#' @export

delta <- function(Tm){
    expo <- (17.27*Tm)/(Tm + 237.3)
    slope <- (2503*exp(expo))/(Tm+237.3)^2
    return(slope)
}