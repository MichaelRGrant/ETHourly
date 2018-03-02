#' Calculate the saturation vapor pressure
#' 
#' @param Temp numerical value for temperature in C
#' @export

es <- function(Temp){
    es <- 0.6108*exp((17.27*Temp)/(Temp+237.3))
    return(es)
}