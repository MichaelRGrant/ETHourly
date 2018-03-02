#' Calculate the actual vapor pressure
#' 
#' @param Tdew numerical value for dewpoint temperature in C
#' @export

ea <- function(Tdew){
    ea <- es(Tdew)
    return(ea)
}