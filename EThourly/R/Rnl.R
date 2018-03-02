#' Calculate long wave radiation
#' 
#' @param fcd numerical value for cloudiness function
#' @param Tdew mean hourly dewpoint temperature in C
#' @param Tm mean hourly temperature in C
#' @export

Rnl <- function(fcd, Tdew, Tm){
    # input temps in farenheight
    Tdew <- convert2C(Tdew)
    Tm <- convert2C(Tm)
    TmK <- Tm+273.16
    Rnl <- (2.042e-10)*fcd*(0.34-0.14*sqrt(ea(Tdew)))*TmK^4
    return(Rnl)
}