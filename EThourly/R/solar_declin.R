#' Calculate the solar declination
#' 
#' @param date
#' @export

solar_declin <- function(date){
    Jday <- Jday(date)
    declin <- 0.409*sin(((2*pi)/365)*Jday - 1.39)
    return(declin)
}