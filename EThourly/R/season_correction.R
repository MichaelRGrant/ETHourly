#' Calculate the seasonal correction
#' 
#' @param date
#' @export

season_correction <- function(date){
    Jday <- Jday(date)
    b <- (2*pi*(Jday-81))/364
    Sc <- 0.1645*sin(2*b)-0.1255*cos(b)-0.025*sin(b)
    return(Sc)
}