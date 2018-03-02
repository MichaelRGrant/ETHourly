#' Calculate the Julian day
#' 
#' @param date
#' @export

Jday <- function(date){
    require(lubridate)
    Jday <- yday(date)
    return(Jday)
}