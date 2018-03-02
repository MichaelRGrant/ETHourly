#' Calculate the angle of the sun above the horizon
#' 
#' @param lat the latitude in degrees, positive for North, negative for East
#' @param long the longitude in degrees, positive for West, negative for East
#' @param sct the standard clock time
#' @param date date
#' @export


beta <- function(lat, long, sct, date){
    lat <- (pi/180) * lat
    w <- solar_time_angle(date, long, sct)
    declin <- solar_declin(date)
    beta <- asin(sin(lat)*sin(declin) + cos(lat)*cos(declin)*cos(w))
    return(beta)
}