#' Calculate clear sky radiation
#' 
#' @param z elevation in meters
#' @param lat latitude in degrees
#' @param date
#' @param dr distance from earth to the sun
#' @param w1 solar time angle for beginning of hour
#' @param w2 solar time angle for end of hour
#' @export

Rso <- function(z, lat, date, dr, w1, w2){
    Rso <- (0.75 + 2e-5*z)*Ra_hr(lat = lat, date = date, dr=dr, w1 = w1, w2 = w2)
    return(Rso)
}