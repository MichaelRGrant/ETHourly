#' Calculate the sunset hour angle of the sun
#' 
#' @param lat latitude in degrees, postive for N, negative for S
#' @export

sunset_angle <- function(lat, date){
    lat <- (pi/180) * lat
    declin <- solar_declin(date)
    sunset_angle <- acos(-tan(lat)*tan(declin))
    return(sunset_angle)
}