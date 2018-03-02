#' Calculate the extraterrestrial radiation
#' 
#' @param lat latitude in degrees
#' @param date
#' @param dr distance from earth to the sun
#' @param w1 solar time angle for beginning of hour
#' @param w2 solar time angle for end of hour
#' @export

Ra_hr <- function(lat, date, dr, w1, w2, Gsc = 4.92){
    # Ra calculated for hourly data collection
    lat <- (pi/180)*lat
    declin <- solar_declin(date)
    Ra <- (12/pi) * Gsc * dr * ((w2-w1)*sin(lat)*sin(declin) + 
                                    cos(lat)*cos(declin)*(sin(w2)-sin(w1)))
    return(Ra)
}