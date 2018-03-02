#' Calculate the solar time angle
#' 
#' @param date
#' @param long the longitude in degrees, positive for West, negative for East
#' @param sct the standard clock time
#' @export

solar_time_angle <- function(date, long, sct, Lz=120){
    Sc <- season_correction(date)
    long = abs(long)
    w = (pi/12)*((sct+0.06667*(Lz-long)+Sc)-12)
    return(w)
}