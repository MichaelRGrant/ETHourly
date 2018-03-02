#' Convert from W/m2*hour to MJ/m2*hour
#' 
#' @param solar measured solar radiation in Watts/meter^2*hour
#' @return solar radiation in MJ/m2*h
#' @export

convert2MJm2h <- function(solar){
    solar <- solar * 0.0036
    return(solar)
}