#' Calculate the distance from earth to the sun
#' 
#' @param date
#' @export

distance <- function(date){
    J <- Jday(date)
    dr <- 1 + 0.033 * cos(((2*pi)/365)*J)
    return(dr)
}