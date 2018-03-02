#' Solar time angles at the beginning and end of each hourly period
#' 
#' @param w solar time angle
#' @export

w1 <- function(w){
    w1 <- w - pi/24
    return(w1)
}