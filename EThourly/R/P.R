#' Calculate atmospheric pressure
#' 
#' @param z numeric value for elevation in meters
#' @export

P <- function(z){
    P <- 101.3*((293-0.0065*z)/(293))^5.26
    return(P)
}