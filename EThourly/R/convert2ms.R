#' Convert to meters per second from miles per hour
#' 
#' @param data numeric value in miles per hour
#' @export

convert2ms <- function(data){
    data <- data*0.44704
    return(data)
}