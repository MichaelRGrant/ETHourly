#' Convert to farenheit to celsius 

#' @param TF numerical value for a temperture in F
#' @export

convert2C <- function(TF){
    TC <- TF-32*(5/9)
    return(TC)
}