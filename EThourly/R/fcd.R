#' Calculate the cloudiness function
#' 
#' @param Rs_Rso the ratio of the measured radiation (Rs) to the clear sky radiation (Rso)
#' @export

fcd <- function(Rs_Rso){
    fcd <- 1.35*(Rs_Rso)-0.35
    fcd <- ifelse(fcd<0.05, 0.05, fcd)
    fcd <- ifelse(fcd>1.0, 1.0, fcd)
    return(fcd)
}