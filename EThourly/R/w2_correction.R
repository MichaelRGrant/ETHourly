#' Hourly solar time angle correction
#' 
#' @param w2 solar time angle at the end of the hour
#' @param Ws solar time angle
#' @param neg_Ws sunrise solar time angle
#' @export

w2_correction <- function(w2, Ws, neg_Ws){
    w2 <- ifelse(w2<neg_Ws, neg_Ws, w2)
    w2 <- ifelse(w2>Ws, Ws, w2)
    return(w2)
}