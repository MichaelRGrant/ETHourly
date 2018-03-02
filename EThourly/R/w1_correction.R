#' Hourly solar time angle correction
#' 
#' @param w1 solar time angle at the beginning of the hour
#' @param w2 solar time angle at the end of the hour
#' @param Ws solar time angle
#' @param neg_Ws sunrise solar time angle
#' @export

w1_correction <- function(w1, w2, Ws, neg_Ws){
    w1<-ifelse(w1<neg_Ws, neg_Ws, w1)
    w1<-ifelse(w1>Ws, Ws, w1)
    w1<-ifelse(w1>w2, w2, w1)
    return(w1)
}