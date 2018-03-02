#' Calculate the soil heat flux for both tall and short reference crops. This automatically takes into consideration day versus night differences. 
#' 
#' @param Rn numberical value for the net radiation
#' @return list of soil heat flux for both short and tall reference crops
#' @export

G <- function(Rn){
    G_ETo <- ifelse(Rn<0, 0.5*Rn, 0.1*Rn)
    G_ETr <- ifelse(Rn<0, 0.2*Rn, 0.04*Rn)
    G_list <- list(G_ETo, G_ETr)
    return(G_list)
}