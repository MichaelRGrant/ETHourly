#' Correction for the Rs/Rso ratio. Because Rs/Rso is undefined at night, this correctin is needed. 
#' 
#' @param Rs_Rso raio of the measured and clear sky radiation
#' @export

Rs_Rso_correction <- function(Rs_Rso){
    # if(Rs_Rso == 'Inf' | Rs_Rso == 'NaN'){Rs_Rso <- NA}
    Rs_Rso <- ifelse(Rs_Rso == Inf, NA, Rs_Rso)
    Rs_Rso <- ifelse(Rs_Rso < 0.3, 0.3, Rs_Rso)
    Rs_Rso <- ifelse(Rs_Rso > 1.0, 1.0, Rs_Rso)
    return(Rs_Rso)
}