#' Correction needed for the fcd for nighttime
#' 
#' @param fcd_vector a numerical vector of the fcd values
#' @return a numerical vector of corrected fcd
#' @export

fcd_beta_corr <- function(fcd_vector){
    # returns the vector of corrected imputed fcd values
    for(i in 1:length(fcd_vector)){
        if(is.na(fcd_vector[i])){
            if(i == 1){
                i <- i + 1
            }
            fcd_vector[i] <- fcd_vector[i-1]
        }
    }
    j=1
    loop = T
    while(loop == T){
        if(is.na(fcd_vector[j])){
            j = j + 1
        }
        else{
            fillinNA <- fcd_vector[j]
            loop = F
        }
    }
    fcd_vector <- sapply(fcd_vector, function(x) ifelse(is.na(x), fillinNA, x))
    return(fcd_vector)
}