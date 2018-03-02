#' Function to calculate hourly evapotranspiration
#' 
#' @param Tm numeric value of mean hourly temperature in F
#' @param z numeric value for elevation in meters
#' @param u2 numeric value for wind speed in m/s
#' @param Tdew numeric value for dewpoint temperature in F
#' @param Rn numeric value for net radiation in MJ/(m2*h)
#' @param G_ETo numeric value for short crop soil heat flux
#' @param G_ETr numeric value for tall crop soil heat flux
#' @return list of the short and tall crop reference evapotranspiration
#' @keywords ET, evapotranspiration, reference evapotranspiration
#' @export

ET_hr_calc <- function(Tm, z, u2, Tdew, Rn, G_ETo, G_ETr){
    
    Cn_short_day <- 37
    Cn_tall_day <- 66
    Cn_short_night <- 37
    Cn_tall_night <- 66
    Cd_short_day <- 0.24
    Cd_tall_day <- 0.25
    Cd_short_night <- 0.96
    Cd_tall_night <- 1.7
    
    u2 <- convert2ms(u2)
    Tdew <- convert2C(Tdew)
    
    delta <- delta(Tm = Tm)
    P <- P(z=z)
    pysc <- pyschro(P)
    es_data <- es(Tm)
    es_data <- es(Tdew)
    
    ETo <- ifelse(Rn<0, (0.408*delta*(Rn-G_ETo)+pysc*(Cn_short_night/(Tm+273))*u2*(es_data-ea_data))/(delta+pysc*(1+Cd_short_night*u2)), 
                  (0.408*delta*(Rn-G_ETo)+pysc*(Cn_short_day/(Tm+273))*u2*(es_data-ea_data))/(delta+pysc*(1+Cd_short_day*u2)))
    ETr <- ifelse(Rn<0, (0.408*delta*(Rn-G_ETr)+pysc*(Cn_tall_night/(Tm+273))*u2*(es_data-ea_data))/(delta+pysc*(1+Cd_tall_night*u2)),
                  (0.408*delta*(Rn-G_ETr)+pysc*(Cn_tall_day/(Tm+273))*u2*(es_data-ea_data))/(delta+pysc*(1+Cd_tall_day*u2)))
    
    ETfinal <- list(ETo, ETr)
    return(ETfinal) 
}