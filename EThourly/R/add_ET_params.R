#' Wrapper for all ET hourly functions to add to the dataset
#' 
#' @param data dataframe with all the necessary measured values
#' @return dataframe with all the intermediate ET params with the reference ETo and ETr per hour
#' @export

add_ET_params <- function(data){
    require(plyr)
    data_list <- list()
    stations <- unique(data$station)
    for(i in 1:length(stations)){
        print(as.character(stations[i]))
        data_list[[i]] <- subset(data, station == stations[i])
        data_list[[i]] <- transform(data_list[[i]], sct = hour+0.5)
        data_list[[i]] <- transform(data_list[[i]], beta = beta(lat = data_list[[i]]$lat, long = data_list[[i]]$long, sct = data_list[[i]]$sct, date = data_list[[i]]$date2))
        data_list[[i]] <- transform(data_list[[i]], w = solar_time_angle(data_list[[i]]$date2, data_list[[i]]$long, data_list[[i]]$sct))
        data_list[[i]] <- transform(data_list[[i]], w1 = w1(data_list[[i]]$w))
        data_list[[i]] <- transform(data_list[[i]], w2 = w2(data_list[[i]]$w))
        data_list[[i]] <- transform(data_list[[i]], ws = sunset_angle(data_list[[i]]$lat, 
                                                                      data_list[[i]]$date2))
        data_list[[i]] <- transform(data_list[[i]], neg_ws = -data_list[[i]]$ws)
        data_list[[i]] <- transform(data_list[[i]], w1_corr = w1_correction(data_list[[i]]$w1, 
                                                                            data_list[[i]]$w2, 
                                                                            data_list[[i]]$ws, 
                                                                            data_list[[i]]$neg_ws))
        data_list[[i]] <- transform(data_list[[i]], w2_corr = w2_correction(data_list[[i]]$w2, 
                                                                            data_list[[i]]$ws, 
                                                                            data_list[[i]]$neg_ws))
        data_list[[i]] <- transform(data_list[[i]], dr = distance(data_list[[i]]$date2))
        data_list[[i]] <- transform(data_list[[i]], Ra = Ra_hr(data_list[[i]]$lat, 
                                                               data_list[[i]]$date2, 
                                                               data_list[[i]]$dr, 
                                                               data_list[[i]]$w1_corr, 
                                                               data_list[[i]]$w2_corr))
        data_list[[i]] <- transform(data_list[[i]], Rso = Rso(data_list[[i]]$elevation, 
                                                              data_list[[i]]$lat, 
                                                              data_list[[i]]$date2, 
                                                              data_list[[i]]$dr, 
                                                              data_list[[i]]$w1_corr, 
                                                              data_list[[i]]$w2_corr))
        data_list[[i]]$Rs <- convert2MJm2h(data_list[[i]]$solar_Watts_m2)
        data_list[[i]] <- transform(data_list[[i]], Rs_Rso = data_list[[i]]$Rs/data_list[[i]]$Rso)
        data_list[[i]] <- transform(data_list[[i]], fcd = fcd(data_list[[i]]$Rs_Rso))
        data_list[[i]] <- transform(data_list[[i]], fcd = ifelse(data_list[[i]]$beta<0.3, NA, data_list[[i]]$fcd))
        data_list[[i]]$fcd <- fcd_beta_corr(data_list[[i]]$fcd)
        data_list[[i]] <- transform(data_list[[i]],
                                    Rnl = Rnl(data_list[[i]]$fcd, 
                                              data_list[[i]]$dewpoint, 
                                              data_list[[i]]$Tm),
                                    Rns = (1-0.23)*data_list[[i]]$Rs)
        data_list[[i]] <- transform(data_list[[i]], 
                                    Rn = data_list[[i]]$Rns - data_list[[i]]$Rnl)
        data_list[[i]] <- transform(data_list[[i]], 
                                    G_ETo = G(data_list[[i]]$Rn)[[1]],
                                    G_ETr = G(data_list[[i]]$Rn)[[2]])
        data_list[[i]] <- transform(data_list[[i]], 
                                    ETo = ET_hr(data_list[[i]]$Tm, 
                                                data_list[[i]]$elevation, 
                                                data_list[[i]]$wind_speed, 
                                                data_list[[i]]$dewpoint, 
                                                data_list[[i]]$Rn, 
                                                data_list[[i]]$G_ETo, 
                                                data_list[[i]]$G_ETr)[[1]],
                                    ETr = ET_hr(data_list[[i]]$Tm, 
                                                data_list[[i]]$elevation, 
                                                data_list[[i]]$wind_speed, 
                                                data_list[[i]]$dewpoint, 
                                                data_list[[i]]$Rn, 
                                                data_list[[i]]$G_ETo, 
                                                data_list[[i]]$G_ETr)[[2]])
    }
    final_data <- ldply(data_list, data.frame)
    return(final_data)
}