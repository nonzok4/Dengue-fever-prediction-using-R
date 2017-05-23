#Reading in data file
install.packages('corrplot')
library(corrplot)
x<-read.csv("lbld_train_2.csv",header=TRUE)

corrplot(cor(x[, -1]))
?corrplot
library(plyr)
x<-rename(x, c("reanalysis_air_temp_k"="temp_avg", "reanalysis_dew_point_temp_k"="temp_dew"))
x<-rename(x, c("reanalysis_relative_humidity_percent"="humidity%", "reanalysis_sat_precip_amt_mm"="sat_precip"))
x<-rename(x, c("reanalysis_specific_humidity_g_per_kg"="humidit_g_kg"))
x<-rename(x, c("reanalysis_tdtr_k"="tdtr_k"))
x<-rename(x, c("station_diur_temp_rng_c"="diur_temp"))
x<-rename(x, c("station_precip_mm"="precip_mm"))