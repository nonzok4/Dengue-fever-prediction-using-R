install.packages("MASS")
library(MASS)
dat<-read.csv("train.csv")

# independent variables to be determined
ml<-glm.nb(total_cases~reanalysis_dew_point_temp_k+reanalysis_specific_humidity_g_per_kg+station_avg_temp_c+station_precip_mm,data=dat)
summary(ml)

test<-read.csv("dengue_features_test.csv")
test$cases<-predict(ml,test,type="response")
