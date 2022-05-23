library(foreign)

India_rainfall<-read.csv("C:/Users/Louis/Desktop/dissertation stuff home/India/India final rainfall and cohabitation.csv")

#below assumes value 1981 is in 44th column and value 2017 in the 80th
library(Matrix)

summary(India_rainfall$v025)
India_rainfall$rural<-0
India_rainfall$rural[India_rainfall$v025=="rural"]<-1
table(India_rainfall$rural)

library(dplyr)


India_rainfall$rainfallsd1<-apply(India_rainfall[, c(44:78)], 1, FUN = sd, na.rm=TRUE)
#India_rainfall$rainfallpriormean<-apply(as.numeric(India_rainfall[, c(44:80)]), 1, FUN=mean, na.rm=TRUE)

India_rainfall$rainfallpriormean<-NA

for (i in 1:nrow(India_rainfall)){
  
  India_rainfall$rainfallpriormean[i]<-mean(as.numeric(India_rainfall[i, c(44:78)]))
  
}

India_rainfall$rainfall_deviation<-(India_rainfall$value_2015-India_rainfall$rainfallpriormean)/India_rainfall$rainfallsd1
India_rainfall$abs_rainfall_deviation<-abs(India_rainfall$rainfall_deviation)

India_rainfall$extreme_rain<-0
India_rainfall$extreme_rain[India_rainfall$rainfall_deviation>1]<-1
India_rainfall$extreme_rain[India_rainfall$rainfall_deviation<(-1)]<-1


India_rainfall$North<-0
India_rainfall$North[(India_rainfall$v024=="gujarat")|(India_rainfall$v024=="andhra pradesh")|
                       (India_rainfall$v024=="punkab")|(India_rainfall$v024=="haryana")|
                       (India_rainfall$v024=="rajasthan")|(India_rainfall$v024=="uttar pradesh")|
                       (India_rainfall$v024=="bihar")|(India_rainfall$v024=="jharkhand")|
                       (India_rainfall$v024=="odisha")|(India_rainfall$v024=="chhattisgarh")|
                       (India_rainfall$v024=="odisha")|(India_rainfall$v024=="chhattisgarh")]<-1



India_rainfall<-transform(India_rainfall, id=as.numeric(factor(LATNUM)))




library(clusterSEs)

library(stargazer)

India_rainfall$firstcousin<-India_rainfall$firstcousinfather+India_rainfall$firstcousinmother
India_rainfall_marriage<-subset(India_rainfall, India_rainfall$v508>2014)


model1<-glm(firstcousin~
              wealth
            +education
            +muslim
            +d113ed
            +North
            +extreme_rain,
            family=binomial(link='logit'),data=India_rainfall_marriage,
            na.action=na.exclude)

cousin_extreme<-cluster.bs.glm(model1, India_rainfall_marriage, ~id, report=T)

model2<-glm(firstcousin~
              wealth
            +education
            +muslim
            +d113ed
            +North
            +abs_rainfall_deviation,
            family=binomial(link='logit'),data=India_rainfall_marriage,
            na.action=na.exclude)

cousin_rainfallabs<-cluster.bs.glm(model2, India_rainfall_marriage, ~id, report=T)

library(stargazer)
stargazer(model2, model1, type="html", title="India First Cousin and Rainfall")

stargazer(cousin_rainfallabs, cousin_extreme, type="html", title="India First cousin regression clusters")
