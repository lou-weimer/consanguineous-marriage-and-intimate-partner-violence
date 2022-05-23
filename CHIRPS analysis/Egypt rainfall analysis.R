rm(list=ls())
gc()
memory.limit(1000000000000)
library(RNetCDF)

nc <- open.nc("C:/chirps data/chirps-v2.0.annual.nc")
print.nc(nc) # print for information


nc_data <- read.nc(nc) # actually read data
precip <- nc_data$precip # take the 3d precipitation matrix


# add the names for the dimensions for the next step
dimnames(precip)<-list(d1=nc_data$longitude, d2=nc_data$latitude, d3=nc_data$time)
rm(nc_data)
# now when we convert this to a table the dimnames will be automatically set
df <- as.data.frame.table(precip, responseName = "value", stringsAsFactors = FALSE)
rm(precip)
rm(nc)
# there seem to be quite a few missing values
relevant <- df[!is.na(df$value) & df$value!=0, ]
colnames(relevant) <- c("longitude", "latitude", "time", "value")
rm(df)
relevant$latitude <- as.numeric(relevant$latitude)
relevant$longitude <- as.numeric(relevant$longitude)
relevant$time <- as.numeric(relevant$time)
#head(relevant)

relevantEgypt<- subset(relevant, ((relevant$latitude>20) &(relevant$latitude<35))& ((relevant$longitude>22)& (relevant$longitude<40)))
rm(relevant)

write.dta(relevantEgypt, "C:/dissertation stuff/Egypt/Egyptrelevantchirps.dta")
library(foreign)
relevantEgypt<-read.dta("C:/dissertation stuff/Egypt/Egyptrelevantchirps.dta")
#rm(Jordan12, Jordan12final, Jordan12final2mile, Jordangeo, Jordangeo1, Jtry, relevant, relevantJordan, relevantJordan3)
relevantEgypt$year<-round((relevantEgypt$time)/365)+1980
relevantEgypt1<-subset(relevantEgypt, relevantEgypt$year<2015)
relevantEgypt1<-relevantEgypt1[, c("longitude", "latitude","year", "value")]
relevantEgypt2<-relevantEgypt1[order(relevantEgypt1[,1], relevantEgypt1[,2]),  ]
library(reshape)
rm(relevantEgypt, relevantEgypt1)
relevantEgypt3<-reshape(relevantEgypt2, idvar=c("longitude", "latitude"), timevar=c("year"), direction="wide")

rm(relevantEgypt2)
write.dta(relevantEgypt3, "C:/dissertation stuff/Egypt/Egyptrelevanttransformedchirps.dta")
library(foreign)
relevantEgypt3<-read.dta("C:/dissertation stuff/Egypt/Egyptrelevanttransformedchirps.dta")
EgyptgeoIR<-read.csv("C:/dissertation stuff/Egypt/Egypt 14 subset.csv")
EgyptgeoIR$longitude<-EgyptgeoIR$LONGNUM
EgyptgeoIR$latitude<-EgyptgeoIR$LATNUM

library(fuzzyjoin)

Egypt14final<-geo_left_join(EgyptgeoIR, relevantEgypt3, 
                            by=c("longitude", "latitude"), max_dist=2)
write.dta(Egypt14final, "C:/dissertation stuff/Egypt/Egypt14final2mile.dta")                             

rm(EgyptgeoIR, relevantEgypt3)

Egypt14finalmile2<-read.dta("C:/dissertation stuff/Egypt/Egypt14final2mile.dta")
dupes<-duplicated(Egypt14finalmile2$X)
dupesnew<-!dupes
Egypt14finalmile2new<-Egypt14finalmile2[dupesnew,]



Egypt14finalmile2new$rainfallsd1<-apply(Egypt14finalmile2new[, c(34:66)], 1, FUN = sd, na.rm=TRUE)
Egypt14finalmile2new$rainfallpriormean<-apply(Egypt14finalmile2new[, c(34:61)], 1, FUN = mean, na.rm=TRUE)
Egypt14finalmile2new$rainfallcurrentmean<-apply(Egypt14finalmile2new[, c(62:66)], 1, FUN = mean, na.rm=TRUE)
Egypt14finalmile2new$rainfalldelta<-Egypt14finalmile2new$rainfallcurrentmean-Egypt14finalmile2new$rainfallpriormean
Egypt14finalmile2new$extremerainfall<-(Egypt14finalmile2new$rainfallcurrentmean-Egypt14finalmile2new$rainfallpriormean)/Egypt14finalmile2new$rainfallsd1

Egypt14finalmile2new$extremerainfall1<-NA
Egypt14finalmile2new$extremerainfall1[Egypt14finalmile2new$extremerainfall<(-1)]<-1
Egypt14finalmile2new$extremerainfall1[Egypt14finalmile2new$extremerainfall>(-1)]<-0


Egyptagesubset<-subset(Egypt14finalmile2new, Egypt14finalmile2new$v012<26)
model1<-glm(lessviolhus~firstcousin+
              secondcousin+
              otherrelative+
              Wealth+
              wifeeducation+
              Childrenborn+
              Childrenathome+
              v714wifeemployed+
              v705husbandemployed+
              v130Christian+
              husbanddrinks
              +extremerainfall1,
            family=binomial(link='logit'),data=Egyptagesubset,
            na.action=na.exclude)


model2<-glm(firstcousin~
              Wealth+
              wifeeducation+
              Childrenborn+
              Childrenathome+
              v714wifeemployed+
              v705husbandemployed+
              v130Christian+
              husbanddrinks
            +extremerainfall1,
            family=binomial(link='logit'),data=Egyptagesubset,
            na.action=na.exclude)
