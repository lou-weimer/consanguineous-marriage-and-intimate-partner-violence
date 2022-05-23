rm(list=ls())
gc()
memory.limit(10000000000000)
library(RNetCDF)

nc <- open.nc("C:/dissertation stuff/chirps data/chirps-v2.0.annual.nc")
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

relevantJordan<-subset(relevant, ((relevant$latitude>25) &(relevant$latitude<35))& ((relevant$longitude>35)& (relevant$longitude<45)))
relevantEgypt<- subset(relevant, ((relevant$latitude>20) &(relevant$latitude<35))& ((relevant$longitude>22)& (relevant$longitude<40)))
#save(relevantJordan, file="C:/chirps data/relevantJordan.Rdata")
#save(relevantEgypt, file="C:/chirps data/relevantEgypt.Rda")


#dput(relevantJordan, file="C:/chirps data/relevantJordan.txt")

#rJordan<-load(file="C:/chirps data/relevantJordan.Rdata")

relevantJordan$year<-round((relevantJordan$time)/365)+1980
relevantJordan1<-subset(relevantJordan, relevantJordan$year<2013)
relevantJordan1<-relevantJordan1[, c("longitude", "latitude","year", "value")]
relevantJordan2<-relevantJordan1[order(relevantJordan1[,1], relevantJordan1[,2]),  ]
library(reshape)
rm(relevantJordan1)
relevantJordan3<-reshape(relevantJordan2, idvar=c("longitude", "latitude"), timevar=c("year"), direction="wide")

rm(relevantJordan2)
write.dta(relevantJordan3, "C:/dissertation stuff/chirps data/transformed Jordan.dta")









library(foreign)
Jordangeo<-read.dbf("C:/dissertation stuff/Jordan/JOGE6AFL.dbf")

Jordangeo1<-Jordangeo[, c("DHSCLUST", "LATNUM", "LONGNUM")]

Jordangeo1$v001<-Jordangeo1$DHSCLUST
Jordan12<-read.csv("C:/dissertation stuff/Jordan/Jordan12subset.csv")


#Jordan12geomerge<-merge(Jordangeo1, Jordan12, by="v001")
Jordan12$longitude<-Jordan12$LONGNUM
Jordan12$latitude<-Jordan12$LATNUM

#rm(Jordangeo, Jordangeo1, Jordan12)
library(fuzzyjoin)

Jordan12final<-geo_left_join(Jordan12, relevantJordan3, max_dist=2,
                             by=c("longitude", "latitude"))
dupes<-duplicated(Jordan12final$X)
dupesnew<-!dupes
Jordan12final2mile<-Jordan12final[dupesnew,]




#Below should be dataset used to calculate rainfall prior to cohabitation
write.dta(Jordan12final2mile, "C:/dissertation stuff/Jordan/Jordan12final.dta")


model1<-glm(severeviolhus~firstcousin+secondcousin+otherrelative+Wealth+wifeeducation+
              husbandeducation+Childrenborn+childrenathome+v714wifeemployed+v705husbandemployed
            +priorstd,
            family=binomial(link='logit'),data=Jordan12final,
            na.action=na.exclude)


