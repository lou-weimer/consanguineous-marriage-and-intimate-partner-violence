library(foreign)

Jordanrain<-read.dta("C:/Users/Louis/Desktop/dissertation stuff home/rainfall stuff/Jordan12final.dta")
View(Jordanrain)

Jordanrain$priorstd<-NA

for (i in 1:nrow(Jordanrain)){
  
  Jordanrain$priorstd[i]<-sd(Jordanrain[i, 31:60])
}
View(Jordanrain)

Jordanrain$priormean<-NA
for (i in 1:nrow(Jordanrain)){
  
  Jordanrain$priormean[i]<-mean(as.numeric(Jordanrain[i, 31:60]))
}

Jordanrain$rainfalldelta<-(Jordanrain$value_2011-Jordanrain$priormean)/Jordanrain$priorstd
hist(Jordanrain$rainfalldelta)
Jordanrain$rainfalldeltaabs<-abs(Jordanrain$rainfalldelta)



Jordanrain$extremerain<-NA
#Jordanrain$extremerain[Jordanrain$rainfalldelta>(1)]<-1
Jordanrain$extremerain[Jordanrain$rainfalldelta>(-1)]<-0
Jordanrain$extremerain[Jordanrain$rainfalldelta<(-1)]<-1

Jordanrain$ruraldummy<-NA
Jordanrain$ruraldummy[Jordanrain$v025=='rural']<-1
Jordanrain$ruraldummy[Jordanrain$v025=='urban']<-0

Jordanrainkeep<-!is.na((Jordanrain$severeviolhus))
Jordanrain<-Jordanrain[Jordanrainkeep,]

Jordanrainkeep1<-!is.na(Jordanrain$latitude_y)
Jordanrain<-Jordanrain[Jordanrainkeep1, ]

Jordanrain<-transform(Jordanrain, id=as.numeric(factor(Jordanrain$latitude_x)))
#Jordanrain$ruraldummyinteraction<-Jordanrain$rainfalldelta*Jordanrain$ruraldummy

#Below will investigate whether external rainfall shocks affect IPV
#EXCELLENT NEWS--with updated shock date(prior year, 2011) extreme rainfall is a significant risk factor for IPV 
#under multiple specifications

#Including the rural interactions removes some of the significance, but I'm going to keep it in for 
#theoretical consistency in later regressions that have cousin marriage as a DV.
#Still pretty significant in most cases
model_rainabs_IPV<-glm(severeviolhus~firstcousin+
                         #secondcousin+
                         #otherrelative+
                         Wealth+
                         wifeeducation+
                             husbandeducation+
                         #Childrenborn+
                         #childrenathome+
                         v714wifeemployed+
                         v705husbandemployed
                           +rainfalldeltaabs*ruraldummy,
                           family=binomial(link='logit'),data=Jordanrain)

                          # na.action=na.exclude)


library(clusterSEs)
model_rainabs_IPV_cluster<-cluster.bs.glm(model_rainabs_IPV, Jordanrain, ~id, report=T)
library(stargazer)
#stargazer(model_rainabs_IPV_cluster, type="html")

model_rainabs_IPVb<-glm(lessviolhus~firstcousin+
                          #secondcousin+
                          #otherrelative+
                          Wealth+
                          wifeeducation+
                             husbandeducation+
                          #Childrenborn+childrenathome+
                          v714wifeemployed+
                          v705husbandemployed
                           +rainfalldeltaabs*ruraldummy,
                           family=binomial(link='logit'),data=Jordanrain)

model_rainabs_IPVb_cluster<-cluster.bs.glm(model_rainabs_IPVb, Jordanrain, ~id, report=T)

#reminder to also include histogram of the rainfall absolute
hist(Jordanrain$rainfalldelta)

model_extremerain_IPV<-glm(severeviolhus~
                             firstcousin+
                             #secondcousin+
                             #otherrelative+
                             Wealth+
                             wifeeducation+
                               husbandeducation+
                             #Childrenborn+childrenathome+
                             v714wifeemployed+
                             v705husbandemployed
                             +extremerain*ruraldummy,
                             family=binomial(link='logit'),data=Jordanrain)

model_extremerain_IPVb_cluster<-cluster.bs.glm(model_extremerain_IPV, Jordanrain, ~id, report=T)



model_extremerain_IPVb<-glm(lessviolhus~
                              firstcousin+
                              #secondcousin+
                              #otherrelative+
                              Wealth+
                              wifeeducation+
                               husbandeducation+
                            #+Childrenborn+childrenathome+
                              v714wifeemployed+
                              v705husbandemployed
                             +extremerain*ruraldummy,
                             family=binomial(link='logit'),data=Jordanrain)

model_extremerain_IPVbb_cluster<-cluster.bs.glm(model_extremerain_IPVb, Jordanrain, ~id, report=T)


library(stargazer)

stargazer(model_rainabs_IPV_cluster, model_rainabs_IPVb_cluster, model_extremerain_IPVb_cluster, model_extremerain_IPVbb_cluster,
          type="html", align=TRUE, title="Jordan 2012 Rainfall shocks and IPV")

#Now working on rainfall and consanguinity



Jordanrain_mar<-subset(Jordanrain, Jordanrain$v508>1985)

Jordanrain_mar<-subset(Jordanrain_mar, Jordanrain_mar$v511>16)

Jordanrain_mar$prior_marriage_rain<-NA

Jordanrain_mar$yeardiff<-2012-Jordanrain_mar$v508

n<-nrow(Jordanrain_mar)



#The below only works if the value of rainfall in the year 2012 occupies the 60th column and is immediately preceded
#by the prior years in descending order


#Defining the prior year alone yielded unsatisfactory results (best p value was .65 with 
#marriage delta regressed on first cousin)
#Going to try mean for three years prior to marriage

for (i in 1:n){
  
  Jordanrain_mar$priormarriagerain[i]<-mean(as.numeric(Jordanrain_mar[i, (62-Jordanrain_mar$yeardiff[i]-2): (62-Jordanrain_mar$yeardiff[i]-0)]))
  
}

Jordanrain_mar$priormarriagerainsd<-NA

#for (i in 1:n){
  
 
# Jordanrain_mar$priormarriagerainsd[i]<-sd(Jordanrain_mar[i, (60-Jordanrain_mar$yeardiff[i]-6):(60-Jordanrain_mar$yeardiff[i]-1)])
  
#}

for (i in 1:n){
  
  Jordanrain_mar$priormarriagerainsd[i]<-sd(Jordanrain_mar[i, 31:(62-Jordanrain_mar$yeardiff[i]-1)])
  
}

Jordanrain_mar$priormarriagerainmean<-NA

for (i in 1:n){
  
  Jordanrain_mar$priormarriagerainmean[i]<-mean(as.numeric(Jordanrain_mar[i, 31:(62-Jordanrain_mar$yeardiff[i])]))
}


Jordanrain_mar$marriage_delta<-(Jordanrain_mar$priormarriagerain-Jordanrain_mar$priormarriagerainmean)/Jordanrain_mar$priormarriagerainsd
Jordanrain_mar$extreme_marriage_delta<-NA
#Jordanrain_mar$extreme_marriage_delta[abs(Jordanrain_mar$marriage_delta)>(1)]<-1
#Jordanrain_mar$extreme_marriage_delta[abs(Jordanrain_mar$marriage_delta)<(1)]<-0

Jordanrain_mar$extreme_marriage_delta[Jordanrain_mar$marriage_delta<(-1)]<-1
Jordanrain_mar$extreme_marriage_delta[Jordanrain_mar$marriage_delta>(-1)]<-0

Jordanrain_mar$extreme_marriage_wet<-NA
Jordanrain_mar$extreme_marriage_wet[Jordanrain_mar$marriage_delta>(1)]<-1
Jordanrain_mar$extreme_marriage_wet[Jordanrain_mar$marriage_delta<(1)]<-0
#This didn't work--Jordanrain_mar$consang<-Jordanrain_mar$firstcousin+Jordanrain_mar$secondcousin+Jordanrain_mar$otherrelative

#Below, I'll look at whether prior shocks in rainfall are correlated to consnaguineous marriage

#I actually think that just including marriage delta doesn't make a ton of sense because it lumps 
#positive effects in with the negative ones--in the above regressions for IPV, there were no positive effects
#so taking an absolute value made sense
model_rain_consang<-glm(firstcousin~Wealth+wifeeducation+
                          husbandeducation+Childrenborn+childrenathome+v714wifeemployed+v705husbandemployed
                        +abs(marriage_delta)*ruraldummy,
                        family=binomial(link='logit'),data=Jordanrain_mar,
                        na.action=na.exclude)


#Neither is particularly significant. The point estimate for the rain*rural interaction is inconsistent 
#depending on how you specify 
#the number of sd's below mean that qualify as extreme

model_extreme_rain_consang<-glm(firstcousin~Wealth+wifeeducation+
                          husbandeducation+Childrenborn+childrenathome+v714wifeemployed+v705husbandemployed
                        +extreme_marriage_delta*ruraldummy,
                        family=binomial(link='logit'),data=Jordanrain_mar)

model_extreme_rain_consang_Jor_cluster<-cluster.bs.glm(model_extreme_rain_consang, 
                                                       Jordanrain_mar, ~id, report=T)



Jordanrain_mar$consanguinity<-Jordanrain_mar$firstcousin+Jordanrain_mar$secondcousin

model_extreme_rain_consang1<-glm(consanguinity~Wealth+wifeeducation+
                                  husbandeducation+Childrenborn+childrenathome+v714wifeemployed+v705husbandemployed
                                +extreme_marriage_delta*ruraldummy,
                                family=binomial(link='logit'),data=Jordanrain_mar,
                                na.action=na.exclude)

stargazer(model_extreme_rain_consang, model_extreme_rain_consang1, type="html", title="Jordan2012 Rainfall and Consanguinity")

####
#####
######
##Below is Egypt rainfall stuff

library(foreign)

Egyptrain<-read.dta('C:/Users/Louis/Desktop/dissertation stuff home/rainfall stuff/Egypt14final2mile.dta')


dupes<-duplicated(Egyptrain$X)
dupesnew<-!dupes
Egyptrain1<-Egyptrain[dupesnew,]


Egyptrain1$priorstd<-NA

for (i in 1:nrow(Egyptrain1)){
  
  Egyptrain1$priorstd[i]<-sd(Egyptrain1[i, 38:69])
}


Egyptrain1$priormean<-NA
for (i in 1:nrow(Egyptrain1)){
  
  Egyptrain1$priormean[i]<-mean(as.numeric(Egyptrain1[i, 38:69]))
}

Egyptrain1$rainfalldelta<-(Egyptrain1$value_2013-Egyptrain1$priormean)/Egyptrain1$priorstd

Egyptrain1$rainfalldeltaabs<-abs(Egyptrain1$rainfalldelta)



Egyptrain1$extremerain<-0
Egyptrain1$extremerain[Egyptrain1$rainfalldelta>(1)]<-1
Egyptrain1$extremerain[Egyptrain1$rainfalldelta<(-1)]<-1
#Egyptrain1$extremerain[Egyptrain1$rainfalldelta<(-.75)]<-1
Egyptrain1$ruraldummy<-NA
Egyptrain1$ruraldummy[Egyptrain1$v025=='urban']<-0
Egyptrain1$ruraldummy[Egyptrain1$v025=='rural']<-1

Egyptrain1<-transform(Egyptrain1, id=as.numeric(factor(Egyptrain1$latitude_x)))

Egyptrainkeep<-!is.na(Egyptrain1$lessviolhus)
Egyptrain1<-Egyptrain1[Egyptrainkeep,]
#Below investigating extreme rainfall on IPV
#Per Simeon, trying only rural subset

Egypt_rain_rural_subset<-subset(Egyptrain1, Egyptrain1$ruraldummy==1)

model_rainfallabs_IPV_Egypt<-glm(severeviolhus~
              Wealth+
              wifeeducation+
                firstcousin+
             # Childrenborn+
            #  Childrenathome+
              v714wifeemployed+
              v705husbandemployed+
              v130Christian+
              husbanddrinks
            +rainfalldeltaabs,
            family=binomial(link='logit'),data=Egypt_rain_rural_subset)

model_rainfallabs_IPv_cluster<-cluster.bs.glm(model_rainfallabs_IPV_Egypt, 
                                              Egypt_rain_rural_subset, ~id, report=T)
#The below specification yields a significant and positive result
model_rainfallabs_IPV_Egyptb<-glm(lessviolhus~
                                   Wealth+
                                   wifeeducation+
                                   #Childrenborn+
                                   #Childrenathome+
                                   v714wifeemployed+
                                   v705husbandemployed+
                                   v130Christian+
                                   husbanddrinks
                                 +rainfalldeltaabs,
                                 family=binomial(link='logit'),data=Egypt_rain_rural_subset)


model_rainfallabs_IPvb_cluster<-cluster.bs.glm(model_rainfallabs_IPV_Egyptb, 
                                               Egypt_rain_rural_subset, ~id, report=T)


model_extremerain_IPV_Egypt<-glm(severeviolhus~
                                   Wealth+
                                   wifeeducation+
                                  # Childrenborn+
                                   #Childrenathome+
                                   v714wifeemployed+
                                   v705husbandemployed+
                                   v130Christian+
                                   husbanddrinks
                                 +extremerain,
                                 family=binomial(link='logit'),data=Egypt_rain_rural_subset)

model_extremerain_IPV_Egypt_cluster<-cluster.bs.glm(model_extremerain_IPV_Egypt, 
                                                    Egypt_rain_rural_subset, ~id, report=T)


model_extremerain_IPV_Egyptb<-glm(lessviolhus~
                                    Wealth+
                                    wifeeducation+
                                   # Childrenborn+
                                    #Childrenathome+
                                    v714wifeemployed+
                                    v705husbandemployed+
                                    v130Christian+
                                    husbanddrinks
                                  +extremerain,
                                  family=binomial(link='logit'),data=Egypt_rain_rural_subset)

model_extremerain_IPVb_Egypt_cluster<-cluster.bs.glm(model_extremerain_IPV_Egyptb, 
                                                     Egypt_rain_rural_subset, ~id, report=T)


model_rainvalue_IPV_Egypt<-glm(severeviolhus~
                                   Wealth+
                                   wifeeducation+
                                   # Childrenborn+
                                   #Childrenathome+
                                   v714wifeemployed+
                                   v705husbandemployed+
                                   v130Christian+
                                   husbanddrinks
                                 +value_2013,
                                 family=binomial(link='logit'),data=Egypt_rain_rural_subset)

model_rainvalue_IPV_Egypt_cluster<-cluster.bs.glm(model_rainvalue_IPV_Egypt, 
                                                    Egypt_rain_rural_subset, ~id, report=T)


model_rainvalue_IPV_Egyptb<-glm(lessviolhus~
                                    Wealth+
                                    wifeeducation+
                                    # Childrenborn+
                                    #Childrenathome+
                                    v714wifeemployed+
                                    v705husbandemployed+
                                    v130Christian+
                                    husbanddrinks
                                  +value_2013,
                                  family=binomial(link='logit'),data=Egypt_rain_rural_subset)

model_rainvalue_IPVb_Egypt_clusterb<-cluster.bs.glm(model_rainvalue_IPV_Egyptb, 
                                                     Egypt_rain_rural_subset, ~id, report=T)















library(stargazer)
stargazer(model_rainfallabs_IPv_cluster, model_rainfallabs_IPvb_cluster, model_extremerain_IPV_Egypt_cluster, 
          model_extremerain_IPVb_Egypt_cluster, model_rainvalue_IPV_Egypt_cluster, model_rainvalue_IPVb_Egypt_clusterb,
          type="html", align=TRUE, title="Egypt 2014 Rainfall and IPV")


#Manipulating data to explore relationship between rainfall delta and consanguinity
Egyptrain1$yeardiff<-2014-Egyptrain1$v508
Egyptrain1<-subset(Egyptrain1, Egyptrain1$v508>1985)

Egyptrain1<-subset(Egyptrain1, Egyptrain1$v511>16)
#Egyptrain1<-subset(Egyptrain1, Egyptrain1$v511>17)
n<-nrow(Egyptrain1)

#for (i in 1:n){
#  
#  Egyptrain1$priormarriagerain[i]<-Egyptrain1[i, (69-Egyptrain1$yeardiff[i]-1)]
#  
#}

for (i in 1:n){
  
  Egyptrain1$priormarriagerain[i]<-mean(as.numeric(Egyptrain1[i, (71-Egyptrain1$yeardiff[i]-3):(71-Egyptrain1$yeardiff[i]-1)]))
  
}


Egyptrain1$priormarriagerainsd<-NA

for (i in 1:n){
  
  Egyptrain1$priormarriagerainsd[i]<-sd(as.numeric(Egyptrain1[i, 38:(71-Egyptrain1$yeardiff[i])]))
}

Egyptrain1$priormarriagerainmean<-NA

for (i in 1:n){
  
  Egyptrain1$priormarriagerainmean[i]<-mean(as.numeric(Egyptrain1[i, 38:(71-Egyptrain1$yeardiff[i])]))
}




Egyptrain1$priormarriage_rainfalldelta<-(Egyptrain1$priormarriagerain-Egyptrain1$priormarriagerainmean)/Egyptrain1$priormarriagerainsd

Egyptrain1$priormarriageextreme<-0
#Egyptrain1$priormarriageextreme[Egyptrain1$priormarriage_rainfalldelta>(1)]<-1
#Egyptrain1$priormarriageextreme[Egyptrain1$priormarriage_rainfalldelta<(1)]<- 0
Egyptrain1$priormarriageextreme[Egyptrain1$priormarriage_rainfalldelta<(-1)]<-1

Egyptrain1$priormarriageabs<-abs(Egyptrain1$priormarriage_rainfalldelta)

#BOOM! defined as +/- 1 sd's above mean, extreme reain is extreme risk for consanguineous marriage. P value of
#.22 when subistitute for 1 sd above/below mean
#Also important--significant regadless of how we define prior marriage variable--just one year prior to marriage, 
#or average of 3 years

#right now the interaction term is included--neither the rural nor interaction terms are significant, but the 
#prior to marriage shock remains so regardless
model_extremerain_consang_Egypt<-glm(firstcousin~
                                             Wealth+
                                             wifeeducation+
                                             #Childrenborn+
                                             #Childrenathome+
                                             v714wifeemployed+
                                             v705husbandemployed+
                                             v130Christian+
                                             husbanddrinks
                                           +priormarriageextreme*ruraldummy,
                                           family=binomial(link='logit'),data=Egyptrain1)

model_extremerain_consang_Egypt_cluster<-cluster.bs.glm(model_extremerain_consang_Egypt, 
                                                     Egyptrain1, ~id, report=T)



model_extremerain_consang_Egypt_inter<-glm(consang~
                                    Wealth+
                                    wifeeducation+
                                    #Childrenborn+
                                    #Childrenathome+
                                    v714wifeemployed+
                                    v705husbandemployed+
                                    v130Christian+
                                    husbanddrinks
                                  +priormarriageextreme*ruraldummy,
                                  family=binomial(link='logit'),data=Egyptrain1,
                                  na.action=na.exclude)

stargazer(model_extremerain_consang_Egypt, model_extremerain_consang_Egypt_inter, type="html", 
          title="Egypt Extreme Rain and Consanguinity")
#Below is super significant (.002)




model_extremerain_consang_Egyptb<-glm(firstcousin~
                                              Wealth+
                                              wifeeducation+
                                              Childrenborn+
                                              Childrenathome+
                                              v714wifeemployed+
                                              v705husbandemployed+
                                              v130Christian+
                                              husbanddrinks
                                            +priormarriageabs+daughterinlawhead,
                                            family=binomial(link='logit'),data=Egyptrain1,
                                            na.action=na.exclude)





model_extremerain_consang_Egyptb_inter<-glm(firstcousin~
                                       Wealth+
                                       wifeeducation+
                                       Childrenborn+
                                       Childrenathome+
                                       v714wifeemployed+
                                       v705husbandemployed+
                                       v130Christian+
                                       husbanddrinks
                                     +priormarriageabs*ruraldummy 
                                     +daughterinlawhead,
                                     family=binomial(link='logit'),data=Egyptrain1,
                                     na.action=na.exclude)


library(stargazer)
stargazer(model_extremerain_consang_Egypt, model_extremerain_consang_Egypt_inter, model_extremerain_consang_Egyptb,
          model_extremerain_consang_Egyptb_inter, type="html", align=TRUE, title="Egypt 2014 rainfall and Consanguinity")
