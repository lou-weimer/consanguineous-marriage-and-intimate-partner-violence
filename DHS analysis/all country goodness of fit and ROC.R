
library(foreign)

Egypt<-read.csv("C:/Users/Louis/Desktop/prospectus stuff home/egyptdata.csv")


EgyptIR<-Egypt[,c("severeviolhus", "lessviolhus", "consang", "levelconsang", "husbanded", "wifeeducation",
                  "husbanddrinks", "Wealth", "Childrenborn", "Childrenathome", 
                  "firstcousin", "secondcousin", "otherrelative", "v705husbandemployed",
                  "v714wifeemployed", "v130Christian", "sexualviolence")]


model1<-glm(severeviolhus~firstcousin+secondcousin+otherrelative+husbanded+wifeeducation
            +husbanddrinks+Wealth+Childrenborn+Childrenathome
            +v705husbandemployed+v714wifeemployed+v130Christian, 
            family=binomial(link='logit'),data=EgyptIR,
            na.action=na.exclude)

library(generalhoslem)

library(pROC)



gofpre<-cbind(EgyptIR$severeviolhus, fitted(model1))

gofpre1<-subset(gofpre, is.na(gofpre[,2])==FALSE)

logitgof(gofpre1[,1], gofpre1[,2])
roccurve <- roc(gofpre1[,1]~ gofpre1[,2])
plot(roccurve, main="Egypt Severe Violence ROC Curve")


model2<-glm(lessviolhus~firstcousin+secondcousin+otherrelative+husbanded+wifeeducation
            +husbanddrinks+Wealth+Childrenborn+Childrenathome+
              v705husbandemployed+v714wifeemployed+v130Christian, 
            family=binomial(link='logit'),data=EgyptIR,
            na.action=na.exclude)




gofpre<-cbind(EgyptIR$lessviolhus, fitted(model2))

gofpre1<-subset(gofpre, is.na(gofpre[,2])==FALSE)

logitgof(gofpre1[,1], gofpre1[,2])
roccurve <- roc(gofpre1[,1]~ gofpre1[,2])
plot(roccurve, main="Egypt Less Severe Violence ROC Curve")


Jordan12IR<-read.csv("C:/Users/Louis/Desktop/prospectus stuff home/Jordandata.csv")

Jordan12<-Jordan12IR[,c("firstcousin", "secondcousin", "otherrelative", "lessviolhus",
                        "severeviolhus", "Wealth", "wifeeducation", "husbandeducation", 
                        "Childrenborn", "childrenathome", "v714wifeemployed", 
                        "v705husbandemployed", "helpfatherinlaw", "helpmotherinlaw")]



model1<-glm(severeviolhus~firstcousin+secondcousin+otherrelative+Wealth+wifeeducation+
              husbandeducation+Childrenborn+childrenathome+v714wifeemployed+v705husbandemployed,
            family=binomial(link='logit'),data=Jordan12,
            na.action=na.exclude)



gofpre<-cbind(Jordan12$severeviolhus, fitted(model1))

gofpre1<-subset(gofpre, is.na(gofpre[,2])==FALSE)

logitgof(gofpre1[,1], gofpre1[,2])
roccurve <- roc(gofpre1[,1]~ gofpre1[,2])
plot(roccurve, main="Jordan Severe Violence ROC Curve")


model2<-glm(lessviolhus~firstcousin+secondcousin+otherrelative+Wealth+wifeeducation+
              husbandeducation+Childrenborn+childrenathome+v714wifeemployed+v705husbandemployed,
            family=binomial(link='logit'),data=Jordan12,
            na.action=na.exclude)



gofpre<-cbind(Jordan12$lessviolhus, fitted(model2))

gofpre1<-subset(gofpre, is.na(gofpre[,2])==FALSE)

logitgof(gofpre1[,1], gofpre1[,2])
roccurve <- roc(gofpre1[,1]~ gofpre1[,2])
plot(roccurve, main="Jordan Less Severe Violence ROC Curve")









Pak12Individualex<-read.csv("C:/Users/Louis/Desktop/prospectus stuff home/Pakistandata.csv")



#d114ed is drunkenness, V106ed is education, v190ed is wealth index,
#s111cousin is cousin marriage or not, d113 is husband drinker or not, 
#s113 is whether respondent had say in choosing husband or not
Pak12Individualbase<-Pak12Individualex[,c("v106","v106ed", "s110", 
                                          "s111", "s111ed", "s113", "d106", "d107", "d108", "d114ed", "v190ed",
                                          "s111firstcousin", "s111secondcousin", "s111other", "d113",
                                          "baluchi", "baruhi", "punjabi","pushto", "sariaki", "sindhi", "urdu", 
                                          "wifeedu", "s113", "v705husunemployed", "v714wifeemployed")]
Pak12Individualconsang<-subset(Pak12Individualbase, is.na(Pak12Individualbase$d106)==FALSE)

model5<-glm(d106~v106ed+s111firstcousin+s111secondcousin+s111other+d114ed+v190ed
            #+baluchi+baruhi+punjabi+pushto+sariaki+sindhi+urdu
            +wifeedu +v705husunemployed+v714wifeemployed, 
            family=binomial(link='logit'),data=Pak12Individualconsang,
            na.action=na.exclude)





gofpre<-cbind(Pak12Individualconsang$d106, fitted(model5))

gofpre1<-subset(gofpre, is.na(gofpre[,2])==FALSE)

logitgof(gofpre1[,1], gofpre1[,2])
roccurve <- roc(gofpre1[,1]~ gofpre1[,2])
plot(roccurve, main="Pakistan Less Violent ROC Curve")



model6<-glm(d107~v106ed+s111firstcousin+s111secondcousin+s111other+d114ed+v190ed
            #+baluchi+baruhi+punjabi+pushto+sariaki+sindhi+urdu
            +wifeedu +v705husunemployed+v714wifeemployed, 
            family=binomial(link='logit'),data=Pak12Individualconsang,
            na.action=na.exclude)

gofpre<-cbind(Pak12Individualconsang$d107, fitted(model6))

gofpre1<-subset(gofpre, is.na(gofpre[,2])==FALSE)

logitgof(gofpre1[,1], gofpre1[,2])
roccurve <- roc(gofpre1[,1]~ gofpre1[,2])
plot(roccurve, main="Pakistan Less Violent ROC Curve")


