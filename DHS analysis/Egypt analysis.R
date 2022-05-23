library(foreign)
Egyptdata<-read.dta("C:/Users/U046466/Desktop/Project/Egypt/2014/egir61dt/EGIR61FL.DTA")
write.csv(Egyptdata, "C:/Users/U046466/Desktop/Project/Egypt/2014/egir61dt/EGYPT Individual Recode.csv")
Egypt<-read.csv("C:/Users/Louis/Desktop/dissertation stuff home/egyptdata.csv")


EgyptIR<-Egypt[,c("severeviolhus", "lessviolhus", "consang", "levelconsang", "husbanded", "wifeeducation",
                  "husbanddrinks", "Wealth", "Childrenborn", "Childrenathome", 
                  "firstcousin", "secondcousin", "otherrelative", "v705husbandemployed",
                  "v714wifeemployed", "v130Christian", "sexualviolence")]

model1<-glm(severeviolhus~firstcousin+secondcousin+otherrelative+husbanded+wifeeducation
          +husbanddrinks+Wealth+Childrenborn+Childrenathome
          +v705husbandemployed+v714wifeemployed+v130Christian, 
            family=binomial(link='logit'),data=EgyptIR,
            na.action=na.exclude)
#Confirmed first cousin marriage is a significant protetive factor for more severe violence

library(stargazer)
stargazer(model1, type="latex")

#Not originally code from work. This is an attempt to address James' critique that there is a mediating variable at
#work here. Reading in the mediate package, the guide to which is in the prospectus folder.

#First let's try drinking as a mediating variable, with and without consanguinity
library(mediation)
#Model for the mediating variable

Egyptsub1<-subset(EgyptIR, !is.na(EgyptIR$severeviolhus))

model.m<-glm(husbanddrinks~firstcousin+secondcousin+otherrelative+husbanded+wifeeducation
             +Wealth+Childrenborn+Childrenathome
             +v705husbandemployed+v714wifeemployed+v130Christian, 
             family=binomial(link='logit'),data=Egyptsub1,
             na.action=na.exclude)
model.y<-glm(severeviolhus~firstcousin+secondcousin+otherrelative+husbanded+wifeeducation
             +husbanddrinks+Wealth+Childrenborn+Childrenathome
             +v705husbandemployed+v714wifeemployed+v130Christian, 
             family=binomial(link='logit'),data=EgyptIR,
             na.action=na.exclude)

firstmediate<-mediate(model.m, model.y, treat="firstcousin", mediator="husbanddrinks", sims=5000)
summary(firstmediate)
  
#Seems to be good, though I'm not positive how to read the output. Suggests that the causal effect of 
#first cousin marriage is significant and negative

#Let's try the same thing but with second cousin marriage as the treatment variable


secondmediate<-mediate(model.m, model.y, treat="secondcousin", mediator="husbanddrinks", sims=1000)
summary(secondmediate)


thirdmediate<-mediate(model.m, model.y, treat="otherrelative", mediator="husbanddrinks", sims=1000)
summary(thirdmediate)

#For second cousin and other relatives, HUUUUUGE difference in outcomes. Neither is remotely significant
#(assuming I'm reading the output correctly and that I understand the function)








model2<-glm(lessviolhus~firstcousin+secondcousin+otherrelative+husbanded+wifeeducation
            +husbanddrinks+Wealth+Childrenborn+Childrenathome+
              v705husbandemployed+v714wifeemployed+v130Christian, 
            family=binomial(link='logit'),data=EgyptIR,
            na.action=na.exclude)

stargazer(model2, model1, type="html", title="Egypt DHS")
#Not significant protective against sexual violence
model3<-glm(sexualviolence~firstcousin+secondcousin+otherrelative+husbanded+wifeeducation
            +husbanddrinks+Wealth+Childrenborn+Childrenathome+
              v705husbandemployed+v714wifeemployed+v130Christian, 
            family=binomial(link='logit'),data=EgyptIR,
            na.action=na.exclude)
