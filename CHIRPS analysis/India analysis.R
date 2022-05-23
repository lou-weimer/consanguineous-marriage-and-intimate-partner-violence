library(foreign)
Indiaraw<-read.csv('C:/Users/Louis/Desktop/dissertation stuff home/Indiadataedit.csv', header=TRUE, sep = ",")

dim(Indiaraw)
colnames(Indiaraw)

library(MASS)
library(stargazer)

Indiaraw$wealthy<-Indiaraw$wealth4+Indiaraw$wealth5
Indiaraw$generalpoor<-1-Indiaraw$wealthy
#model1<-glm(lesssevere~ first.cousin*generalpoor+first.cousin*wealthy+ second.cousin + uncle+
 #             brotherinlaw+ otherblood + othernonblood + wifeedu +
  #            muslim+ christian+ wealth + totalchildren+ husbanddrinks+
   #           drunkenness, data=Indiaraw)

model1<-glm(lesssevere~ first.cousin+ second.cousin + uncle+
             brotherinlaw+ otherblood + othernonblood + wifeedu +
            muslim+ christian+ wealth + totalchildren+ husbanddrinks+
           drunkenness, data=Indiaraw)
summary(model1)

#stargazer(model1, type="html", title="PHRII Survey Data")

model2<-glm(moresevere~ first.cousin+ second.cousin + uncle+
              brotherinlaw+ otherblood + othernonblood + wifeedu +
              muslim+ christian+ wealth + totalchildren+ husbanddrinks+
              drunkenness, data=Indiaraw)
summary(model2)

stargazer(model1, model2, title="India DHS", type="html")



model3<-glm(sexual~ first.cousin+ second.cousin + uncle+
              brotherinlaw+ otherblood + othernonblood + wifeedu +
              muslim+ christian+ wealth + totalchildren+ husbanddrinks+
              drunkenness, data=Indiaraw)
summary(model3)

Indiamuslim<-subset(Indiaraw, Indiaraw$muslim==1)

model4<-glm(lesssevere~ first.cousin+ second.cousin + uncle+
              brotherinlaw+ otherblood + othernonblood + 
              wifeedu +
              wealth + totalchildren+ husbanddrinks
              +drunkenness
            , data=Indiamuslim)
summary(model4)
dim(Indiamuslim)

