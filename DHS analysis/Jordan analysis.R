library(foreign)
Jordan12<-read.dta("C:/Users/U046466/Desktop/Project/Jordan/JOIR6CFL.DTA")
write.csv(Jordan12, "C:/Users/U046466/Desktop/Project/Jordan/Jordan12.csv")

Jordan12IR<-read.csv("C:/Users/Louis/Desktop/prospectus stuff home/Jordandata.csv")

Jordan12<-Jordan12IR[,c("firstcousin", "secondcousin", "otherrelative", "lessviolhus",
                       "severeviolhus", "Wealth", "wifeeducation", "husbandeducation", 
                       "Childrenborn", "childrenathome", "v714wifeemployed", 
                       "v705husbandemployed", "helpfatherinlaw", "helpmotherinlaw")]

#Consistent results. First cousin marriage is significant protective factor for both 
#less severe and severe violence. 
#In this case, second cousin marriage is also a significant protective factor
model1<-glm(severeviolhus~firstcousin+secondcousin+otherrelative+Wealth+wifeeducation+
              husbandeducation+Childrenborn+childrenathome+v714wifeemployed+v705husbandemployed,
            family=binomial(link='logit'),data=Jordan12,
            na.action=na.exclude)

#stargazer(model1, type="latex")



model2<-glm(lessviolhus~firstcousin+secondcousin+otherrelative+Wealth+wifeeducation+
              husbandeducation+Childrenborn+childrenathome+v714wifeemployed+v705husbandemployed,
            family=binomial(link='logit'),data=Jordan12,
            na.action=na.exclude)

stargazer(model2, model1, type="html", title="Jordan DHS")

#Do cousin marriages go to in laws for help? Nope
model3<-glm(helpfatherinlaw~firstcousin+secondcousin+otherrelative+Wealth+wifeeducation+
              husbandeducation+Childrenborn+childrenathome+v714wifeemployed+v705husbandemployed,
            family=binomial(link='logit'),data=Jordan12,
            na.action=na.exclude)

model4<-glm(helpmotherinlaw~firstcousin+secondcousin+otherrelative+Wealth+wifeeducation+
              husbandeducation+Childrenborn+childrenathome+v714wifeemployed+v705husbandemployed,
            family=binomial(link='logit'),data=Jordan12,
            na.action=na.exclude)
