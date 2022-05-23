
#Jordan


model_rain_consang_Jo<-glm(firstcousin~Wealth+wifeeducation+
                          husbandeducation+Childrenborn+childrenathome+v714wifeemployed+v705husbandemployed
                        +priorstd,
                        family=binomial(link='logit'),data=Jordanrain,
                        na.action=na.exclude)


model_rain_consang_Jo1<-glm(severeviolhus~Wealth+wifeeducation+
                          husbandeducation+Childrenborn+childrenathome+v714wifeemployed+v705husbandemployed
                        +priorstd,
                        family=binomial(link='logit'),data=Jordanrain,
                        na.action=na.exclude)

model_rain_consang_Jo2<-glm(lessviolhus~Wealth+wifeeducation+
                          husbandeducation+Childrenborn+childrenathome+v714wifeemployed+v705husbandemployed
                        +priorstd,
                        family=binomial(link='logit'),data=Jordanrain,
                        na.action=na.exclude)

library(stargazer)
stargazer(model_rain_consang_Jo, model_rain_consang_Jo1, model_rain_consang_Jo2, align=TRUE, 
          title="Prior Std. Tests in Jordan", type="html")

#Egypt

Egypt_prior_stdEgypt1<-glm(firstcousin~
                            Wealth+
                            wifeeducation+
                            Childrenborn+
                            Childrenathome+
                            v714wifeemployed+
                            v705husbandemployed+
                            v130Christian+
                            husbanddrinks
                          +priorstd,
                          family=binomial(link='logit'),data=Egyptrain1,
                          na.action=na.exclude)


Egypt_prior_stdEgypt2<-glm(severeviolhus~
                                   Wealth+
                                   wifeeducation+
                                   Childrenborn+
                                   Childrenathome+
                                   v714wifeemployed+
                                   v705husbandemployed+
                                   v130Christian+
                                   husbanddrinks
                                 +priorstd,
                                 family=binomial(link='logit'),data=Egyptrain1,
                                 na.action=na.exclude)


Egypt_prior_stdEgypt3<-glm(lessviolhus~
                             Wealth+
                             wifeeducation+
                             Childrenborn+
                             Childrenathome+
                             v714wifeemployed+
                             v705husbandemployed+
                             v130Christian+
                             husbanddrinks
                           +priorstd,
                           family=binomial(link='logit'),data=Egyptrain1,
                           na.action=na.exclude)


library(stargazer)
stargazer(Egypt_prior_stdEgypt1, Egypt_prior_stdEgypt2, Egypt_prior_stdEgypt3, align=TRUE, 
          title="Prior Std. Tests in Egypt", type="html")


