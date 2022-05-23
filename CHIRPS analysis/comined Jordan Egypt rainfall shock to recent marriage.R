Jordan_recent_marriage<-subset(Jordanrain_mar, Jordanrain_mar$yeardiff<5)
Egypt_recent_marriage<-subset(Egyptrain1, Egyptrain1$yeardiff<5)

model_extreme_rain_consang<-glm(firstcousin~Wealth+wifeeducation+
                                  husbandeducation+Childrenborn+childrenathome+v714wifeemployed+v705husbandemployed
                                +extreme_marriage_delta,
                                family=binomial(link='logit'),data=Jordan_recent_marriage)

model_extremerain_consang_Egypt<-glm(firstcousin~
                                       Wealth+
                                       wifeeducation+
                                       #Childrenborn+
                                       #Childrenathome+
                                       v714wifeemployed+
                                       v705husbandemployed+
                                       v130Christian+
                                       husbanddrinks
                                     +priormarriageextreme,
                                     family=binomial(link='logit'),data=Egypt_recent_marriage)

combined_recent_marriage<-subset(exper_merge, exper_merge$yeardiff<2)

model_extremerain_firstcousin_both<-glm(firstcousin~
                                       Wealth+
                                       wifeeducation+
                                       Childrenborn+
                                       Childrenathome+
                                       v714wifeemployed+
                                       v705husbandemployed
                                     +priormarriageextreme,
                                     family=binomial(link='logit'),data=combined_recent_marriage)

library(clusterSEs)


combined_recent_marriage<-transform(combined_recent_marriage, id=as.numeric(factor(combined_recent_marriage$latitude_x)))

model_extremerain_firstcousin_both_cluster<-cluster.bs.glm(model_extremerain_firstcousin_both, 
                                                           combined_recent_marriage, ~id, report=T)
#No rural subset is significant
#Let's try rural subset

combined_recent_marriage_rural<-subset(combined_recent_marriage, combined_recent_marriage$ruraldummy==1)


model_extremerain_firstcousin_both_rural<-glm(firstcousin~
                                          Wealth+
                                          wifeeducation+
                                          Childrenborn+
                                          Childrenathome+
                                          v714wifeemployed+
                                          v705husbandemployed
                                        +priormarriageextreme,
                                        family=binomial(link='logit'),data=combined_recent_marriage_rural)

model_extremerain_firstcousin_both_rural_cluster<-cluster.bs.glm(model_extremerain_firstcousin_both_rural, 
                                                                 combined_recent_marriage_rural, ~id, report=T)




summary(model_extremerain_consang_both)
stargazer(model_extremerain_firstcousin_both, model_extremerain_firstcousin_both_rural, 
          type="html", title="Jordan and Egypt Recent Marriage rainfall Shock")
stargazer(model_extremerain_firstcousin_both_cluster, model_extremerain_firstcousin_both_rural_cluster, 
          type="html")
