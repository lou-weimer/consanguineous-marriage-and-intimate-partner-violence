#run after running the code for the "combined analysis" file in the rainfall stuff folder. The datasets
#referenced below are created in that file

Egypt_comb<-Egyptrain1[,c("severeviolhus", "lessviolhus", "consang", "firstcousin", "husbanded", "wifeeducation",
                           "Wealth", "Childrenborn", "Childrenathome", "v705husbandemployed", "v714wifeemployed",
                          "longitude_x", "latitude_x",
                          "extremerain", "ruraldummy", "priormarriageextreme", "yeardiff", "priorstd", "rainfalldeltaabs")]

Jordan_comb<-Jordanrain_mar[,c("severeviolhus", "lessviolhus", "consanguinity",
                                "firstcousin", "husbandeducation", 
                                "wifeeducation", 
                                "Wealth", "Childrenborn", "childrenathome", "v705husbandemployed", "v714wifeemployed",
                                "longitude_x", "latitude_x", "extremerain", "ruraldummy",
                                "extreme_marriage_delta", "yeardiff", "priorstd", "rainfalldeltaabs", "rainfalldeltaabs")]

colnames(Jordan_comb)<-c("severeviolhus", "lessviolhus", "consang", "firstcousin", "husbanded", "wifeeducation",
                         "Wealth", "Childrenborn", "Childrenathome", "v705husbandemployed", "v714wifeemployed",
                         "longitude_x", "latitude_x",
                         "extremerain", "ruraldummy", "priormarriageextreme", "yeardiff", "priorstd", "rainfalldeltaabs")
Jordan_comb<-Jordan_comb[,c(1:19)]

exper_merge<-rbind(Egypt_comb, Jordan_comb)

combined_model<-glm(severeviolhus~
                                       Wealth+
                                       wifeeducation+
                                       #Childrenborn+
                                       #Childrenathome+
                                       v714wifeemployed+
                                       v705husbandemployed+
                                     +priormarriageextreme,
                                     family=binomial(link='logit'),data=exper_merge)


