#must have all the images from Egypt, Jordan, and Pakistan

#Pakistan first

altPakless<-glm(d106~v106ed+s111firstcousin+s111secondcousin+s111other+d114ed+v190ed
                +baluchi+baruhi+punjabi+pushto+sariaki+sindhi+urdu
                +wifeedu +v705husunemployed+v714wifeemployed, 
                family=binomial(link='logit'),data=Pak12Individualconsang,
                na.action=na.exclude)


library(stargazer)
stargazer(altPakless, type="latex")


altPakless2<-glm(d106~v106ed+s111firstcousin+d114ed+v190ed
                #+baluchi+baruhi+punjabi+pushto+sariaki+sindhi+urdu
                +wifeedu +v705husunemployed+v714wifeemployed, 
                family=binomial(link='logit'),data=Pak12Individualconsang,
                na.action=na.exclude)

stargazer(altPakless2, type="latex")

#Pakistan more severe

altPakmore<- glm(d107~v106ed+s111firstcousin+s111secondcousin+s111other+d114ed+v190ed
                 +baluchi+baruhi+punjabi+pushto+sariaki+sindhi+urdu
                 +wifeedu +v705husunemployed+v714wifeemployed, 
                 family=binomial(link='logit'),data=Pak12Individualconsang,
                 na.action=na.exclude)
stargazer(altPakmore, type="latex")


altPakmore2<-glm(d107~v106ed+s111firstcousin+d114ed+v190ed
                 #+baluchi+baruhi+punjabi+pushto+sariaki+sindhi+urdu
                 +wifeedu +v705husunemployed+v714wifeemployed, 
                 family=binomial(link='logit'),data=Pak12Individualconsang,
                 na.action=na.exclude)

stargazer(altPakmore2)
