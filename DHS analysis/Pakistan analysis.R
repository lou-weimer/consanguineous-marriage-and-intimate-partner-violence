library(foreign)
#Pak12Individual<-read.dta("C:/Users/U046466/Desktop/Project/Pakistan/pkIndividual recode61dt/PKIR61FL.DTA")
Pak12Individualex<-read.csv("C:/Users/Louis/Desktop/prospectus stuff home/Pakistandata.csv")
#Pak12Household<-read.dta("C:/Users/U046466/Desktop/Project/Pakistan/pkhousehold recode61dt/PKHR61FL.DTA")


In06Individual<-read.dta("C:/Users/U046466/Desktop/Project/India/DHS 5/iair52dt/IAIR52FL.dta")




#d114ed is drunkenness, V106ed is education, v190ed is wealth index,
#s111cousin is cousin marriage or not, d113 is husband drinker or not, 
#s113 is whether respondent had say in choosing husband or not
Pak12Individualbase<-Pak12Individualex[,c("v106","v106ed", "s110", 
                      "s111", "s111ed", "s113", "d106", "d107", "d108", "d114ed", "v190ed",
                      "s111firstcousin", "s111secondcousin", "s111other", "d113",
                      "baluchi", "baruhi", "punjabi","pushto", "sariaki", "sindhi", "urdu", 
                      "wifeedu", "s113", "v705husunemployed", "v714wifeemployed")]
Pak12Individualconsang<-subset(Pak12Individualbase, is.na(Pak12Individualbase$d106)==FALSE)

#Pak12Individualconsang<-subset(Pak12Individualconsang, Pak12Individualbase$s111ed!=9)
#Pak12Individualconsang<-subset(Pak12Individualconsang, Pak12Individualbase$d106!=NA)
#Pak12Individualconsang<-subset(Pak12Individualconsang, Pak12Individualbase$d107!=9)
#Pak12Individualconsang<-subset(Pak12Individualconsang, Pak12Individualbase$d107!=NA)
#Pak12Individualconsang<-subset(Pak12Individualconsang, Pak12Individualbase$s110!=9)
#Pak12Individualconsang<-subset(Pak12Individualconsang, Pak12Individualbase$s110!=NA)
#Pak12Individualconsang<-subset(Pak12Individualconsang, Pak12Individualbase$s11ed!=9)

Pak12edsub<-subset(Pak12Individualconsang, Pak12Individualconsang$v106ed==0|
Pak12Individualconsang$v106ed==1|Pak12Individualconsang$v106ed==2)
y<-xtabs(~s111ed+d106, data=Pak12Individualconsang)

x<-xtabs(~s111ed+d107, data=Pak12edsub)



#Good stuff below. Model 5 suggests that marrying a cousin is a protective factor for
#less severe violence. Model 6 suggests that it is protective for more sever violence, 
#though at .1 significance levels
#Model 8 is the one about predicting first cousin marriage, 
#in which having a say is a significant correlate
model5<-glm(d106~s111firstcousin+s111secondcousin+s111other+d114ed+v190ed+v106ed
            #+baluchi+baruhi+punjabi+pushto+sariaki+sindhi+urdu
            +wifeedu +v705husunemployed+v714wifeemployed, 
            family=binomial(link='logit'),data=Pak12Individualconsang,
            na.action=na.exclude)


library(stargazer)
stargazer(model5, type="html", title="Pakistan DHS")


model6<-glm(d107~s111firstcousin+s111secondcousin+s111other+d114ed+v190ed
            #+baluchi+baruhi+punjabi+pushto+sariaki+sindhi+urdu
            +wifeedu +v705husunemployed+v714wifeemployed, 
            family=binomial(link='logit'),data=Pak12Individualconsang,
            na.action=na.exclude)

stargazer(model5, model6, type="html", title="Pakistan DHS")

#Data for sexual violence is empty


#Now I'm going to subset the data to only include observations 
#where women got to have say in choice of husband

Pak12Individualchoose<-subset(Pak12Individualconsang, Pak12Individualconsang$s113==1)
model7<-glm(d106~v106ed+s111firstcousin+s111secondcousin+s111other+d114ed+d113+v190ed
            #+baluchi+baruhi+punjabi+pushto+sariaki+sindhi+urdu
            +wifeedu, 
            family=binomial(link='logit'),data=Pak12Individualchoose,
            na.action=na.exclude)


#And now to try a different DV--I expect that being able to choose your husband
#will lead to more consanguinity
#AQUI another big result. Having a voice in choosing your husband is significantly 
#positively associated with marrying a first cousin

model9<-glm(s111firstcousin~s113+d114ed+d113+v190ed
            #+baluchi+baruhi+punjabi+pushto+sariaki+sindhi+urdu
            +wifeedu +v705husunemployed+v714wifeemployed, 
            family=binomial(link='logit'),data=Pak12Individualconsang,
            na.action=na.exclude)


stargazer(model9, type="html", title="Pakistan DHS")




model10<-glm(s111secondcousin~v106ed+s113+d114ed+d113+v190ed
            #+baluchi+baruhi+punjabi+pushto+sariaki+sindhi+urdu
            +wifeedu +v705husunemployed+v714wifeemployed, 
            family=binomial(link='logit'),data=Pak12Individualconsang,
            na.action=na.exclude)
