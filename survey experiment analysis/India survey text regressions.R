library(readxl)

India_text<-read_excel("C:/Users/Louis/Desktop/dissertation stuff home/PDEL consang marriage/data and analysis/datanewformat.xlsx")

India_text_support_daughter<-glm(parents_support_son~
                                   violencelevel+
                                   firstcousin+
                                   education+
                                   livewithhusband,
                                family=binomial(link='logit'),data=India_text)

India_text_consang<-filter(India_text, experimentconsang==1)

India_text_support_daughter<-glm(parents_support_daughter_in_law~
                                   violencelevel+
                                   firstcousin+
                                   education+
                                   livewithhusband,
                                 family=binomial(link='logit'),data=India_text_consang)
