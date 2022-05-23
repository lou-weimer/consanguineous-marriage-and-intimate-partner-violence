

library(htmlTable)



output1<-matrix(nrow=2, ncol=3, byrow=TRUE, c("Yes", "Mostly No", "Mostly No", "In 'Extreme Rain' Operationalization&dagger;",
                                              "In 'Extreme Rain' Operationalization&dagger;", 
                                              "In 'Extreme rain' Operationalization"))

htmlTable(output1, align="c|",
          
          header =  c("Jordan", "Egypt", "India"),
          
          rnames = c("Rainfall Shock and IPV", "Rainfall Shock and Consanginity"),
          css.cell="padding-left:.5em; padding-right:.5em;",
          tfoot="&dagger; Egypt and Jordan analyzed jointly",
          cgroup = c("Brideprice", "Dowry"),
          n.cgroup = c(2,1),
          caption="Summary of Rainfall Shocks by Country")
