load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")

library(readxl)
library(stringr)
waagepath<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/waage/"
saugkerzen<-read_xlsx(paste0(waagepath,"wassermenge_saugkerzen.xlsx"))
sub<-subset(saugkerzen,tiefe==5&datum==Versuch)
sub$datum<-format(sub$datum,"%m.%d")

wasser_transf<-do.call("c",lapply(all_list,function(x) max(x$wasser,na.rm=T)))
date_transf<-do.call("c",lapply(all_list,function(x) format(min(x$date,na.rm=T),"%m.%d")))
transf<-data.frame(wasser_transf,datum=date_transf)
merged<-merge(sub,transf)
plot(merged$`wasser [ml]`,merged$wasser_transf)
abline(1,1)

