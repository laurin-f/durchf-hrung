source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/event.R")

library(readxl)
library(stringr)
capath<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/ca/"
lfpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/leitf/"

kat17.10<-read.csv(paste0(capath,"ca_17.10.csv"),sep=";",skip=6,na.strings = "n.a.",stringsAsFactors = F)
lf<-read.csv(paste0(lfpfad,"lf_saugkerzen.csv"),sep=";",stringsAsFactors = F)

probe<-kat17.10[grep("T ?[12345]",kat17.10[,2]),2]
ca<-kat17.10[grep("T ?[12345]",kat17.10[,2]),7]

tiefe<-as.numeric(str_extract(str_extract(probe,"T ?[12345]"),"[12345]"))
datum<-str_extract(probe,"[123]..1[012]")
lf$datum<-str_extract(lf$datum,"[123]..1[012]")
ic<-merge(data.frame(tiefe=tiefe,ca=ca,datum=datum,stringsAsFactors = F),lf)

plot(ic$ca~ic$lf)
cafm<-glm(ca~log(lf),data = ic)
lfs<-seq(min(ic$lf),max(ic$lf),1)
preds<-predict(cafm,data.frame(lf=lfs))
lines(lfs,preds)
rsq<-(1-cafm$deviance/cafm$null.deviance)*100
text(170,60,paste("R² = ",round(rsq,2)))
save(cafm,file=paste0(capath,"cafm.R"))
     
ic$tiefenstufe<-ic$tiefe
ic$tiefe<-ifelse(ic$tiefenstufe==5,-17,-(ic$tiefenstufe*4-2))

ints<-event()
ints$datum<-format(ints$start,"%d.%m")
ic<-merge(ic,ints[,5:6])

library(ggplot2)
ggplot()+
  geom_path(data=ic,aes(ca,tiefe,col=as.factor(round(rain_mm_h))))+
  labs(x=expression("Ca"^{"2+"}*"  [mg * l"^{-1}*"]"),y="tiefe [cm]",col=expression("Intensität [mm*h"^{-1}*"]"))+theme_classic()

