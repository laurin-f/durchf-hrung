source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/event.R")

library(readxl)
library(stringr)
capath<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/ca/"
lfpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/leitf/"

kat17.10<-read.csv(paste0(capath,"ca_17.10.csv"),sep=";",skip=6,na.strings = "n.a.",stringsAsFactors = F)
kat05.11<-read.csv(paste0(capath,"ca_05.11.csv"),sep=";",skip=6,na.strings = "n.a.",stringsAsFactors = F)

kat_all<-rbind(kat17.10,kat05.11)

lf<-read.csv(paste0(lfpfad,"lf_saugkerzen.csv"),sep=";",stringsAsFactors = F)

probe<-kat_all[grep("T ?[012345]",kat_all[,2]),2]
ca<-kat_all[grep("T ?[012345]",kat_all[,2]),7]

tiefe<-as.numeric(str_extract(str_extract(probe,"T ?[012345]"),"[012345]"))

datum<-str_extract(probe,"[123]..1[012]")
lf$datum<-str_extract(lf$datum,"[123]..1[012]")

ic<-merge(data.frame(tiefe=tiefe,ca=ca,datum=datum,stringsAsFactors = F),lf,all=T)

ic$datum[is.na(ic$datum)]<-"regen"

plot(ic$ca~ic$lf)
cafm<-glm(ca~lf,data = ic)
lfs<-seq(min(ic$lf,na.rm = T),max(ic$lf,na.rm = T),1)
preds<-predict(cafm,data.frame(lf=lfs))
lines(lfs,preds)
rsq<-(1-cafm$deviance/cafm$null.deviance)*100
text(170,60,paste("linear R² = ",round(rsq,2)))


ic$tiefenstufe<-ic$tiefe
ic$tiefe<-ifelse(ic$tiefenstufe==5,-17,-(ic$tiefenstufe*4-2))
ic$tiefe[ic$tiefe==2]<-0
ints<-event()

ic<-merge(ic,ints[,6:7],all=T)

library(ggplot2)
ggplot()+
  geom_point(data=ic,aes(ca,tiefe,col=as.factor(round(rain_mm_h))))+
  labs(x=expression("Ca"^{"2+"}*"  [mg * l"^{-1}*"]"),y="tiefe [cm]",col=expression("Intensität [mm*h"^{-1}*"]"))+theme_classic()

lf<-merge(lf,ints[,6:7])
plot(lf$lf,lf$tiefe)

ggplot(lf)+geom_point(aes(lf,-tiefe,shape=as.factor(round(rain_mm_h)),col=as.factor(round(rain_mm_h))))

ggplot(lf)+geom_point(aes(rain_mm_h,lf,shape=as.factor(tiefe),col=as.factor(tiefe)))

save(ic,cafm,file=paste0(capath,"cafm.R"))
