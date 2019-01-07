source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/event.R")

#packages laden
library(readxl)
library(stringr)

#pfade festlegen
capath<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/ca/"
lfpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/leitf/"
plotpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/abbildungen/plots/"


#dateien einlesen
kat17.10<-read.csv(paste0(capath,"ca_17.10.csv"),sep=";",skip=6,na.strings = "n.a.",stringsAsFactors = F)
kat05.11<-read.csv(paste0(capath,"ca_05.11.csv"),sep=";",skip=6,na.strings = "n.a.",stringsAsFactors = F)
kat23.11<-read.csv(paste0(capath,"ca_23.11.csv"),sep=";",skip=6,na.strings = "n.a.",stringsAsFactors = F)
kat_19.12<-read.csv(paste0(capath,"ca_19.12.csv"),sep=";",skip=6,na.strings = "n.a.",stringsAsFactors = F)

wassermenge<-read_xlsx("C:/Users/ThinkPad/Documents/Masterarbeit/daten/waage/wassermenge_saugkerzen.xlsx")
wassermenge$tiefe<-wassermenge$tiefe*-4+2
wassermenge$tiefe[wassermenge$tiefe==-18]<--17
wassermenge<-wassermenge[wassermenge$datum==wassermenge$Versuch,]

kat_all<-rbind(kat17.10,kat05.11,kat23.11,kat_19.12)

lf<-read.csv(paste0(lfpfad,"lf_saugkerzen.csv"),sep=";",stringsAsFactors = F)

probe<-kat_all[grep("T ?[012345]",kat_all[,2]),2]
ca<-kat_all[grep("T ?[012345]",kat_all[,2]),7]

tiefe<-as.numeric(str_extract(str_extract(probe,"T ?[012345]"),"[012345]"))

datum<-str_extract(probe,"[0123]..1[012]")
lf$datum<-str_extract(lf$datum,"[0123]..1[012]")

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
ic<-subset(ic,round(rain_mm_h)!=16)
ic$treatment<-round(ic$rain_mm_h)
ic$sample<-ifelse(parse_date_time(ic$datum,"dm")>="0000-11-29 UTC","dist","undist")
ic$datum<-parse_date_time(paste0(2018,ic$datum),"ydm")
ic<-merge(ic,wassermenge[,c(1,4,5)])
ic$ca_mg<-ic$ca*ic$`wasser [ml]`/1000#mg/l*ml/1000->mg

icmean<-aggregate(ic[2:7],list(ic$treatment,ic$tiefe,ic$sample),mean)
names<-paste("Intensität =",unique(ic$treatment),"mm/h")
names[c(1,2)]<-paste(unique(ic$treatment)[c(1,2)],"mm/h")
named<-setNames(names,unique(ic$treatment))
library(ggplot2)
legendtitle<-expression("Intensität [mm*h"^{-1}*"]")
ggplot()+geom_path(data=icmean,aes(ca,tiefe,col=Group.3))+
  geom_point(data=subset(ic,!is.na(rain_mm_h)),aes(ca,tiefe,col=sample))+facet_wrap(~treatment,labeller = as_labeller(named))+labs(x=expression("Ca"^{"2+"}*"  [mg * l"^{-1}*"]"),y="Tiefe [cm]",col="Probe")+theme_bw()+ggsave(paste0(plotpfad,"ca_tiefenprofil.pdf"),width = 8,height = 6)

ggplot(ic)+geom_point(aes(rain_mm_h,ca,col=as.factor(tiefe)))+facet_wrap(~sample)+geom_smooth(aes(rain_mm_h,ca,col=as.factor(tiefe)),method = "glm",se=F,linetype=1)+labs(y=expression("Ca"^{"2+"}*"  [mg * l"^{-1}*"]"),x=expression("Intensität [mm*h"^{-1}*"]"),col="Tiefe [cm]")+theme_bw()+ggsave(paste0(plotpfad,"ca_intensität.pdf"),width = 8,height = 6)

ggplot(ic)+geom_point(aes(rain_mm_h,ca_mg,col=as.factor(tiefe)))+facet_wrap(~sample)+geom_smooth(aes(rain_mm_h,ca_mg,col=as.factor(tiefe)),method = "glm",se=F,linetype=1)+labs(y=expression("Ca"^{"2+"}*"  [mg]"),x=expression("Intensität [mm*h"^{-1}*"]"),col="Tiefe [cm]")+theme_bw()+ggsave(paste0(plotpfad,"ca_intensität_mg.pdf"),width = 8,height = 6)

save(ic,cafm,file=paste0(capath,"cafm.R"))
