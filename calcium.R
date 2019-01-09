#nötiges script ausführen
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/event.R")

#packages laden
library(readxl)
library(stringr)
library(ggplot2)

#pfade festlegen
capath<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/ca/"
lfpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/leitf/"
plotpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/abbildungen/plots/"

#############################################
#daten einladen
#############################################

#IC dateien einlesen
kat17.10<-read.csv(paste0(capath,"ca_17.10.csv"),sep=";",skip=6,na.strings = "n.a.",stringsAsFactors = F)
kat05.11<-read.csv(paste0(capath,"ca_05.11.csv"),sep=";",skip=6,na.strings = "n.a.",stringsAsFactors = F)
kat23.11<-read.csv(paste0(capath,"ca_23.11.csv"),sep=";",skip=6,na.strings = "n.a.",stringsAsFactors = F)
kat_19.12<-read.csv(paste0(capath,"ca_19.12.csv"),sep=";",skip=6,na.strings = "n.a.",stringsAsFactors = F)
#IC datensätze Mergen
kat_all<-rbind(kat17.10,kat05.11,kat23.11,kat_19.12)

#Wassermenge Datei einlesen
wassermenge<-read_xlsx("C:/Users/ThinkPad/Documents/Masterarbeit/daten/waage/wassermenge_saugkerzen.xlsx")
#Datenpunkte bei denen datum und versuch nicht übereinstimmen rausschmeißen
wassermenge<-wassermenge[wassermenge$datum==wassermenge$Versuch,]

#LF daten einlesen
lf<-read.csv(paste0(lfpfad,"lf_saugkerzen.csv"),sep=";",stringsAsFactors = F)


##########################################################
#datensätze formatieren
###########################################################

#meine Proben im Format "T tiefe datum" aus dem IC datensatz auswählen
probe<-kat_all[grep("T ?[012345]",kat_all[,2]),2]
#die dazugehörigen Calcium-konzentrationen wählen
ca<-kat_all[grep("T ?[012345]",kat_all[,2]),7]

#Tiefen aus Probennahme ausschneiden
tiefe<-as.numeric(str_extract(str_extract(probe,"T ?[012345]"),"[012345]"))
#datum aus Probennahme auscchneiden
datum<-str_extract(probe,"[0123]..1[012]")

#datum im LF-datensatz auf gleiche formatierung bringen
lf$datum<-str_extract(lf$datum,"[0123]..1[012]")

#IC daten als  data.frame mit LF daten mergen
ic<-merge(data.frame(tiefe=tiefe,ca=ca,datum=datum,stringsAsFactors = F),lf,all=T)

#Regenwasser hat kein Datum
ic$datum[is.na(ic$datum)]<-"regen"

#plot calcium gegen lf
plot(ic$ca~ic$lf)
#Regression von calcium gegen lf
cafm<-glm(ca~lf,data = ic)
#regressionsgerade plotten
abline(cafm)

#regression summary anschauen
summary(cafm)
#R² ausrechnen
rsq<-(1-cafm$deviance/cafm$null.deviance)*100
#R² in plot schreiben
text(170,150,paste("R² = ",round(rsq,2)))


#tiefe von tiefenstufe in cm angabe umrechnen
#für wassermenge
wassermenge$tiefe<-wassermenge$tiefe*-4+2
wassermenge$tiefe[wassermenge$tiefe==-18]<--17
#und ic
ic$tiefenstufe<-ic$tiefe
ic$tiefe<-ifelse(ic$tiefenstufe==5,-17,-(ic$tiefenstufe*4-2))
ic$tiefe[ic$tiefe==2]<-0

#events einladen
ints<-event()
#aus eventsdatensatz intensitäten in IC datensatz übernehmen
ic<-merge(ic,ints[,6:7],all=T)
#ein nicht gelungenes Experiment entfernen
ic<-subset(ic,round(rain_mm_h)!=16)
#spalte treatment als gerundete intensität
ic$treatment<-round(ic$rain_mm_h)

#datum formatieren
ic$datum<-parse_date_time(paste0(2018,ic$datum),"ydm")
#alle versuche vor dem 29.11. waren mit der ungestörten Probe danach die gestörte
ic$sample<-ifelse(parse_date_time(ic$datum,"dm")>="0000-11-29 UTC","dist","undist")

#Wassermengenwerte an IC datensatz anhängen
ic<-merge(ic,wassermenge[,c(1,4,5)])
#Calcium mengen aus  ca konzentration  mal  wassermenge berechnen
ic$ca_mg<-ic$ca*ic$`wasser [ml]`/1000#mg/l*ml/1000->mg

#Ca mittelwerte der tiefenstufen und intensitäten berechnen 
icmean<-aggregate(ic[2:7],list(treatment=ic$treatment,tiefe=ic$tiefe,sample=ic$sample),mean)

#Ca range der tiefenstufen und intensitäten berechnen
icrange<-aggregate(ic[2:7],list(treatment=ic$treatment,tiefe=ic$tiefe,sample=ic$sample),range)
#datensatz einmal umgekehrt hintendran hängen um polygon zu plotten 
ic_poly<-rbind(icrange,icrange[nrow(icrange):1,])
#calium minima und maxima in den polygon datensatz
ic_poly$ca<-c(icrange$ca[,1],rev(icrange$ca[,2]))


###############################################
#Plots
###############################################

#namensvektor für label erstellen
names<-paste("Intensität =",unique(ic$treatment),"mm/h")
names[c(1,2)]<-paste(unique(ic$treatment)[c(1,2)],"mm/h")
named<-setNames(names,unique(ic$treatment))

#ca tiefenprofil nach intensität aufgeteilt
ggplot()+
  geom_path(data=icmean,aes(ca,tiefe,col=sample))+
  geom_point(data=subset(ic,!is.na(rain_mm_h)),aes(ca,tiefe,col=sample))+
  facet_wrap(~treatment,labeller = as_labeller(named))+labs(x=expression("Ca"^{"2+"}*"  [mg / l]"),y="Tiefe [cm]",col="Probe")+
  theme_bw()+
  ggsave(paste0(plotpfad,"ca_tiefenprofil2.pdf"),width = 8,height = 6)

#ca tiefenprofil nach Probe aufgeteilt
ggplot()+
  geom_polygon(data=ic_poly,aes(ca,tiefe,fill=as.factor(treatment)),col=0,alpha=0.2,show.legend = F)+
  geom_path(data=icmean,aes(ca,tiefe,col=as.factor(treatment)))+
  geom_point(data=subset(ic,!is.na(rain_mm_h)),aes(ca,tiefe,col=as.factor(treatment)))+
  facet_wrap(~sample,labeller = as_labeller(setNames(c("gestört","ungestört"),c("dist","undist"))),scales = "free_x")+
  labs(x=expression("Ca"^{"2+"}*"  [mg / l]"),y="Tiefe [cm]",col=expression("Intensität \n [mm / h]"))+
  theme_bw()+
  ggsave(paste0(plotpfad,"ca_tiefenprofil.pdf"),width = 7,height = 4)


####################################################
#plots ca ~ intensität

#min mean und max von ca über tiefe treat und probe aggregieren
icquant<-aggregate(ic[,c(3,10)],list(treatment=ic$treatment,tiefe=ic$tiefe,sample=ic$sample),function(x) quantile(x,c(0,0.5,1)))


caquant<-icquant$ca
camgquant<-icquant$ca_mg
icquant$ca_0<-caquant[,1]
icquant$ca_50<-caquant[,2]
icquant$ca_100<-caquant[,3]

icquant$camg_0<-camgquant[,1]
icquant$camg_50<-camgquant[,2]
icquant$camg_100<-camgquant[,3]

icquantrange<-rbind(icquant,icquant[nrow(icquant):1,])

icquantrange$ca<-c(icquant$ca_0,rev(icquant$ca_100))
icquantrange$ca_mg<-c(icquant$camg_0,rev(icquant$camg_100))

gest<-setNames(c("gestört","ungestört"),c("dist","undist"))
ggplot()+geom_polygon(data=icquantrange,aes(treatment,ca,fill=as.factor(tiefe)),col=0,alpha=0.2,show.legend = F)+geom_line(data=icquant,aes(treatment,ca_50,col=as.factor(tiefe)))+geom_point(data=ic,aes(rain_mm_h,ca,col=as.factor(tiefe)))+facet_wrap(~sample,labeller = as_labeller(gest),scales = "fixed")+labs(y=expression("Ca"^{"2+"}*"  [mg / l]"),x=expression("Intensität [mm / h]"),col="Tiefe [cm]")+theme_bw()+ggsave(paste0(plotpfad,"ca_int.pdf"),width = 7,height = 4)

ggplot(ic)+geom_polygon(data=icquantrange,aes(treatment,ca_mg,fill=as.factor(tiefe)),col=0,alpha=0.2,show.legend = F)+geom_line(data=icquant,aes(treatment,camg_50,col=as.factor(tiefe)))+geom_point(aes(rain_mm_h,ca_mg,col=as.factor(tiefe)))+facet_wrap(~sample,labeller = as_labeller(gest))+labs(y=expression("Ca"^{"2+"}*"  [mg]"),x=expression("Intensität [mm / h]"),col="Tiefe [cm]")+theme_bw()+ggsave(paste0(plotpfad,"ca_int_mg.pdf"),width = 7,height = 4)


##############################################
#datensatz und regression in r object speichern
save(ic,cafm,file=paste0(capath,"cafm.R"))
