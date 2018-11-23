
###########################################################################
#auswertung der Versuche

#auführen der R-skripte für die benötigten Funktionen
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/Versuchsdesign/read_vaisala.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_teta.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/waage.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/event.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_all.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/plot_all.R")


plotpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/abbildungen/plots/"
#q_15.10<-read_waage("15.10",start = "09:21",mov_avg = 5)
#q_15.10$date[which(q_15.10$id==125):length(q_15.10$date)] <-q_15.10$date[which(q_15.10$id==125):length(q_15.10$date)]+10*60


okt10<-read_all(datum="10.10",qs=F)

okt15<-read_all("15.10","09:21")
tiefe2_15.10<-read_vaisala("tiefe2_15.10.2","C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/",aggregate = T,temp_line = 31,Sonde=2)
tiefe2_15.10$date<-round_date(tiefe2_15.10$date,"min")
okt15[okt15$tiefe==-6&okt15$date%in%tiefe2_15.10$date,3:5]<-tiefe2_15.10[2:4]
okt15<-subset(okt15,date>="2018-10-15 09:06:31 CEST")

okt18<-read_all(datum="18.10",start = "09:30")

okt22<-read_all(datum="22.10",start = "14:06")

#bei änderung der cafm unter zeitumstellung neu einladen 
load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/okt26.R")

okt31<-read_all(datum="31.10",start="12:42")


nov07<-read_all(datum="07.11",start="12:36")


nov14<-read_all(datum="14.11",start="10:06")

nov21<-read_all(datum="21.11",start="10:58",qs=F,lfs=F)



##########################################################
#Alle in einen Datensatz
all<-rbind(okt15,okt18,okt22,okt26,okt31,nov07,nov14)
#save(all,file="C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")

events<-event()

#####################################################
#thetas korrigieren
thetamax<-max(all$theta[all$tiefe==-14],na.rm=T)
all$theta_korr<-all$theta
for (i in c(-6,-10,-14)){
all$theta_korr[all$tiefe==i]<-(thetamax-max(all$theta[all$tiefe==i],na.rm = T))+all$theta[all$tiefe==i]}


plot(all$theta_korr)
ggplot(all)+geom_line(aes(date,theta_korr,col=as.factor(tiefe)))

ggplot(all)+geom_line(aes(date,theta,col=as.factor(tiefe)))


###############################################################
#plots

#Reaktion der Unteschiedlichen Tiefen in Miunten anch Event
library(ggplot2)
ggplot(subset(all,tiefe%in%c(-10,-14)),aes(t_min,CO2,col=as.factor(treatment)))+geom_path()+facet_wrap(~tiefe,nrow = 2)+theme_classic()+labs(col=expression("Intensität [mm h"^{-1}*"]"))+ggsave(paste0(plotpfad,"mins_nach_Event.pdf"),width = 7,height = 7)

ggplot(subset(all,tiefe%in%c(-2,-6)),aes(t_min,CO2,col=as.factor(treatment)))+geom_path()+facet_wrap(~tiefe,nrow = 2)

#########################################################
#Einfluss des ein und auschaltens der Pumpe für die Saugkerzen
tiefenstufen<--seq(2,14,by=4)
sub_saugkerz<-subset(all,date>"2018-10-31 08:00:00 CEST"&date<"2018-10-31 15:00:00 CEST"&tiefe%in%tiefenstufen)

ggplot(sub_saugkerz)+geom_line(aes(date,CO2,col=as.factor(tiefe)))+geom_vline(xintercept = as.numeric(ymd_hms(c("2018-10-31 09:26:00","2018-10-31 12:30:00"),tz="CET")))+facet_wrap(~tiefe,scales = "free",ncol=1)+theme_classic()+labs(col="tiefe")+ggsave(paste0(plotpfad,"Saugkerzen_leeren.pdf"),width = 7,height = 7)

plot(1,1)
dev.off()
ggplot(okt31)+geom_line(aes(date,CO2_raw,col=as.factor(tiefe)))
ggplot(okt31)+geom_line(aes(date,theta,col=as.factor(tiefe)))
#############################################################
#Übersichtsplots
plot_all(okt10)
plot_all(okt15)#,name="15.10_int50mm8h",height = 9)
plot_all(okt18)#,name="18.10_int50mm3h",height = 9)
plot_all(okt18[1:5])
plot_all(okt22)#,name="22.10_int50mm3h",height = 9)
plot_all(okt22[,1:6])
plot_all(okt26)#,name="26.10_int50mm8h",height = 9)
plot_all(okt31)#,name="31.10_int50mm50h",height = 9)
plot_all(nov07)#,name="07.11_int50mm50h",height = 9)
plot_all(nov14)#,name="14.11_int50mm50h",height = 9)

plot_all(nov21)#,name="21.11_int50mm3h",height = 9)

plot_all(all[,1:6])#,name="alle",height = 6)
plot_all(all)#,name="alle_alles",height = 6)
plot_all(okt151822,point = F)#,name = "int50mm3h&50mm8h")
plot(all$CO2_raw[all$tiefe==-2],type="l")


##############################################################
#tiefenprofil plot
ts<-seq(events$start[events$datum=="18.10"]+60*60,max(okt18$date),by=60*60*4)
sub18<-okt18[which(okt18$date%in%ts),]
ggplot(sub18,aes(CO2_raw,tiefe,col=as.factor(date)))+geom_path()



ggplot(okt10,aes(theta,CO2_raw,col=as.factor(tiefe)))+geom_point()
ggplot(okt15,aes(theta,CO2_raw,col=as.factor(tiefe)))+geom_point()

ggplot(okt10,aes(temp,CO2_raw,col=as.factor(tiefe)))+geom_point()

ggplot(okt10,aes(date,temp,col=as.factor(tiefe)))+geom_point()

ggplot(okt15,aes(temp,CO2_raw,col=as.factor(tiefe)))+geom_point()

ggplot(okt15,aes(date,temp,col=as.factor(tiefe)))+geom_point()


ggplot(all,aes(date,temp,col=as.factor(tiefe)))+geom_point()


