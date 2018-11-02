
###########################################################################
#auswertung der Versuche

#auführen der R-skripte für die benötigten Funktionen
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/Versuchsdesign/read_vaisala.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_teta.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/waage.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/event.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_all.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/plot_all.R")

q_15.10<-read_waage("15.10",start = "09:21",mov_avg = 5)
#q_15.10$date[which(q_15.10$id==125):length(q_15.10$date)] <-q_15.10$date[which(q_15.10$id==125):length(q_15.10$date)]+10*60


okt10<-read_all(datum="10.10",qs=F)

okt15<-read_all("15.10","09:21")
tiefe2_15.10<-read_vaisala("tiefe2_15.10.2","C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/",aggregate = T,temp_line = 31,Sonde=2)
tiefe2_15.10$date<-round_date(tiefe2_15.10$date,"min")
okt15[okt15$tiefe==-6&okt15$date%in%tiefe2_15.10$date,3:5]<-tiefe2_15.10[2:4]
okt15<-subset(okt15,date>="2018-10-15 09:06:31 CEST")

okt18<-read_all(datum="18.10",start = "09:30")

okt22<-read_all(datum="22.10",start = "14:06")

#########################################################
#Zeitumstellung

co2pfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/"

#CO2_zeitumst<-read_vaisala(datum = "26.10",pfad=co2pfad,aggregate = F)
CO2_zeitumst$datechr<-format(CO2_zeitumst$date,usetz = T)
t1<-which(CO2_zeitumst$datechr=="2018-10-28 02:00:00 CET"&CO2_zeitumst$tiefe==-2)
CO2_zeitumst$date[t1[1]:(t1[2]-1)]<-CO2_zeitumst$date[t1[1]:(t1[2]-1)]-60*60
t2<-which(CO2_zeitumst$datechr=="2018-10-28 02:00:00 CET"&CO2_zeitumst$tiefe==-14)
CO2_zeitumst$date[t2[1]:(t2[2]-1)]<-CO2_zeitumst$date[t2[1]:(t2[2]-1)]-60*60
t3<-which(CO2_zeitumst$datechr=="2018-10-28 02:01:00 CET"&CO2_zeitumst$tiefe==-10)
CO2_zeitumst$date[t3[1]:(t3[2]-2)]<-CO2_zeitumst$date[t3[1]:(t3[2]-2)]-60*60
CO2_zeitumst<-CO2_zeitumst[-6]
#vektor mit Minutenwerten erstellen
min<-round_date(CO2_zeitumst$date,"min")
minchr<-format(min,usetz = T)
CO2_zeitumstmin<-aggregate(CO2_zeitumst,list(minchr,CO2_zeitumst$tiefe),mean)
out<-CO2_zeitumstmin[,-(1:2)]
save(out,file = paste0(co2pfad,"co2_26.10.R"))

bf26<-read_teta(pfad = bfpfad,name=paste0("bf_","26.10"))
datchr<-format(bf26$date,usetz = T)
datchr<-gsub("CET","CEST",datchr)
bf26$date<-with_tz(ymd_hms(datchr,tz="UTC")-2*60*60)

okt26<-read_all(datum="26.10",start = "10:03")
okt26<-okt26[,-(6:7)]
okt26$datechr<-format(okt26$date,usetz = T)
bf26$datechr<-format(bf26$date,usetz = T)
okt26<-merge(okt26,bf26,by=c("datechr","date","tiefe"),all=T)

okt26<-okt26[,c(1:5,16:17,6:15)+1]

tiefe17<-subset(okt26,tiefe==-17)
zeitumst<-format(tiefe17$date,usetz = T)
zwei_winterzeit<-min(grep("2018-10-28 02:00:00 CET",zeitumst))
zwei_sommerzeit<-min(grep("2018-10-28 02:00:00 CEST",zeitumst))

ende_sommerzeit<-length(tiefe17$lf)-(zwei_winterzeit-zwei_sommerzeit)
tiefe17[zwei_sommerzeit:ende_sommerzeit,]<-tiefe17[zwei_winterzeit:length(tiefe17$lf),]


okt26$lf[okt26$tiefe==-17]<-tiefe17$lf
okt26<-okt26[1:max(which(!is.na(okt26$theta))),]


okt31<-read_all(datum="31.10",start="12:42",qs=F,lfs=F)
##########################################################
#Alle in einen Datensatz
all<-rbind(okt15,okt18,okt22,okt26)
save(all,file="C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")

okt151822<-rbind(okt15[,1:6],okt18[,1:6],okt22[,1:6])

events<-event()
events$start
1.15*50
# 
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
ggplot(subset(all,tiefe%in%c(-10,-14)),aes(t_min,CO2,col=as.factor(treatment)))+geom_path()+facet_wrap(~tiefe,nrow = 2)

ggplot(subset(all,tiefe%in%c(-2,-6)),aes(t_min,CO2,col=as.factor(treatment)))+geom_path()+facet_wrap(~tiefe,nrow = 2)

#########################################################
#Einfluss des ein und auschaltens der Pumpe für die Saugkerzen
sub_saugkerz<-subset(all,date>"2018-10-31 07:00:00 CEST"&tiefe!=0)

ggplot(sub_saugkerz)+geom_line(aes(date,CO2,col=as.factor(tiefe)))+geom_vline(xintercept = as.numeric(ymd_hms(c("2018-10-31 09:26:00","2018-10-31 12:30:00"),tz="CET")))+facet_wrap(~tiefe,scales = "free",ncol=1)

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
plot_all(okt31)

plot_all(all[,1:6])#,name="alle",height = 6)
plot_all(all)#,name="alle_alles",height = 6)
plot_all(okt151822,point = F)#,name = "int50mm3h&50mm8h")



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





