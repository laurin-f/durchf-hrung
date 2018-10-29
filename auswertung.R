
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

teset<-read_vaisala("tiefe2_22.10","C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/",aggregate = F,temp_line = 31,Sonde=2)[1:60000,]
plot(teset$date,teset$CO2_raw,type="l")
abline(v=events$start)
abline(v=events$stop)
teset2<-read_vaisala("tiefe1_22.10","C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/",aggregate = F,temp_line = 31,Sonde=1)[1:60000,]
plot(teset2$date,teset2$CO2_raw,type="l")
abline(v=events$start)
abline(v=events$stop)

teset3<-read_vaisala("tiefe3_22.10","C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/",aggregate = F,temp_line = 31,Sonde=3)[1:60000,]
plot(teset3$date,teset3$CO2_raw,type="l")
abline(v=events$start)
abline(v=events$stop)

teset4<-read_vaisala("tiefe4_22.10","C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/",aggregate = F,temp_line = 31,Sonde=4)[1:60000,]
plot(teset4$date,teset4$CO2_raw,type="l")
abline(v=events$start)
abline(v=events$stop)

okt10<-read_all(datum="10.10",qs=F)

okt15<-read_all("15.10","09:21")
tiefe2_15.10<-read_vaisala("tiefe2_15.10.2","C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/",aggregate = T,temp_line = 31,Sonde=2)
tiefe2_15.10$date<-round_date(tiefe2_15.10$date,"min")
okt15[okt15$tiefe==-6&okt15$date%in%tiefe2_15.10$date,3:5]<-tiefe2_15.10[2:4]
okt15<-subset(okt15,date>="2018-10-15 09:06:31 CEST")

okt18<-read_all(datum="18.10",start = "09:30")

okt22<-read_all(datum="22.10",start = "14:06")
okt26<-read_all(datum="26.10",start = "10:03")

all<-rbind(okt15,okt18,okt22,okt26)
save(all,file="C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")

okt151822<-rbind(okt15[,1:6],okt18[,1:6],okt22[,1:6])

events<-event()
events$start
###############################################################
#plots
library(ggplot2)
ggplot(subset(all,tiefe%in%c(-10,-14)),aes(t_min,CO2,col=as.factor(treatment)))+geom_path()+facet_wrap(~tiefe,nrow = 2)

ggplot(subset(all,tiefe%in%c(-2,-6)),aes(t_min,CO2,col=as.factor(treatment)))+geom_path()+facet_wrap(~tiefe,nrow = 2)


plot_all(okt10)
plot_all(okt15)#,name="15.10_int50mm8h",height = 9)
plot_all(okt18)#,name="18.10_int50mm3h",height = 9)
plot_all(okt18[1:5])
plot_all(okt22)#,name="22.10_int50mm3h",height = 9)
plot_all(okt22[,1:6])
plot_all(okt26)

plot_all(all[,1:6])
plot_all(okt151822,point = F)#,name = "int50mm3h&50mm8h")

max(okt18$wasser,na.rm=T)
ts<-seq(events$start[events$datum=="18.10"]+60*60,max(okt18$date),by=60*60*4)
sub18<-okt18[which(okt18$date%in%ts),]
ggplot(sub18,tiefe,aes(CO2_raw,tiefe,col=as.factor(date)))+geom_path()



ggplot(okt10,aes(theta,CO2_raw,col=as.factor(tiefe)))+geom_point()
ggplot(okt15,aes(theta,CO2_raw,col=as.factor(tiefe)))+geom_point()

ggplot(okt10,aes(temp,CO2_raw,col=as.factor(tiefe)))+geom_point()

ggplot(okt10,aes(date,temp,col=as.factor(tiefe)))+geom_point()

ggplot(okt15,aes(temp,CO2_raw,col=as.factor(tiefe)))+geom_point()

ggplot(okt15,aes(date,temp,col=as.factor(tiefe)))+geom_point()




