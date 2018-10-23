
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


okt10<-read_all(datum="10.10",q=F)

okt15<-read_all("15.10","09:21")
tiefe2_15.10<-read_vaisala("tiefe2_15.10.2","C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/",aggregate = T,temp_line = 31,Sonde=2)
tiefe2_15.10$datum<-round_date(tiefe2_15.10$date,"min")
okt15<-merge(okt15,tiefe2_15.10,all=T)
okt15<-subset(okt15,date>="2018-10-15 09:06:31 CEST")

okt18<-read_all(datum="18.10",start = "09:30")

#okt22<-read_all(datum="22.10",start = "13:36")

all<-rbind(okt10[,1:6],okt15[,1:6],okt18[,1:6])
okt1518<-rbind(okt15[,1:6],okt18[,1:6])

events<-event()
events$datum<-format(events$start,"%d.%m")
###############################################################
#plots

plot_all(okt10)
plot_all(okt15,name="15.10_int50mm8h",height = 9)
plot_all(okt18,name="18.10_int50mm3h",height = 9)
plot_all(okt18[1:5])

plot_all(all,point = F)
plot_all(okt1518,point = F)#,name = "int50mm3h&50mm8h")

max(okt18$wasser,na.rm=T)
ts<-seq(events$start[events$datum=="18.10"]+60*60,max(okt18$date),by=60*60*4)
sub18<-okt18[which(okt18$date%in%ts),]
ggplot(sub18,tiefe,aes(CO2_raw,tiefe,col=as.factor(date)))+geom_path()

minokt10<-c(format(okt10$date,"%Y%m%d%H%M")[30:nrow(okt10)],rep("201910101010",29))

ggplot(okt10,aes(theta,CO2_raw,col=as.factor(tiefe)))+geom_point()
ggplot(okt15,aes(theta,CO2_raw,col=as.factor(tiefe)))+geom_point()

ggplot(okt10,aes(temp,CO2_raw,col=as.factor(tiefe)))+geom_point()

ggplot(okt10,aes(date,temp,col=as.factor(tiefe)))+geom_point()

ggplot(okt15,aes(temp,CO2_raw,col=as.factor(tiefe)))+geom_point()

ggplot(okt15,aes(date,temp,col=as.factor(tiefe)))+geom_point()

capath<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/ca/"
load(file=paste0(capath,"cafm.R"))
okt18$ca_conc<-predict(cafm,data.frame(lf=okt18$lf))
okt18$ca_conc[okt18$ca_conc<0]<-0
okt18$ca_mg<-okt18$ca_conc*okt18$q/1000#mg/l*ml/min<-mg

sum(okt18$ca_mg,na.rm = T)/max(okt18$wasser,na.rm = T)*1000
