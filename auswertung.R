
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/Versuchsdesign/read_vaisala.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_teta.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/merge_data.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/waage.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/event.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_all.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/plot_all.R")

q_15.10<-read_waage("15.10",start = "09:21",mov_avg = 5)
#q_15.10$date[which(q_15.10$id==125):length(q_15.10$date)] <-q_15.10$date[which(q_15.10$id==125):length(q_15.10$date)]+10*60


okt10<-read_all("10.10",q=F)

okt15<-read_all("15.10","09:21")
tiefe2_15.10<-read_vaisala("tiefe2_15.10.2","C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/",aggregate = T,temp_line = 31,Sonde=2)
okt15<-merge(okt15,tiefe2_15.10,all=T)
okt15<-subset(okt15,date>="2018-10-15 09:06:31 CEST")

okt18<-read_all(datum="18.10",start = "09:30")

all<-rbind(okt10[,1:6],okt15[,1:6],okt18[,1:6])
okt1518<-rbind(okt15[,1:6],okt18[,1:6])

event_10.10<-event("10.10","11:15","14:10",881.5)
event_15.10<-event("15.10","09:31","17:23",1267.2-380.6)
event_18.10<-event("18.10","09:37","12:33",1234.6-357.1)
event_22.10<-event("22.10","13:38","16:34",1263.5-397.9)

event_all<-rbind(event_10.10,event_15.10,event_18.10)

###############################################################
#plots

plot_all(okt10,event_10.10)
plot_all(okt15,event_15.10)
plot_all(okt18,event_18.10,name="18.10_int50mm3h")
plot_all(okt18[1:5],event_18.10)

plot_all(all,event_all,point = T)
plot_all(okt1518,event_all,point = F,name = "int50mm3h&50mm8h")

max(okt18$wasser,na.rm=T)
ts<-seq(event_18.10$start,max(okt18$date),by=60*60*12)
sub18<-okt18[which(okt18$date%in%ts),]
ggplot(sub18,aes(CO2_raw,tiefe,col=as.factor(date)))+geom_line()

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

sum(okt18$ca,na.rm = T)/max(okt18$wasser,na.rm = T)*1000
