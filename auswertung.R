
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
tiefe2_15.10<-read_vaisala("tiefe2_15.10.2",co2pfad,aggregate = T,temp_line = 31,Sonde=2)
okt15<-merge(okt15,tiefe2_15.10,all=T)
okt15<-subset(okt15,date>="2018-10-15 09:06:31 CEST")

event_10.10<-event("10.10","11:15","14:10",881.5)
event_15.10<-event("15.10","09:31","17:23",1267.2-380.6)
event_18.10<-event("18.10","09:37","12:33",1234.6-357.1)


###############################################################
#plots

plot_all(okt15,event_15.10)
plot_all(okt10,event_10.10)


ggplot(okt10,aes(theta,CO2_raw,col=as.factor(tiefe)))+geom_point()
ggplot(okt15,aes(theta,CO2_raw,col=as.factor(tiefe)))+geom_point()

ggplot(okt10,aes(temp,CO2_raw,col=as.factor(tiefe)))+geom_point()

ggplot(okt10,aes(date,temp,col=as.factor(tiefe)))+geom_point()

ggplot(okt15,aes(temp,CO2_raw,col=as.factor(tiefe)))+geom_point()

ggplot(okt15,aes(date,temp,col=as.factor(tiefe)))+geom_point()

