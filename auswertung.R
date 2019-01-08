
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

luecke<-which(as.numeric(diff(okt22$date[!is.na(okt22$q)]))>60)
okt22$q_interpol[okt22$date>okt22$date[!is.na(okt22$q)][luecke]&okt22$date<okt22$date[!is.na(okt22$q)][luecke+1]]<-NA

#bei änderung der cafm unter zeitumstellung neu einladen 
load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/okt26.R")

okt31<-read_all(datum="31.10",start="12:42")


nov07<-read_all(datum="07.11",start="12:36")


nov14<-read_all(datum="14.11",start="10:06")

nov21<-read_all(datum="21.11",start="10:58")

nov29<-read_all(datum="29.11",start="12:31")

dez05<-read_all(datum="05.12",start="09:37")

dez11<-read_all(datum="11.12",start="11:55")

dez17<-read_all(datum="17.12",start="09:54")


##########################################################
#Alle in einen Datensatz
all_list<-list(okt15,okt18,okt22,okt26,okt31,nov07,nov14)
all<-rbind(okt15,okt18,okt22,okt26,okt31,nov07,nov14)
all_s<-rbind(okt18,okt22,okt26,okt31)

all_plot<-rbind(okt18,okt26,okt31)

alldist_list<-list(nov29,dez05,dez11,dez17)
alldist<-rbind(nov29,dez05,dez11,dez17)
alldist_s<-rbind(dez05,dez11,dez17)

alldist_plot<-rbind(dez11,dez17)

plot(alldist_s$ca_conc)

range_all_s<-range(all_s$date)

ca_mavg_l<-lapply(all_list,function(x) zoo::rollapply(x$ca_conc[!is.na(x$ca_conc)],50,mean,fill=NA))
ca_mavg<-do.call("c",ca_mavg_l)

all$ca_conc[!is.na(all$ca_conc)][c(2,diff(ca_mavg))>0.2|all$ca_conc[!is.na(all$ca_conc)]<50]<-NA

all_s<-all[all$date>=min(range_all_s)&all$date<=max(range_all_s),]

range_alldist_s<-range(alldist_s$date)

ca_mavg_l<-lapply(alldist_list,function(x) zoo::rollapply(x$ca_conc[!is.na(x$ca_conc)],50,mean,fill=NA))
ca_mavg<-do.call("c",ca_mavg_l)

alldist$ca_conc[!is.na(alldist$ca_conc)][c(2,diff(ca_mavg))>0.2|alldist$ca_conc[!is.na(alldist$ca_conc)]<50]<-NA

alldist_s<-alldist[alldist$date>=min(range_alldist_s)&alldist$date<=max(range_alldist_s),]
points(alldist_s$ca_conc,col=2)

save(all,all_s,alldist,alldist_s,all_list,file="C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")
plot(all$CO2)

#save(all,all_s,alldist,all_list,file="C:/Users/ThinkPad/Documents/Masterarbeit/daten/all_s.R")

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
##########################################################
library(ggplot2)
named<-setNames(as.character(c("Tiefe= -2 cm","-6 cm","-10 cm","-14 cm")),c(2,6,10,14))

all$CO2_raw[all$t_min<0]<-NA

ggplot(subset(all,tiefe%in%c(-2,-6,-10,-14)&!is.na(treatment)),aes(t_min/(60*24),CO2_raw,col=as.factor(treatment)))+geom_path()+facet_wrap(~(-tiefe),nrow = 4,scales = "free",labeller =  as_labeller(named))+theme_classic()+labs(x="Zeit [Tage]",y=expression("CO"[2]*" [ppm]"),col=expression("Intensität [mm h"^{-1}*"]"))+scale_x_continuous(breaks = 0:10)+ggsave(paste0(plotpfad,"mins_nach_Event.pdf"),width = 7,height = 7)

alldist$CO2_raw[alldist$t_min<0]<-NA

ggplot(subset(alldist,tiefe%in%c(-2,-6,-10,-14)&!is.na(treatment)),aes(t_min/(60*24),CO2_raw,col=as.factor(treatment)))+geom_path()+facet_wrap(~(-tiefe),nrow = 4,scales = "free",labeller =  as_labeller(named))+theme_classic()+labs(x="Zeit [Tage]",y=expression("CO"[2]*" [ppm]"),col=expression("Intensität [mm h"^{-1}*"]"))+scale_x_continuous(breaks = 0:10)+ggsave(paste0(plotpfad,"mins_nach_Event_dist.pdf"),width = 7,height = 7)

ggplot(subset(all,tiefe%in%c(-2,-6)),aes(t_min,CO2,col=as.factor(treatment)))+geom_path()+facet_wrap(~tiefe,nrow = 2)

#########################################################
#Einfluss des ein und auschaltens der Pumpe für die Saugkerzen
####################################################
tiefenstufen<--seq(2,14,by=4)
sub_saugkerz<-subset(all,date>"2018-10-31 08:00:00 CEST"&date<"2018-10-31 15:00:00 CEST"&tiefe%in%tiefenstufen)

ggplot(sub_saugkerz)+geom_line(aes(date,CO2,col=as.factor(tiefe)))+geom_vline(xintercept = as.numeric(ymd_hms(c("2018-10-31 09:26:00","2018-10-31 12:30:00"),tz="CET")))+facet_wrap(~tiefe,scales = "free",ncol=1)+theme_classic()+labs(col="tiefe")+ggsave(paste0(plotpfad,"Saugkerzen_leeren.pdf"),width = 7,height = 7)

plot(1,1)
dev.off()
ggplot(okt31)+geom_line(aes(date,CO2_raw,col=as.factor(tiefe)))
ggplot(okt31)+geom_line(aes(date,theta,col=as.factor(tiefe)))
#############################################################
#Übersichtsplots
##########################################################
plot_all(okt10)
dev.off()
plot_all(okt15)#,name="15.10_int50mm8h",height = 9)
plot_all(okt18,show.legend = F)#,name="okt18_3h",height = 9,width=4)
plot_all(okt18[1:5])
plot_all(okt22)#,name="22.10_int50mm3h",height = 9)
plot_all(okt22[,1:6])
plot_all(data=okt26,show.legend = F,ylabs=rep("",4),scale=F)#,name="okt26_8h",height = 9, width= 4)
plot_all(okt31,ylabs=rep("",4),scale=F)#,name="okt31_50h",height = 9,width = 6)
plot_all(nov07)#,name="07.11_int50mm50h",height = 9)
plot_all(nov14)#,name="14.11_int50mm50h",height = 9)

p1<-plot_all(okt18,show.legend = F,lfmin = 250)
p2<-plot_all(data=rbind(okt26,okt31),show.legend = T,ylabs=rep("",4),scale=F,lfmin = 250)
pdf(paste0(plotpfad,"uebersicht_messungen.pdf"),width = 8,height = 5.5)
grid.arrange(p1,p2,ncol=2,layout_matrix=rbind(c(rep(1,5),rep(2,12),4)))
dev.off()

plot_all(nov21)#,name="21.11_int50mm3h",height = 9)
plot_all(nov29)#,name="29.11_int50mm3h",height = 9)

plot_all(dez05)#,name="05.12_int50mm8h",height = 9)
plot_all(dez11)#,name="11.12_int50mm3h",height = 9)

plot_all(dez17)#,name="11.12_int50mm3h",height = 9)

plot_all(all[,1:6])#,name="alle",height = 6)
plot_all(all)#,name="alle_alles",height = 6)
plot_all(all_s)#,name="3850_alles",height = 6)


plot_all(alldist)

pdist<-plot_all(alldist_plot,height = 6,width = 7,lfmin=400)
pdf(paste0(plotpfad,"uebersicht_dist.pdf"),width = 8,height = 5.5)
grid.arrange(pdist,ncol=2,layout_matrix=rbind(c(rep(1,40),2)))
dev.off()
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

co2mean<-aggregate(all$CO2_raw,list(all$tiefe),function(x) mean(x,na.rm = T))

co2mean_dist<-aggregate(alldist$CO2_raw,list(alldist$tiefe),function(x) mean(x,na.rm = T))
co2range[,2][,1]
co2range<-aggregate(all$CO2_raw,list(all$tiefe),function(x) range(x,na.rm = T))
co2range_vals<-c(co2range[2:5,2][,1],rev(co2range[2:5,2][,2]))
range_undist<-data.frame(co2=co2range_vals,tiefe=c(co2range[2:5,1],rev(co2range[2:5,1])))
co2range_dist<-aggregate(alldist$CO2_raw,list(alldist$tiefe),function(x) range(x,na.rm = T))
co2range_vals_dist<-c(co2range_dist[2:5,2][,1],rev(co2range_dist[2:5,2][,2]))
range_dist<-data.frame(co2=co2range_vals_dist,tiefe=c(co2range_dist[2:5,1],rev(co2range_dist[2:5,1])))

ggplot()+geom_path(data=co2mean[2:5,],aes(x,Group.1,col="ungestört"))+geom_path(data=co2mean_dist[2:5,],aes(x,Group.1,col="gestört"))+geom_polygon(data=range_undist,aes(co2,tiefe,fill="ungestört"),col=0,alpha=0.3,show.legend = F)+geom_polygon(data=range_dist,aes(co2,tiefe,fill="gestört"),col=0,alpha=0.3,show.legend = F)+labs(x=expression("CO"[2]*"  [ppm]"),y="Tiefe [cm]",col="")+theme_classic()+ggsave(paste0(plotpfad,"co2_tiefenprofil.pdf"),width=6,height = 4.5)

