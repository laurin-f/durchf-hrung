
###########################################################################
#auswertung der Versuche

#auführen der R-skripte für die benötigten Funktionen
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/Versuchsdesign/read_vaisala.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_teta.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/waage.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/event.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_all.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/plot_all.R")

#plotpfad festlegen
plotpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/abbildungen/plots/"

##################################################################
#daten einlesen
#################################################################
okt10<-read_all(datum="10.10",qs=F)

okt15<-read_all("15.10","09:21")
#messlücke einladen
tiefe2_15.10<-read_vaisala("tiefe2_15.10.2","C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/",aggregate = T,temp_line = 31,Sonde=2)
#datum runden
tiefe2_15.10$date<-round_date(tiefe2_15.10$date,"min")
#messlücke in datensatz einfügen
okt15[okt15$tiefe==-6&okt15$date%in%tiefe2_15.10$date,3:5]<-tiefe2_15.10[2:4]
#anfang abschneiden
okt15<-subset(okt15,date>="2018-10-15 09:06:31 CEST")

okt18<-read_all(datum="18.10",start = "09:30")
okt22<-read_all(datum="22.10",start = "14:06")

#Messlücke von q-werten auswählen
luecke<-which(as.numeric(diff(okt22$date[!is.na(okt22$q)]))>60)
#interpolierte q-werte während der Messlücke entfernen 
okt22$q_interpol[okt22$date>okt22$date[!is.na(okt22$q)][luecke]&okt22$date<okt22$date[!is.na(okt22$q)][luecke+1]]<-NA

#Datensatz okt26 wird wegen Zeitumstellung im script zeitumstelllung bearbeitet
#bei änderung der cafm unter zeitumstellung neu einladen 
load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/okt26.R")

#restliche Events ohne probleme einladen
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
#########################################################

#unterschiedliche zusammengefasst ungestörte Probe
all_list<-list(okt15,okt18,okt22,okt26,okt31,nov07,nov14)
all<-rbind(okt15,okt18,okt22,okt26,okt31,nov07,nov14)
all_s<-rbind(okt18,okt22,okt26,okt31)
all_plot<-rbind(okt18,okt26,okt31)

#unterschiedliche zusammengefasst gestörte Probe
alldist_list<-list(nov29,dez05,dez11,dez17)

alldist<-rbind(nov29,dez05,dez11,dez17)
alldist$CO2_raw[alldist$tiefe==-10&alldist$date<min(dez11$date)]<-NA
alldist_s<-rbind(dez05,dez11,dez17)
alldist_plot<-rbind(dez11,dez17)


################################
#entfernen zu geringer Ca-Werte
#die durch nicht ganz  gefüllte LF-Kammer beim Anstieg enstehen
#################################

#Zeitraum von all_s
range_all_s<-range(all_s$date)

#gleitendes Mittel  über Ca-Werte (ohne NAs) aller Events in Liste "all_list"
ca_mavg_l<-lapply(all_list,function(x) zoo::rollapply(x$ca_conc[!is.na(x$ca_conc)],50,mean,fill=NA))
#Listenelemte aneinanderhängen
ca_mavg<-do.call("c",ca_mavg_l)

#von allen Ca-werten diejenigen mit einer Steigung über 0.2 oder mit Wert unter 50 entfernen 
all$ca_conc[!is.na(all$ca_conc)][c(2,diff(ca_mavg))>0.2|all$ca_conc[!is.na(all$ca_conc)]<50]<-NA

#die änderung auch für all_s übernehmen
all_s<-all[all$date>=min(range_all_s)&all$date<=max(range_all_s),]

#dasselbe auch für die gestörte Probe
#zeitraum wählen
range_alldist_s<-range(alldist_s$date)

#gleitendes Mittel
ca_mavg_l<-lapply(alldist_list,function(x) zoo::rollapply(x$ca_conc[!is.na(x$ca_conc)],50,mean,fill=NA))
#liste auspacken
ca_mavg<-do.call("c",ca_mavg_l)

#zu hohe steigung und zu niedrige ca-werte raus
alldist$ca_conc[!is.na(alldist$ca_conc)][c(2,diff(ca_mavg))>0.2|alldist$ca_conc[!is.na(alldist$ca_conc)]<50]<-NA

#für alldist_s übernehmen
alldist_s<-alldist[alldist$date>=min(range_alldist_s)&alldist$date<=max(range_alldist_s),]

#speichern der Datensätze in all.R
save(all,all_s,alldist,alldist_s,all_list,file="C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")



###############################################################
#plots

#Reaktion der Unteschiedlichen Tiefen in Minuten nach Event
##########################################################
#package laden
library(ggplot2)
#events laden
events<-event()


cols<-scales::hue_pal()(3)

??hue_pal
#namen vektor für labeller
named_tief<-setNames(as.character(c("Tiefe= -2 cm","-6 cm","-10 cm","-14 cm")),c(2,6,10,14))

#Anfangswerte NA setzten damit linien der unterschiedlichen Events nicht verbunden werden
all$CO2_raw[all$t_min<0]<-NA

#CO2 t_min plot undist
ggplot(subset(all,tiefe%in%c(-2,-6,-10,-14)&!is.na(treatment)),aes(t_min/(60*24),CO2_raw,col=as.factor(treatment)))+
  geom_path()+
  facet_wrap(~(-tiefe),nrow = 2,scales = "free",labeller =  as_labeller(named_tief))+
  theme_classic()+
  labs(x="Zeit [Tage]",y=expression("CO"[2]*" [ppm]"),col="Intensität \n [mm / h]")+
  scale_x_continuous(breaks = 0:10,limits = c(0,max(all$t_min/(60*24))))+
  scale_color_manual(values = cols[c(2,1,3)])+
  ggsave(paste0(plotpfad,"mins_nach_Event.pdf"),width = 7,height = 5)

#anfangswerte NA auch für dist
alldist$CO2_raw[alldist$t_min<0]<-NA


#CO2 t_min plot dist
ggplot(subset(alldist,tiefe%in%c(-2,-6,-10,-14)&!is.na(treatment)),aes(t_min/(60*24),CO2_raw,col=as.factor(treatment)))+
  geom_path()+
  facet_wrap(~(-tiefe),nrow = 2,scales = "free",labeller =  as_labeller(named_tief))+
  theme_classic()+
  labs(x="Zeit [Tage]",y=expression("CO"[2]*" [ppm]"),col="Intensität \n [mm / h]")+
  scale_x_continuous(breaks = 0:10,limits = c(0,max(alldist$t_min/(60*24))))+
  scale_color_manual(values = cols[c(1,3)])+
  ggsave(paste0(plotpfad,"mins_nach_Event_dist.pdf"),width = 7,height = 5)

######################
#lf plots
#######################

#anfangswerte NA
all$ca_conc[all$t_min<0]<-NA

#LF t_min plot undist
ggplot(subset(all,tiefe==-17&!is.na(treatment)&(date<min(nov07$date)|date>max(nov07$date))),aes(t_min/(60*24),ca_conc,col=as.factor(treatment)))+
  geom_path()+
  theme_classic()+
  labs(x="Zeit [Tage]",y=expression("Ca"^{"2+"}*"  [mg / l]"),col=expression("Intensität \n [mm / h]"))+
  scale_x_continuous(breaks = 0:10)+
  scale_color_manual(values = cols[c(2,1,3)])+
  facet_wrap(~treatment,ncol=1)+
  ggsave(paste0(plotpfad,"ca_t_min.pdf"),width = 6,height = 5)

#events laden
events<-event()

#zeitspanne ausschneiden
event<-subset(events,start>=min(alldist$date)&stop<=max(alldist$date))

#Lf plot dist
ggplot(subset(alldist,tiefe==-17))+
  geom_line(aes(date,ca_conc,col=as.factor(treatment)))+
  labs(x="",y=expression("Ca"^{"2+"}*"  [mg / l]"),col="Intensität \n [mm / h]")+
  geom_rect(data=event,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  scale_color_manual(values = cols[c(1,3)])+
  scale_fill_manual(name="Beregnung",values="blue")+
  theme_classic()+
  ggsave(paste0(plotpfad,"ca_t_min_dist.pdf"),width = 6,height = 3)




#########################################################
#Einfluss des ein und auschaltens der Pumpe für die Saugkerzen
####################################################
tiefenstufen<--seq(2,14,by=4)
sub_saugkerz<-subset(all,date>"2018-10-31 08:00:00 CEST"&date<"2018-10-31 15:00:00 CEST"&tiefe%in%tiefenstufen)

ggplot(sub_saugkerz)+geom_line(aes(date,CO2,col=as.factor(tiefe)))+geom_vline(xintercept = as.numeric(ymd_hms(c("2018-10-31 09:26:00","2018-10-31 12:30:00"),tz="CET")))+facet_wrap(~tiefe,scales = "free",ncol=1)+theme_classic()+labs(col="tiefe")+ggsave(paste0(plotpfad,"Saugkerzen_leeren.pdf"),width = 7,height = 7)


#############################################################
#Übersichtsplots
##########################################################
#einzelevents
plot_all(okt10)
plot_all(okt15)
plot_all(okt18)
plot_all(okt22)
plot_all(okt26)
plot_all(okt31)
plot_all(nov07)
plot_all(nov14)

plot_all(nov21)
plot_all(nov29)
plot_all(dez05)
plot_all(dez11)
plot_all(dez17)

#zusammengefasste Events
plot_all(all[,1:6])#,name="alle",height = 6)
p<-plot_all(all,date_breaks = F)
plot_all(all,name="alle_alles",height = 8,date_breaks = F)
plot_all(all_s)#,name="3850_alles",height = 6)
plot_all(alldist,name="alledist",height=8,date_breaks = F)

#Ausgewählte Events für die Arbeit
p1<-plot_all(okt18,show.legend = F,lfmin = 250)
p2<-plot_all(data=rbind(okt26,okt31),show.legend = T,ylabs=rep("",4),scale=F,lfmin = 250)
pdf(paste0(plotpfad,"uebersicht_messungen.pdf"),width = 8,height = 5.5)
grid.arrange(p1,p2,ncol=2,layout_matrix=rbind(c(rep(1,5),rep(2,12),4)))
dev.off()

pdist<-plot_all(alldist_plot,height = 6,width = 7,lfmin=400)
pdf(paste0(plotpfad,"uebersicht_dist.pdf"),width = 8,height = 5.5)
grid.arrange(pdist,ncol=2,layout_matrix=rbind(c(rep(1,40),2)))
dev.off()


###########################
#temperatur
###########################

ggplot(all,aes(date,temp,col=as.factor(tiefe)))+geom_point()

ggplot(alldist,aes(date,temp,col=as.factor(tiefe)))+geom_point()

##############################################################
#tiefenprofil plot
##############################################################

#Co2 mittelwerte der unterschiedlichen Tiefenstufen von dist und undist Probe
co2mean<-aggregate(all$CO2_raw,list(all$tiefe),function(x) mean(x,na.rm = T))
co2mean_dist<-aggregate(alldist$CO2_raw,list(alldist$tiefe),function(x) mean(x,na.rm = T))

#Co2 range der unterschiedlichen Tiefenstufen von dist und undist Probe
co2range<-aggregate(all$CO2_raw,list(all$tiefe),function(x) range(x,na.rm = T))
co2range_dist<-aggregate(alldist$CO2_raw,list(alldist$tiefe),function(x) range(x,na.rm = T))

#Die CO2 minima und die maxima verkehrt herum anneinander hängen um polygon zu plotten
range_undist<-data.frame(co2=c(co2range[2:5,2][,1],rev(co2range[2:5,2][,2])),tiefe=c(co2range[2:5,1],rev(co2range[2:5,1])))
range_dist<-data.frame(co2=c(co2range_dist[2:5,2][,1],rev(co2range_dist[2:5,2][,2])),tiefe=c(co2range_dist[2:5,1],rev(co2range_dist[2:5,1])))

#CO2 tiefenprofil plot
ggplot()+
  geom_polygon(data=range_undist,aes(co2,tiefe,fill="ungestört"),col=0,alpha=0.3,show.legend = F)+
  geom_polygon(data=range_dist,aes(co2,tiefe,fill="gestört"),col=0,alpha=0.3,show.legend = F)+
  geom_path(data=co2mean[2:5,],aes(x,Group.1,col="ungestört"))+
  geom_path(data=co2mean_dist[2:5,],aes(x,Group.1,col="gestört"))+
  labs(x=expression("CO"[2]*"  [ppm]"),y="Tiefe [cm]",col="")+
  theme_classic()+
  ggsave(paste0(plotpfad,"co2_tiefenprofil.pdf"),width=4.5,height = 3)

