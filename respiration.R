#######################################
#respiration undisturbed soil

#nötiges skript ausführen
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/Versuchsdesign/read_vaisala.R")
#pfad definieren
co2pfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/"

#daten einlesen
resp<-read_vaisala(name="respi_undist",pfad=co2pfad,Sonde=1,aggregate = T)
#letzter wert passt nicht
resp<-resp[-nrow(resp),]
#übersicht verschaffen
plot(resp$date,resp$CO2_raw)

#startpunkte der respirationsmessungen sind die Punkte an denen eine negative steigung in eine positive umschlägt
starts<-which(diff(resp$CO2_raw[1:(nrow(resp)-1)])<(-2)&diff(resp$CO2_raw[2:nrow(resp)])>0)+1

#subset der startpunkte
sub<-resp[starts,]

#test ob die richtigen werte ausgewählt wurden
points(sub$date,sub$CO2_raw,col=2)

#liste für einzelne resp-messungen anlegen
startsub<-vector("list",length(starts))
#subset der einzelnen respirationmessungen mit schleife in liste
for (i in 1:length(starts)){
  #vom i-ten startpunkt bis 4-minuten danach auswählen
  sub<-resp[starts[i]:(starts[i]+4),]
  #t_min als minuten nach start berechnen
  sub$t_min<-as.numeric(difftime(sub$date,resp$date[starts[i]],units = "min"))
  #messunge in liste schreiben
  startsub[[i]]<-sub
}

#listenelemente aneinander hängen
sub<-do.call("rbind",startsub)
#überblick verschaffen
plot(sub$t_min,sub$CO2_raw)
#regression  von co2 über t_min
respfm<-glm(CO2_raw~t_min,data=sub)
#slope als geradensteigung der regression
slope<-respfm$coefficients[2]
#regression plotten
abline(respfm)

###########################################
#Größen für die Umrechnung der Einheit von ppm/min in cm/min
r<-7.5#cm Radius
A<-pi*r^2#cm2 area


#volumen der Sonde grob abgeschätzt
volprobe<-4^3#cm3
#kammervolumen sind 1.803 l abzüglich des Sondenvolumens
Vol<-1.803*1000-volprobe#l *1000 = cm3 

#Einheit umrechnen in cm/min
resp_undist<-slope*Vol/A/10^6#ppm/min *cm3 /cm2 *10^6 = cm/min

#Wert anschauen
resp_undist

#######################################
#respiration disturbed soil

#daten einlesen
resp<-read_vaisala(name="respi_dist",pfad=co2pfad,Sonde=1,aggregate = T)
#die letzten 20 werte passen nicht
resp<-resp[-((nrow(resp)-20):nrow(resp)),]
#überblick verschaffen
plot(resp$date,resp$CO2_raw)

#startpunkte auswählen
starts<-which(diff(resp$CO2_raw[1:(nrow(resp)-1)])<(-3)&diff(resp$CO2_raw[2:nrow(resp)])>0)+1
#subset der startpunkte
sub<-resp[starts,]
#schauen ob richtige punkte ausgewählt wurden
points(sub$date,sub$CO2_raw,col=2)

#liste für subsets anlegen
startsub<-vector("list",length(starts))
#schleife um liste zu füllen 
for (i in 1:length(starts)){
  #subset vom i-ten start bis 4 min danach
  sub<-resp[starts[i]:(starts[i]+4),]
  #t_min als minuten nach start
  sub$t_min<-as.numeric(difftime(sub$date,resp$date[starts[i]],units = "min"))
  #subset in liste schreiben
  startsub[[i]]<-sub
}

#listenelemente aneinander hängen
sub<-do.call("rbind",startsub)
#überblick verschaffen
plot(sub$t_min,sub$CO2_raw)
#regression erstellen
respfm<-glm(CO2_raw~t_min,data=sub)
#geradensteigung auswählen
slope<-respfm$coefficients[2]
#steigung anschauen
abline(respfm)

############################
#umrechung der Einheit von ppm/min in cm/min

#grundfläche des zylinders 
r<-7.5#cm Radius
A<-pi*r^2#cm2 area

#sondenvolumen abschätzen
volprobe<-4^3
#kammervolumen bestimmen
Vol<-1.803*1000-volprobe

#umrechunung der Einheit
resp_dist<-slope*Vol/A/10^6#ppm/min/cm2


#######################
resp_undist
resp_dist
