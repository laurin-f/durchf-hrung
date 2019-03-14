#################################
#Hier wird überprüft ob in den Abflussdaten Fehler aufgrund eines Drifts in der Waage sind.
#dazu werden die mit der Wildtierkamera bestimmten Wassergehalte der Sammelgefäße mit den
#nach dem Versuch ohne Deckel bestimmten Wassergehalten verglichen

#Daten laden
load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")

#Packages laden
library(readxl)
library(stringr)
#pfad definieren
waagepath<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/waage/"

#xlsx tabelle einlesen in der die ohne Deckel bestimmten Wassermengen 
#der Sammelgefäßen stehen 
saugkerzen<-read_xlsx(paste0(waagepath,"wassermenge_saugkerzen.xlsx"))
#subset der untersten Tiefe für alle Datenpunkte bei denen datum == Versuch ist
#bei manchen Versuchen wurden die Saugkerzen zweimal geleert, 
#beim 2. mal wurde aber kein abfluss mehr gemessen und datum ist ungleich Versuch
sub<-subset(saugkerzen,tiefe==5&datum==Versuch)
#datum zu character im format Monat.Tag umwandeln
sub$datum<-format(sub$datum,"%m.%d")

#Im Datensatz all_list ist jedes Listenelement ein Versuch
#für jeden Versuch wird der Maximale Wassergehalt 
#der mit der Wildtierkamera gemessen wurde bestimmt
wasser_transf<-do.call("c",lapply(all_list,function(x) max(x$wasser,na.rm=T)))
#als Datum jedes Versuch wird der Tag des Versuchsbeginns extrahiert 
#also das minimale Datum im format Monat.Tag
date_transf<-do.call("c",lapply(all_list,function(x) format(min(x$date,na.rm=T),"%m.%d")))

#Datensatz der Wassergehalte der Wildtierkamera 
#mit dem zur Kontrolle ohne Deckel bestimmten zusammenfügen
transf<-data.frame(wasser_transf,datum=date_transf)
merged<-merge(sub,transf)

#ploten
plot(merged$`wasser [ml]`,merged$wasser_transf)
abline(1,1)
#ganz gute übereinstimmung
