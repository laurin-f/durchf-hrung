##############################################################
#Funktion um Durchflussdaten der untersten Saugkerze einzulesen
#und mit dem Datum des dazugehörigen Fotos zu verknüpfen
read_waage<-function(datum,#datum der Messung
                     start,#zeitpunkt an dem Kamera eingeschaltet wurde
                     q_filter=3,##zellenweite des gleitenden mittels für q, wenn na_interpolation TRUE dann entspricht 3 ein Zeitfenster von 30 Minuten nach vorne und hinten
                     na_interpolation=T,#sollen zwischen die 30 minütigen Werte minütliche Werte interpoliert werden dann TRUE
                     pfad=NULL){#dateipfad wenn NULL wird defaultpfad benutzt

library(exiftoolr)#package um exif metadaten der Fotos einzulesen
library(lubridate)#package um das datum zu formatieren
  
  #defaultpfad
waagepfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/waage/"
if(is.null(pfad)){#dateipfad wenn NULL wird defaultpfad benutzt
  pfad<-waagepfad
}
#einlesen der aus den fotos manuell abgelesenen Gewicht-werte
dat<-read.csv(paste0(waagepfad,"durchfluss_",datum,".csv"),sep=";")
gewicht<-dat$gewicht

#einlesen der namen aller .JPG dateien des Versuchs
file<-list.files(paste0(pfad,datum),pattern = ".JPG")
id<-as.numeric(substr(file,5,8))#id aus dateiname extrahieren

#wenn exifs schon ausgelesen wurden wird hier auf die gespeicherte Datei zugegriffen
if(file.exists(paste0(pfad,"exifs_",datum,".txt"))){
  date<-ymd_hms(read.csv(paste0(pfad,"exifs_",datum,".txt"),stringsAsFactors = F)[,1],tz="CET")
}else{
  
#falls in der .csv tabelle mit den Gewicht-werten nicht alle Fotos enthalten sind wird hier abgebrochen
if(length(gewicht)!=length(file)){ 
  print("length of .jpg files and rows in .csv not the same")
}else{
#ansonsten werden nun die Exif Metadaten der .JPGs eingelesen
print("start reading exifs")
exifs<-exif_read(paste0(pfad,datum,"/",file))
print("finished reading exifs")
#das Erstellungsdatum wird aus den Exifs ausgelesen
date<-ymd_hms(exifs$CreateDate,tz="CET")
#um Zeit zu sparen wird das Datum in einer .csv gespeichert und beim nächsten mal direkt auf diese zugegriffen
write.csv(as.data.frame(date),paste0(pfad,"exifs_",datum,".txt"),row.names = F)
}
}

#da das Datum in der kamera ohne strom zurückgesetzt wird, wird die in den Metadaten gespeicherte Zeit hier in die tatsächliche Zeit umgerechnet 
startdate<-parse_date_time(paste(2018,datum,start),"ydmHM",tz="CET")
#differenz zwischen kamerazeit und tatsächlicher zeit 
timediff<-startdate-min(date)
#angleichen der Zeit an die tatsächliche Zeit
date<-date+timediff

#gewicht der probenflasche entspricht dem gewicht zu anfang des Versuchs
flasche<-min(gewicht)
#die Menge des Wassers ist das gesamte Gewicht abzuglich des Gewichts der Flasche
wasser<-gewicht-flasche
#berechnung des Abflusses als änderungsrate des gewichts pro zeitschritt
#Einheit = ml/min
dt<-as.numeric(difftime(date[2:length(date)],date[1:(length(date)-1)],units="min"))
q<-c(0,ifelse(diff(wasser)>0,diff(wasser)/dt,0))
#der erste q-Wert nach Zeitlücken über 2h wird entfernt 
q<-ifelse(c(0,as.numeric(diff(date)))>3*60,NA,q)

#Datensatz mit allen Werten
q<-data.frame(id=id,date=date,q=q,wasser=wasser)

#runden der Datumsspalte auf Minutenwerte
q$date<-round_date(q$date,unit = "min")

if(na_interpolation==T){
  q_filter<-(q_filter-1)*30+1
#erstellen einer durchgeheden Zeitsequenz mit Minutenwerten 
qmin<-data.frame(date=seq(min(q$date),max(q$date),60))
#zusammenführen der Abflusswerte und der Minutensequenz
q<-merge(qmin,q,all.x=T)

#interpolation der Fehlwerte um minütliche Abflusswerte zu erhalten
q$q<-na.approx(q$q)
}
#gleitendes Mittel des Abflusses um die Kurve zu glätten
q$q<-zoo::rollapply(q$q, q_filter, mean, na.rm = T, fill=NA)
#ausgeben des Datensatzes
return(q)}
