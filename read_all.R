########################################################################
#Funktion um alle dateien einzulesen und in einem Datensatz zusammenzuführen

read_all<-function(datum,#datum des Versuchs
                   start,##zeitpunkt an dem Kamera eingeschaltet wurde
                   qs=T,#sind abflusswerte nicht verfügbar qs=F einstellen 
                   lfs=T){#sind Leitfähigkeitswerte nicht verfügbar lfs=F einstellen
  #auführen der R-skripte für die benötigten Funktionen
  source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/Versuchsdesign/read_vaisala.R")
  source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_teta.R")
  source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/waage.R")
  
  #definieren der dateipfade
  bfpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/feuchte/"
  lfpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/leitf/"
  co2pfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/"
  
  print("reading CO2 data")
  #einlesen der co2 daten
  co2<-read_vaisala(pfad=co2pfad,datum=datum)
  #runden der Datumsspalte auf Minutenwerte
  co2$date<-round_date(co2$date,"minute")
  
  print("reading theta data")
  #einlesen der Bodenfeuchte daten
  bf<-read_teta(pfad = bfpfad,name=paste0("bf_",datum))
  #zusammenführen der datensätze
  merged<-merge(co2,bf,all=T)

  if(qs==T){
    print("reading q data")
  #einlesen der abflussdaten
  q<-read_waage(datum,start)
  #runden der Datumsspalte auf Minutenwerte
  q$date<-round_date(q$date,unit = "min")
  #erstellen einer durchgeheden Zeitsequenz mit Minutenwerten 
  qmin<-data.frame(date=seq(min(q$date),max(q$date),60))
  #zusammenführen der Abflusswerte und der Minutensequenz
  q<-merge(qmin,q,all.x=T)
  #anfangs und endwerte des Abflusses auf null setzen
  q$q[c(1,nrow(q))]<-0
  #interpolation der Fehlwerte um minütliche Abflusswerte zu erhalten
  q$q<-na.approx(q$q)
  #abfluss wurde nur von tiefe -14 bestimmt
  q$tiefe<--17
  #zusammenführen der datensätze
  merged<-merge(merged,q,all=T)
  }
  
  if(lfs==T){
    print("reading lf data")
  #package um .xlsx Format einzulesen
  library(readxl)
  #einlesen der Leitfähigkeitsdaten
  lf<-read_xlsx(paste0(lfpfad,datum,".xlsx"))[,4:5]
  #Spaltennamen ändern
  colnames(lf)<-c("date","lf")
  #datumsspalte formatieren
  lf$date<-ymd_hms(lf$date,tz="CET")
  #minutenwerte aus datumsspalte extrahieren
  min<-round_date(lf$date,"min")
  #aggregieren auf minutenwerte
  lfmin<-aggregate(lf,list(min),mean)
  lf<-lfmin[,-(1)]
  #runden der Datumsspalte auf Minutenwerte
  lf$date<-round_date(lf$date,unit = "min")
  #Leitfähigkeit wurde nur von tiefe -14 bestimmt
  lf$tiefe<--17
  #zusammenführen der datensätze
  merged<-merge(merged,lf,all=T)
  }
  
  #Wenn abfluss und Leitfähigkeit vorhanden sind dann
  if(lfs==T & qs==T){
    #laden der am script ca.R berechneten Regression zwischen LF und Ca
    capath<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/ca/"
    load(file=paste0(capath,"cafm.R"))
    #berechnung der Ca2+ Konzentration mit dem Modell
    merged$ca_conc<-predict(cafm,data.frame(lf=merged$lf))
    #es gibt keine Konzentrationen unter Null
    merged$ca_conc[merged$ca_conc<0]<-0
    #berechnung der Menge an Calcium in mg die pro Zeitschritt transportiert wird 
    merged$ca_mg<-merged$ca_conc*merged$q/1000#mg/l*ml/min<-mg
  }
return(merged)
}#ende Funktion
