
#############################################################
#Funktion zum einlesen der Bodenfeuchtedaten
#inklusive transformation von mV in Vol%
read_teta<-function(name=0,#dateiname ohne .dat endung
                    pfad=path,#dateipfad
                    #a0 & a1 = koeffizienten für die berechnung von theta aus der Spannung in V 
                    a0=1.3,
                    a1=7.8,
                    offset=c(0.03597042,0.03995589,0.04884653,0.01169276),#korrektur der Abweichung der Sonden gemäß teta_kalibrierung.R
                    long_format=T){#wenn TRUE dann wird die eingelesene Matrix ins long format gebracht
  #einlesen der csv datei
  data<-read.csv(paste0(pfad,name,".dat"),header = F)
  #umrechnen der gemessenen Spannung von mV in V
  tiefen<-data[,5:8]/1000
  
  #berechnung der Dielektrizitätskonstante für Sonden Modell ML1
  #Formel aus dem USER MANUAL von Delta-T Devices Ltd.
  eps<-1+6.19*tiefen-9.72*tiefen^2+24.35*tiefen^3-30.84*tiefen^4+14.73*tiefen^5
  #und für Sonden Modell ML2x
  eps[,4]<-1.07+6.4*tiefen[,4]-6.4*tiefen[,4]^2+4.7*tiefen[,4]^3
  #berechnung der Bodenfeuchte theta mit a0 und a1
  thetas<-(eps-a0)/a1
  
  #korrektur der Abweichung der Sonden untereinander
  korr<-t(t(thetas)+offset)
  
  #Werte zu einem Datensatz zusammenfügen
  theta<-cbind(thetas,korr)
  #Spaltennamen definieren
  colnames(theta)<-c(paste0("tiefe",1:4,"raw"),paste0("tiefe",1:4))
  
  #package für Datumsformatierung
  library(lubridate)
  #Package für rollaplly
  library(zoo)
  
  #da in der Zeitspalte stunden unter 10 keine 0 vor der Ziffer haben (910 statt 09:10) 
  #wird die Anzeil der nötigen Nullen um das Datum einlesen zu können hier bestimmt
  #falls die zeitspalte aus 4 ziffern besteht wird keine 0 angehängt falls sie aus 3 besteht eine etc.
  #collapse="" bewirkt das die Nullen ohne Trennung zu einem string verschmelzen 
  zeros<-rollapply((4-nchar(data[,4])),1,function(x) paste0(rep(0,x),collapse=""))
  
  #Formatierung des Datums
  theta$date<-parse_date_time(paste0(data[,2],data[,3],zeros,data[,4]),"YjH!M",tz="CET")

  #Wenn Lang Format dann
  if(long_format==T){
    #datum 4-mal wiederholt aneinandergehängt
    date<-rep(theta$date,4)
    
    #Bodenfeuchte-Werte als Vektor
    bf<-as.numeric(as.matrix(theta[,5:8]))
    
    #Bodenfeuchte-Werte unkorrigiert als Vektor
    bf_raw<-as.numeric(as.matrix(theta[,1:4]))
    
    #vektor der Tiefenstufen jeweils mit der länge der Datumspalte
    tiefenstufe<-c(-2,-6,-10,-14)
    tiefe<-rep(tiefenstufe,each=length(theta$date))
    
    #Datensatz mit allen Werten
    theta<-data.frame(date=date,theta=bf,theta_raw=bf_raw,tiefe=tiefe)
    
    #Ausreißer rausschmeißen
    theta<-theta[theta$theta<2,]
    
    #NAs der tiefe rausschmeißen
    theta<-theta[!is.na(theta$tiefe),]
    #Dopplungen der Datumsspalte finden
    dopplungen<-which(diff(theta$date)==0)
    #Falls vorhanden rausschmeißen
    if(length(dopplungen)!=0){
    theta<-theta[-(dopplungen+1),]}
  }#ende Longformat
  
  #output
  return(theta)
}#ende