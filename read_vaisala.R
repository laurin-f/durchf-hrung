
#Long: funktion um txt dateien im Format wie mttty sie schreibt einzulesen####
#offsets=c(-104.61813,122.11414,0,66.67662)
#speichert im long format
read_vaisala<-function(name=0,#falls angegeben wird die .txt datei einzeln eingelsen
                       pfad=path,#dateipfad der dateien
                       datum=format(Sys.time()-3600*24,"%d.%m"),#falls angegeben werden die Dateien tiefe1_datum bis tiefe4_datum eingelesen und anneinandergehängt. falls name und datum nicht angegeben sind wird das datum des vortags als default angegeben 
                       CO2_line=19,#falls name angegeben wurde kann hier die position der CO2-werte in der .txt datei angepasst werden
                       temp_line=43,#falls name angegeben wurde kann hier die position der temperaturwerte in der .txt datei angepasst werden
                       offsets=c(-230 ,-24,0,80),#die abweichungen der Messsonden können hier angegeben werden
                       Sonde,
                       aggregate=T,#wenn T werden die werte auf minuten aggregiert
                       order=1:4){#die reihenfolge der Sonden in den tiefen 1 bis 4 die Sondennummern lauten wie folgt 1=respi2 2=respi3 3=PC1 4=PC2
  
  #tiefenstufen der Sonden
  tiefenstufen<-c(-2,-6,-10,-14)
  #wenn kein name angegeben wird werden die dateien tiefe1-tiefe4_datum eingelesen und in eine Liste geschrieben
  if (!is.character(name)){
    
    #Falls die datei co2_datum.R besteht wird diese geladen
    if(file.exists(paste0(pfad,"co2_",datum,".R"))){
      load(paste0(pfad,"co2_",datum,".R"))
    
    #Ansonsten wird die Datei hier erstellt
    }else{
    #liste anlegen
    lines<-list(1,2,3,4)
    #package fur datumsformatierung
    library(lubridate)
    #date<-NULL
    #date<-parse_date_time(date)
    date<-vector("list",4)
    #leere Vektoren zum befüllen
    CO2<-NULL
    temp<-NULL
    tiefe<-NULL

    #schleife zum einlesen der dateien
    for (i in 1:4){
      print(paste("reading tiefe",i))
      #.txt datei wird in liste geschrieben
      lines[[i]]<-readLines(paste0(pfad,"tiefe",i,"_",datum,".txt"))
      #timestamp finden 
      timestamp<-lines[[i]][grep("Aug|Sep|Oct|Nov|Dec",lines[[i]])]
      #timestamp formatieren
      timestamp<-parse_date_time(timestamp,"ab!d!HMSY",locale = "English_United States.1252")
      #monat und tag aus dem timestamp extrahieren
      monthday<-(format(timestamp,"%m.%d"))
      
      #position der ersten Zeile nach dem Timestamp finden
      begin<-(grep("Aug|Sep|Oct|Nov|Dec",lines[[i]])+2)

      #falls kein Timestamp vorhanden ist am anfang beginnen
      if (length(begin)==0){begin<-1}
      #subset von der ersten zeile nach dem timestamp bis ende 
      sub<-lines[[i]][begin[1]:length(lines[[i]])]
      #nur zeilen mit l?nge der ersten zeile verwenden
      sub<-sub[nchar(sub)==nchar(sub[1])] 
      #Tag ist der Tag des ersten Timestamps
      day<-monthday[1]
      #wenn monthday länger als eins ist...
      if(length(monthday)>1){
        #leeren Vektor für Tag anlegen
        day<-NULL
        #schleife um dem datenpunkten den richtigen monthday zuzuweisen
        for (j in 1:(length(monthday)-1)){
        #anfangspunkt des j-ten monthday
        start<-which(sub==lines[[i]][begin[j]])-1
        #endpunkt des j-ten monthday
        stop<-which(sub==lines[[i]][begin[j+1]])-1
        #n-mal j-ten monthday an day-vektor anhängen 
        #wobei n der Abstand zwischen start und stop ist
        day<-c(day,rep(monthday[j],stop-start))}
        #das letzte element von monthday wird bis zum ende des Datensatzes angehängt
        day<-c(day,rep(tail(monthday,1),length(sub)-stop))
        }
        
      #Uhrzeit formatieren
      #dati<-parse_date_time(paste0(2018,day,substr(sub,1,8)),"YmdHMS",tz = "CEST")
      date[[i]]<-parse_date_time(paste0(2018,day,substr(sub,1,8)),"YmdHMS",tz = "CET")
      #datum der i-ten Tiefe wird an den datum-Vektor anghängt
      #date<-c(date,dati)
      
      #Vektor für die abweichung der Position der CO2 werte in den unterschiedlich Vaisala Outputs 
      cs<-c(0,0,7,0)[order]#reihenfolge kann mit order verändert werden
      #CO2 Werte der i-ten Tiefe wird an den CO2-Vektor anghängt
      CO2<-c(CO2,as.numeric(substr(sub,19+cs[i],25+cs[i])))
      
      #Vektor für die abweichung der Position der Temperatur werte
      ts<-c(29,31,43,29)[order]#reihenfolge kann mit order verändert werden
      #Temperatur-Werte der i-ten Tiefe wird an den temp-Vektor anghängt
      temp<-c(temp,as.numeric(substr(sub,ts[i],ts[i]+4)))
      
      #tiefenstufe i wird sooft an den tiefen-Vektor angehängt, dass die Länge mit den anderen Vektoren übereinstimmt
      tiefe<-c(tiefe,rep(tiefenstufen[i],length(sub)))
    }
    
    date<-do.call("c",date)
    
    #CO2 korrektur
    CO2_korr<-CO2
    for (i in 1:4){
      #offset i wird von CO2-werten von Tiefenstufe i abgezogen 
      CO2_korr[tiefe==tiefenstufen[i]]<-CO2[tiefe==tiefenstufen[i]]-offsets[i]
    }
    
    #Alle Vektoren zu einem Datensatz zusammenfügen
    out<-data.frame(date=date,CO2_raw=CO2,CO2=CO2_korr,temp=temp,tiefe=(tiefe))
    
    #Dopplungen der Datumsspalte finden
    dopplungen<-which(diff(out$date)==0)
    #falls Dopplungen vorhanden...
    if(length(dopplungen)!=0){
      #diese entfernen
      out<-out[-dopplungen,]}
    
    #falls aggregiert werden soll...
    if (aggregate==T){
      #vektor mit Minutenwerten erstellen
      min<-round_date(out$date,"min")
      #nach Minutenwerten und tiefe aggregieren
      outmin<-aggregate(out,list(min,out$tiefe),mean)
      #spalten die der aggregate-Befehl automatisch erstellt entfernen
      out<-outmin[,-(1:2)]
    }#ende aggregate
    
    #Speichern des Datensatzes als R-Objekt um beim erneuten laden Zeit zu sparen
    save(out,file=paste0(pfad,"co2_",datum,".R"))
    }#ende if schleife file.exists
    
    #output
    return(out)
    
  }else{#ende der if schleife für name = 0
    
    #einlesen der .txt Datei
    lines<-readLines(paste0(pfad,name,".txt"))
    
    #package für Datumsformatierung
    library(lubridate)
    
    #timestamp finden 
    timestamp<-lines[grep("Aug|Sep|Oct|Nov",lines)]
    #timestamp formatieren
    timestamp<-parse_date_time(timestamp,"ab!d!HMSY",locale = "English_United States.1252")
    
    #monat und tag aus dem timestamp extrahieren
    monthday<-(format(timestamp,"%m.%d"))
    
    #position der ersten Zeile nach dem Timestamp finden
    begin<-(grep("Aug|Sep|Oct|Nov",lines)+2)
    
    #falls kein Timestamp vorhanden ist am anfang beginnen
    if (length(begin)==0){begin<-1}
    #subset von der ersten zeile nach dem timestamp bis ende 
    sub<-lines[begin[1]:length(lines)]
    #nur zeilen mit l?nge der ersten zeile verwenden
    sub<-sub[nchar(sub)==nchar(sub[1])]  
    #Tag ist der Tag des ersten Timestamps
    day<-monthday[1]
    #wenn monthday länger als eins ist...
    if(length(monthday)>1){
      #leeren Vektor für Tag anlegen
      day<-NULL
      #schleife um dem datenpunkten den richtigen monthday zuzuweisen
      for (j in 1:(length(monthday)-1)){
        #anfangspunkt des j-ten monthday
        start<-which(sub==lines[begin[j]])-1
        #endpunkt des j-ten monthday
        stop<-which(sub==lines[begin[j+1]])-1
        #n-mal j-ten monthday an day-vektor anhängen 
        #wobei n der Abstand zwischen start und stop ist
        day<-c(day,rep(monthday[j],stop-start))}
      #das letzte element von monthday wird bis zum ende des Datensatzes angehängt
      day<-c(day,rep(tail(monthday,1),length(sub)-stop))
    }
    
    #Uhrzeit formatieren
    date<-parse_date_time(paste0(2018,day,substr(sub,1,8)),"YmdHMS",tz = "CET")
    
    #CO2-Werte extrahieren
    #über CO2_line wird die Position der CO2 Werte in der Datei bestimmt 
    CO2_raw<-as.numeric(substr(sub,CO2_line,CO2_line+6))
    #Offset der angegebenen Sonde wird von den CO2 Werten abgezogen
    CO2<-CO2_raw-offsets[Sonde]
    
    #Temperatur-Werte extrahieren
    #über temp_line wird die Position der Temperatur Werte in der Datei bestimmt 
    temp<-as.numeric(substr(sub,temp_line,temp_line+4))
    
    #Datensatz mit allen Werten
    out<-data.frame(date=date,CO2_raw=CO2_raw,CO2=CO2,temp=temp,tiefe=tiefenstufen[Sonde])
    
    #Wenn aggregiert werden soll...
    if (aggregate==T){
      #vektor mit Minutenwerten erstellen
      min<-round_date(out$date,"min")
      #nach Minutenwerten aggregieren
      outmin<-aggregate(out,list(min),mean)
      #spalten die der aggregate-Befehl automatisch erstellt entfernen
      out<-outmin[,-(1)]}
    #output
    return(out)}}#ende
