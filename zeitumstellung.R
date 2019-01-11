#########################################################
#Zeitumstellungs formatierungsfehler beheben

#pfade
co2pfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/"
bfpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/feuchte/"

#skripte
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_teta.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_all.R")

#bodenfeuchte daten einlesen
bf26<-read_teta(pfad = bfpfad,name=paste0("bf_","26.10"))
#datumsspalte in character mit Timezonekürzel im string
datchr<-format(bf26$date,usetz = T)
#um 2:25 wurde von Central European Summer Time (CEST) in Central European Time (CET) 
#umgestellt anstatt um 2:59 wieder auf zwei uhr zurückzustellen
datchr[(grep("CET",datchr)[1]-1):grep("CET",datchr)[1]]
#also müssen alle werte wie sommerzeit gezählt werden
#CET durch CEST ersetzen
datchr<-gsub("CET","CEST",datchr)
#datum wieder vom character in Datum formatieren
#dabei mit umweg über UTC weil dort keine Zeitumstellung ist
bf26$date<-with_tz(ymd_hms(datchr,tz="UTC")-2*60*60)

#andere daten einlesen
okt26<-read_all(datum="26.10",start = "10:03")

#subset der tiefe -17

tiefe17<-subset(okt26,tiefe==-17)
#da bei der Leitfähigkeit alle Werte zwischen 2:00 und 3:00 doppelt eingelesen wurden,
#sowohl als "CET" als auch als "CEST"
plot(tiefe17$date[2400:2550],tiefe17$lf[2400:2550])

#datum als character
zeitumst<-format(tiefe17$date,usetz = T)
#2 uhr CET wählen
zwei_winterzeit<-grep("2018-10-28 02:00:00 CET",zeitumst)
#den CEST um 2:00 uhr wählen
zwei_sommerzeit<-grep("2018-10-28 02:00:00 CEST",zeitumst)

#durch die dopplung zwischen 2 und 3 uhr ist der datensatz eine stunde länger
ende_sommerzeit<-length(tiefe17$lf)-(zwei_winterzeit-zwei_sommerzeit)
#die werte die ab 2:00 uhr winterzeit bis zum ende des datensatzes stehen werden auf den zeitraum ab 2:00 sommerzeit bis zum eigentlichen ende der messunge geschrieben
tiefe17[zwei_sommerzeit:ende_sommerzeit,]<-tiefe17[zwei_winterzeit:length(tiefe17$lf),]

#lf werte im Datensatz entfernen 
okt26$lf<-NA
#die korrigierten lf werte reinschreiben
okt26$lf[okt26$tiefe==-17]<-tiefe17$lf

#schauen obs geklappt hat
points(okt26$date[okt26$date>"2018-10-28 02:00:00 CEST"&okt26$date<"2018-10-28 03:30:00 CET"],okt26$lf[okt26$date>"2018-10-28 02:00:00 CEST"&okt26$date<"2018-10-28 03:30:00 CET"],col=2)

#der fehler ist auch im Calcium da es über lf berechnet wurde
plot(okt26$date[okt26$date>"2018-10-28 02:00:00 CEST"&okt26$date<"2018-10-28 03:30:00 CET"],okt26$ca_conc[okt26$date>"2018-10-28 02:00:00 CEST"&okt26$date<"2018-10-28 03:30:00 CET"])

#also mit neuem lf calcium berechnen
#laden der am script ca.R berechneten Regression zwischen LF und Ca
capath<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/ca/"
load(file=paste0(capath,"cafm.R"))
#berechnung der Ca2+ Konzentration mit dem Modell
okt26$ca_conc<-predict(cafm,data.frame(lf=okt26$lf))
#es gibt keine Konzentrationen unter Null
okt26$ca_conc[okt26$ca_conc<0]<-0
#berechnung der Menge an Calcium in mg die pro Zeitschritt transportiert wird 
okt26$ca_mg<-okt26$ca_conc*okt26$q_interpol/1000#mg/l*ml/min<-mg

#überprüfen
points(okt26$date[okt26$date>"2018-10-28 02:00:00 CEST"&okt26$date<"2018-10-28 03:30:00 CET"],okt26$ca_conc[okt26$date>"2018-10-28 02:00:00 CEST"&okt26$date<"2018-10-28 03:30:00 CET"],col=2)

#bodenfeuchte spalten entfernen
okt26<-okt26[,-(6:7)]

#spalte mit datum als character einfügen weil hier eindeutig gemerged wird
#da R schwierigkeit mit CET und CEST im date format hat
okt26$datechr<-format(okt26$date,usetz = T)
bf26$datechr<-format(bf26$date,usetz = T)
#bf werte an datensatz anfügen
okt26<-merge(okt26,bf26,by=c("datechr","date","tiefe"),all=T)

#spalten reihenfolge umsortieren damit alles im selben format ist wie bei den anderen events
okt26<-okt26[,c(1:5,16:17,6:15)+1]

#letzte zeilen löschen
okt26<-okt26[1:max(which(!is.na(okt26$theta))),]
#datensatz speichern
save(okt26,file="C:/Users/ThinkPad/Documents/Masterarbeit/daten/okt26.R")
