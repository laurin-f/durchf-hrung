#########################################################
#Zeitumstellung

co2pfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/"

source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_teta.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_all.R")
#CO2_zeitumst<-read_vaisala(datum = "26.10",pfad=co2pfad,aggregate = F)
# CO2_zeitumst$datechr<-format(CO2_zeitumst$date,usetz = T)
# t1<-which(CO2_zeitumst$datechr=="2018-10-28 02:00:00 CET"&CO2_zeitumst$tiefe==-2)
# CO2_zeitumst$date[t1[1]:(t1[2]-1)]<-CO2_zeitumst$date[t1[1]:(t1[2]-1)]-60*60
# t2<-which(CO2_zeitumst$datechr=="2018-10-28 02:00:00 CET"&CO2_zeitumst$tiefe==-14)
# CO2_zeitumst$date[t2[1]:(t2[2]-1)]<-CO2_zeitumst$date[t2[1]:(t2[2]-1)]-60*60
# t3<-which(CO2_zeitumst$datechr=="2018-10-28 02:01:00 CET"&CO2_zeitumst$tiefe==-10)
# CO2_zeitumst$date[t3[1]:(t3[2]-2)]<-CO2_zeitumst$date[t3[1]:(t3[2]-2)]-60*60
# CO2_zeitumst<-CO2_zeitumst[-6]
# #vektor mit Minutenwerten erstellen
# min<-round_date(CO2_zeitumst$date,"min")
# minchr<-format(min,usetz = T)
# CO2_zeitumstmin<-aggregate(CO2_zeitumst,list(minchr,CO2_zeitumst$tiefe),mean)
# out<-CO2_zeitumstmin[,-(1:2)]
# save(out,file = paste0(co2pfad,"co2_26.10.R"))

bfpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/feuchte/"
bf26<-read_teta(pfad = bfpfad,name=paste0("bf_","26.10"))
datchr<-format(bf26$date,usetz = T)
datchr<-gsub("CET","CEST",datchr)
bf26$date<-with_tz(ymd_hms(datchr,tz="UTC")-2*60*60)

okt26<-read_all(datum="26.10",start = "10:03")
okt26<-okt26[,-(6:7)]
okt26$datechr<-format(okt26$date,usetz = T)
bf26$datechr<-format(bf26$date,usetz = T)
okt26<-merge(okt26,bf26,by=c("datechr","date","tiefe"),all=T)

okt26<-okt26[,c(1:5,16:17,6:15)+1]

tiefe17<-subset(okt26,tiefe==-17)
zeitumst<-format(tiefe17$date,usetz = T)
zwei_winterzeit<-min(grep("2018-10-28 02:00:00 CET",zeitumst))
zwei_sommerzeit<-min(grep("2018-10-28 02:00:00 CEST",zeitumst))

ende_sommerzeit<-length(tiefe17$lf)-(zwei_winterzeit-zwei_sommerzeit)
tiefe17[zwei_sommerzeit:ende_sommerzeit,]<-tiefe17[zwei_winterzeit:length(tiefe17$lf),]


okt26$lf[okt26$tiefe==-17]<-tiefe17$lf
okt26<-okt26[1:max(which(!is.na(okt26$theta))),]
save(okt26,file="C:/Users/ThinkPad/Documents/Masterarbeit/daten/okt26.R")
