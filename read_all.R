read_all<-function(datum,start,qs=T,lfs=T){
  source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/Versuchsdesign/read_vaisala.R")
  source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_teta.R")
  source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/waage.R")
  
  bfpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/feuchte/"
  lfpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/leitf/"
  co2pfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/"
  print("reading CO2 data")
  co2<-read_vaisala(pfad=co2pfad,datum=datum)
  co2$date<-round_date(co2$date,"minute")
  
  print("reading theta data")
  bf<-read_teta(pfad = bfpfad,name=paste0("bf_",datum))
  merged<-merge(co2,bf,all=T)

  if(qs==T){
    print("reading q data")
  q<-read_waage(datum,start)
  q$date<-round_date(q$date,unit = "min")
  qmin<-data.frame(date=seq(min(q$date),max(q$date),60))
  q<-merge(qmin,q,all.x=T)
  q$q[c(1,nrow(q))]<-0
  q$q<-na.approx(q$q)
  q$tiefe<--14
  merged<-merge(merged,q,all=T)
  
  
  }
  if(lfs==T){
    print("reading lf data")
  library(readxl)
  lf<-read_xlsx(paste0(lfpfad,datum,".xlsx"))[,4:5]
  colnames(lf)<-c("date","lf")
  lf$date<-ymd_hms(lf$date,tz="CET")
  min<-format(lf$date,"%Y%m%d%H%M")
  lfmin<-aggregate(lf,list(min),mean)
  lf<-lfmin[,-(1)]
  lf$date<-round_date(lf$date,unit = "min")
  lf$tiefe<--14
  
  merged<-merge(merged,lf,all=T)
  }
  if(lfs==T & qs==T){
    capath<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/ca/"
    load(file=paste0(capath,"cafm.R"))
    merged$ca_conc<-predict(cafm,data.frame(lf=merged$lf))
    merged$ca_conc[merged$ca_conc<0]<-0
    merged$ca_mg<-merged$ca_conc*merged$q/1000#mg/l*ml/min<-mg
  }
return(merged)
}
