read_all<-function(datum,start,q=T,lf=T){
  source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/Versuchsdesign/read_vaisala.R")
  source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_teta.R")
  source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/waage.R")
  
  bfpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/feuchte/"
  lfpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/leitf/"
  co2pfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/"
  
  print("reading CO2 data")
  co2<-read_vaisala(pfad=co2pfad,datum=datum)
  #co2$date<-ymd_hm(format(co2$date,"%y%m%d%H%M"),tz="CET")
  print("reading theta data")
  bf<-read_teta(pfad = bfpfad,name=paste0("bf_",datum))
  merged<-merge(co2,bf,all=T)

  if(q==T){
    print("reading q data")
  q<-read_waage(datum,start)
  merged<-merge(merged,q,all=T)}
  if(lf==T){
    print("reading lf data")
  library(readxl)
  lf<-read_xlsx(paste0(lfpfad,datum,".xlsx"))[,4:5]
  colnames(lf)<-c("date","lf")
  merged<-merge(merged,lf,all=T)}
return(merged)
}
