read_waage<-function(datum,start,pfad=NULL,mins=1,mov_avg=5){
library(exiftoolr)
library(lubridate)
waagepfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/waage/"
if(is.null(pfad)){
  pfad<-waagepfad
}
dat<-read.csv(paste0(waagepfad,"durchfluss_",datum,".csv"),sep=";")
gewicht<-dat$gewicht

if(file.exists(paste0(pfad,"exifs_",datum,".txt"))){
  date<-ymd_hms(read.csv(paste0(pfad,"exifs_",datum,".txt"),stringsAsFactors = F)[,1],tz="CET")
}else{
file<-list.files(paste0(pfad,datum),pattern = ".JPG")
if(length(gewicht)!=length(file)){ 
  print("length of .jpg files and rows in .csv not the same")
}else{
  print("start reading exifs")
exifs<-exif_read(paste0(pfad,datum,"/",file))
print("finished reading exifs")
date<-ymd_hms(exifs$CreateDate,tz="CET")
write.csv(as.data.frame(date),paste0(pfad,"exifs_",datum,".txt"),row.names = F)
}
}

startdate<-parse_date_time(paste(2018,datum,start),"ydmHM",tz="CET")
timediff<-startdate-min(date)
date<-date+timediff


flasche<-min(gewicht)
wasser<-gewicht-flasche
wasser<-as.numeric(filter(wasser,rep(1/mov_avg,mov_avg)))
q<-c(0,ifelse(diff(wasser)>0,diff(wasser)/as.numeric(diff(date))*mins,0))
return(data.frame(id=dat$id,date=date,q=q,wasser=wasser))}
