read_waage<-function(datum,start,pfad=NULL,mins=60){
library(exiftoolr)
library(lubridate)
waagepfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/waage/"
if(is.null(pfad)){
  pfad<-waagepfad
}
gewicht<-as.numeric(t(read.csv(paste0(waagepfad,"durchfluss_",datum,".csv"))))
file<-list.files(paste0(pfad,datum),pattern = ".JPG")
if(length(gewicht)!=length(file)){ 
  print("length of .jpg files and rows in .csv not the same")
}else{
  print("start reading exifs")
exifs<-exif_read(paste0(pfad,datum,"/",file))
print("finished reading exifs")
date<-ymd_hms(exifs$CreateDate,tz="CET")
startdate<-parse_date_time(paste(2018,datum,start),"ydmHM",tz="CET")
timediff<-startdate-min(date)
date<-date+timediff

flasche<-min(gewicht)
wasser<-gewicht-flasche
wasser<-as.numeric(filter(wasser,rep(1/3,5)))
q<-c(0,ifelse(diff(wasser)>0,diff(wasser)/15*mins,0))
return(data.frame(date=date,q=q))}
}
#start="09:21"
#diff(c(NA,1,1:5,7))
#id<-1:length(gewicht)
#plot(wasser)
#fm<-glm(wasser~log(id))
#preds<-predict(fm)
#lines(id,preds)
