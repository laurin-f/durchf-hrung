event<-function(datum,start,stop,wassermenge){
  library(lubridate)
  event<-data.frame(start=parse_date_time(paste0(2018,datum,start),"ydmHM",tz="CET"),stop=parse_date_time(paste0(2018,datum,stop),"ydmHM",tz="CET"))
  event$rain_ml_min<-wassermenge/(as.numeric(event$stop-event$start)*60)#ml/min
  r<-7.5/100#m Radius
  A<-pi*r^2#m2 area
  event$rain_mm_h<-event$rain_ml_min/1000*60/A
  return(event)
}