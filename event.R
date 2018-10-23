###############################################
#Funktion um die Events zu speichern

event<-function(pfad="C:/Users/ThinkPad/Documents/Masterarbeit/daten/events/"){
  #packege für datumsformatierung
  library(lubridate)
  #package um .xlsx dateien  einzulesen
  library(readxl)
  #einlesen der eventstabelle
  event<-read_xlsx(paste0(pfad,"events.xlsx"))
  #ändern der Timezone, da default = UTC
  event$start<-ymd_hms(event$start,tz="CET")
  event$stop<-ymd_hms(event$stop,tz="CET")
  #berechnen der minütlichen Wassermenge
  event$rain_ml_min<-event$wassermenge/(as.numeric(event$stop-event$start)*60)#ml/min
  r<-7.5/100#m Radius
  A<-pi*r^2#m2 area
  #berechnung der intensität in mm/h
  event$rain_mm_h<-event$rain_ml_min/1000*60/A#mm/h
  return(event)
}