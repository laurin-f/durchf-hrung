source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/Versuchsdesign/read_vaisala.R")

bfpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/feuchte/"
co2pfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/"

co2<-read_vaisala(pfad=co2pfad,datum="10.10")
bf<-read_teta("bf_10",bfpfad)

length(out$date)/60
bf<-bf[!is.na(bf$tiefe),]
co2<-co2[!is.na(co2$tiefe),]

event<-data.frame(start=as.POSIXct("2018-10-10 11:15:00",tz="CET"),stop=as.POSIXct("2018-10-10 13:10:00",tz="CET"))

library(ggplot2)
library(gridExtra)

co2_plot<-ggplot()+
  geom_rect(data=event,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.3)+
  geom_line(data=co2,aes(x=date,y=CO2_raw,col=as.factor(tiefe)))+
  labs(y=expression("CO"[2]*"  [ppm]"),x="",col="tiefe")+
  theme_classic()

bf_plot<-ggplot()+geom_line(data=bf,aes(date,theta,col=as.factor(tiefe)))+geom_rect(data=event,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.3)+
  labs(y=expression("CO"[2]*"  [ppm]"),x="",col="tiefe")+
  theme_classic()

grid.arrange(co2_plot,bf_plot)

