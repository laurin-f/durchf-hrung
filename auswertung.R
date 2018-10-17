
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/Versuchsdesign/read_vaisala.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_teta.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/merge_data.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/waage.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_all.R")

library(readxl)
bfpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/feuchte/"
lfpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/leitf/"
co2pfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/"
plotpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/abbildungen/plots/"

co2_10.10<-read_vaisala(pfad=co2pfad,datum="10.10")
co2_15.10<-read_vaisala(pfad=co2pfad,datum="15.10")
tiefe2_15.10<-read_vaisala("tiefe2_15.10.2",co2pfad,aggregate = T,temp_line = 31,Sonde=2)
co2_15.10<-rbind(co2_15.10,tiefe2_15.10)

bf_10.10<-read_teta("bf_10.10",bfpfad,offset=c(0.03597042,0.03995589,0.04884653,0.01169276))
bf_15.10<-read_teta("bf_15.10",bfpfad,offset=c(0.03597042,0.03995589,0.04884653,0.01169276))
bf_15.10<-subset(bf_15.10,bf_15.10$date>=min(co2_15.10$date))

lf_15.10<-read_xlsx(paste0(lfpfad,"15.10.xlsx"))[,4:5]
colnames(lf_15.10)<-c("date","lf")
lf_10.10<-read_xlsx(paste0(lfpfad,"10.10.xlsx"))[,4:5]
lf_10
plot(lf_15.10$`Date/Time`,lf_15.10$Value)
plot(lf_10.10$`Date/Time`, lf_10.10$Value)

q_15.10<-read_waage("15.10",start = "09:21")

okt10<-merge_data(co2_10.10,bf_10.10)
okt15<-merge_data(co2_15.10,bf_15.10,q=q_15.10,lf=lf_15.10)

okt10<-read_all("10.10",q=F)
okt15<-read_all("15.10","09:21")



event_10.10<-data.frame(start=as.POSIXct("2018-10-10 11:15:00",tz="CET"),stop=as.POSIXct("2018-10-10 13:10:00",tz="CET"))
event_15.10<-data.frame(start=as.POSIXct("2018-10-15 09:31:00",tz="CET"),stop=as.POSIXct("2018-10-15 17:23:00",tz="CET"))

library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)


co2_plot_10.10<-ggplot()+
  geom_rect(data=event_10.10,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.3)+
  geom_line(data=co2_10.10,aes(x=date,y=CO2_raw,col=as.factor(tiefe)))+
  labs(y=expression("CO"[2]*"  [ppm]"),x="",col="tiefe")+
  theme_classic()+scale_x_datetime(limits = range(okt10$date))
leg<-get_legend(co2_plot_10.10)
co2_plot_10.10<-co2_plot_10.10 +theme(legend.position = "none")



bf_plot_10.10<-ggplot()+geom_line(data=bf_10.10,aes(date,theta,col=as.factor(tiefe)),show.legend = F)+geom_rect(data=event_10.10,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.3)+
  labs(y=expression(theta*"  [VOl %]"),x="",col="tiefe")+  theme_classic()+scale_x_datetime(limits = range(okt10$date))

co2_plot_15.10<-ggplot()+
  geom_rect(data=event_15.10,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.3)+
  geom_line(data=co2_15.10,aes(x=date,y=CO2_raw,col=as.factor(tiefe)))+
  labs(y=expression("CO"[2]*"  [ppm]"),x="",col="tiefe")+
  theme_classic()+scale_x_datetime(limits = range(okt15$date))
leg_15.10<-get_legend(co2_plot_15.10)

co2_plot_15.10<-co2_plot_15.10+theme(legend.position = "none")


bf_plot_15.10<-ggplot()+geom_line(data=bf_15.10,aes(date,theta,col=as.factor(tiefe)),show.legend = F)+geom_rect(data=event_15.10,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.3)+
  labs(y=expression(theta*"  [VOl %]"),x="",col="tiefe")+  theme_classic()+scale_x_datetime(limits = range(okt15$date))

q_plot_15.10<-ggplot()+geom_line(data=q_15.10,aes(date,q),show.legend = F)+geom_rect(data=event_15.10,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.3)+
  labs(y=expression("q  [ml h"^{-1}*"]"),x="")+  theme_classic()+scale_x_datetime(limits = range(okt15$date))

lf_plot_15.10<-ggplot()+geom_line(data=lf_15.10,aes(date,lf),show.legend = F)+geom_rect(data=event_15.10,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.3)+
  labs(y=expression("LF  ["*mu*"S cm"^{-1}*"]"),x="")+  theme_classic()+scale_x_datetime(limits = range(okt15$date))


#pdf(paste0(plotpfad,"int50mm3h.pdf"))
p1<-plot_grid(co2_plot_10.10,bf_plot_10.10,align = "v",ncol=1)
grid.arrange(p1,leg,layout_matrix=rbind(c(rep(1,11),2),c(rep(1,11),NA)))
dev.off()
plot(okt15$date,okt15$q)
q_plot_15.10

#pdf(paste0(plotpfad,"int50mm8h.pdf"))
p2<-plot_grid(co2_plot_15.10,bf_plot_15.10,q_plot_15.10,lf_plot_15.10,align = "v",ncol=1)
grid.arrange(p2,leg_15.10,layout_matrix=rbind(c(rep(1,11),2),c(rep(1,11),NA)))
dev.off()

ggplot(okt10,aes(theta,CO2_raw,col=as.factor(tiefe)))+geom_point()
ggplot(okt15,aes(theta,CO2_raw,col=as.factor(tiefe)))+geom_point()

ggplot(okt10,aes(temp,CO2_raw,col=as.factor(tiefe)))+geom_point()

ggplot(okt10,aes(date,temp,col=as.factor(tiefe)))+geom_point()

ggplot(okt15,aes(temp,CO2_raw,col=as.factor(tiefe)))+geom_point()

ggplot(okt15,aes(date,temp,col=as.factor(tiefe)))+geom_point()

