
#############################################################
#Funktion zum einlesen der Bodenfeuchtedaten
#inklusive transformation von mV in Vol%
#offsets c(0.03597042,0.03995589,0.04884653,0.01169276)
read_teta<-function(name=0,
                    pfad=path,
                    a0=1.3,
                    a1=7.8,
                    offset=c(0,0,0,0),
                    long_format=T){
tetas<-read.csv(paste0(pfad,name,".dat"),header = F)
tiefen<-tetas[,5:8]/1000
eps<-1+6.19*tiefen-9.72*tiefen^2+24.35*tiefen^3-30.84*tiefen^4+14.73*tiefen^5
eps[,4]<-1.07+6.4*tiefen[,4]-6.4*tiefen[,4]^2+4.7*tiefen[,4]^3
thetas<-(eps-a0)/a1
korr<-t(t(thetas)+offset)
theta<-cbind(thetas,korr)
colnames(theta)<-c(paste0("tiefe",1:4,"raw"),paste0("tiefe",1:4))
library(lubridate)
library(zoo)
zeros<-rollapply((4-nchar(tetas[,4])),1,function(x) paste0(rep(0,x),collapse=""))
theta$date<-parse_date_time(paste0(tetas[,2],tetas[,3],zeros,tetas[,4]),"YjH!M",tz="CET")

if(long_format==T){
  date<-rep(theta$date,4)
  bf<-as.numeric(as.matrix(theta[,5:8]))
  tiefenstufe<-c(-2,-6,-10,-14)
  tiefe<-rep(tiefenstufe,each=length(theta$date))
  theta<-data.frame(date=date,theta=bf,tiefe=tiefe)
  theta<-theta[theta$theta<2,]
}
return(theta)
}


#####################################################
#files laden
path<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/vorbereitung/"
bf_cal<-read.csv2(paste0(path,"bf_cal.csv"))
luft<-read_teta("luft")
cal<-read_teta("cal")
path<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/feuchte/"
bf_09.10<-read_teta("bf_09.10")
bf_long<-read_teta("bf_09.10",long_format = T)
bf_long.2<-read_teta("bf_09.10.2",long_format = T)

summary(bf_long.2)
library(ggplot2)
ggplot(bf_long.2,aes(date,theta,col=as.factor(tiefe)))+
  geom_line()+theme_classic()
test<-matrix(1:9,3,3)
as.integer(test)
matplot(bf_09.10[1000:1625,1:4],ylim=c(0,.5),type="l")
#################################################
#kalibrierung

V<-bf_cal[,3:6]/1000
eps<-1+6.19*V-9.72*V^2+24.35*V^3-30.84*V^4+14.73*V^5
eps[,4]<-1.07+6.4*V[,4]-6.4*V[,4]^2+4.7*V[,4]^3
a0<-1.3
a1<-7.8
theta<-(eps-a0)/a1
matplot(theta)
points(1:4,bf_cal$teta/100)
diff<-bf_cal$teta/100-theta
diffs<-apply(diff,2,mean)

matpoints(1:4,t((diffs)+t(theta)),pch=20)


#############################
#plots

matplot()

