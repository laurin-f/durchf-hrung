
#############################################################
#Funktion zum einlesen der Bodenfeuchtedaten
#inklusive transformation von mV in Vol%
#offsets c(0.03597042,0.03995589,0.04884653,0.01169276)
read_teta<-function(name=0,
                    pfad=path,
                    a0=1.3,
                    a1=7.8,
                    offset=c(0.03597042,0.03995589,0.04884653,0.01169276),
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
  theta<-theta[!is.na(theta$tiefe),]
  dopplungen<-which(diff(theta$date)==0)
  if(length(dopplungen)!=0){
  theta<-theta[-(dopplungen+1),]}
}

return(theta)
}