path<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/vorbereitung/"
name="luft"

read_teta<-function(name=0,pfad=path,datum=format(Sys.time()-3600*24,"%d.%m"),a0=1.3,a1=7.8,offset=c(0.03597042,0.03995589,0.04884653,0.01169276)){
tetas<-read.csv(paste0(pfad,"/",name,".dat"),header = F)
tiefen<-tetas[,5:8]/1000


eps<-1+6.19*tiefen-9.72*tiefen^2+24.35*tiefen^3-30.84*tiefen^4+14.73*tiefen^5
eps[,4]<-1.07+6.4*tiefen[,4]-6.4*tiefen[,4]^2+4.7*tiefen[,4]^3
thetas<-(eps-a0)/a1
korr<-t(t(thetas)+offset)
theta<-cbind(thetas,korr)
colnames(theta)<-c(paste0("tiefe",1:4,"raw"),paste0("tiefe",1:4))
library(lubridate)
theta$date<-parse_date_time(paste(tetas[,2],tetas[,3],substr(tetas[,4],nchar(tetas[,4])-4,nchar(tetas[,4])-2),substr(tetas[,4],nchar(tetas[,4])-1,nchar(tetas[,4])),":"),"YjH!M")


return(theta)
}

bf_cal<-read.csv2(paste0(path,"/bf_cal.csv"))
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

luft<-read_teta("luft")
cal<-read_teta("cal")
matplot(cal[120:148,1:4],type="l",lty=1,ylim=c(-0.1,0.4))
matlines(cal[120:148,5:8],type="l",lty=2)

plot(cal$date,cal$tiefe1)

substr(tetas[,4],nchar(tetas[,4])-4,nchar(tetas[,4])-2)
