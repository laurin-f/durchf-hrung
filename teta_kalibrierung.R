source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_teta.R")
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

