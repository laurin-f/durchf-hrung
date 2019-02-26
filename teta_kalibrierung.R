source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_teta.R")
#####################################################
#csv datei laden
path<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/vorbereitung/"
bf_cal<-read.csv2(paste0(path,"bf_cal.csv"))
#in der Datei sind messungen der unterschiedlichen Sonden für jede Tiefe, sowie eine Messung mit einer Referenzsonde bei der direkt ein wert von theta ausgegeben wird

#################################################
#kalibrierung

#von mV in V
V<-bf_cal[,3:6]/1000
#epsilon gemäß der Formel aus dem Manual
eps<-1+6.19*V-9.72*V^2+24.35*V^3-30.84*V^4+14.73*V^5
eps[,4]<-1.07+6.4*V[,4]-6.4*V[,4]^2+4.7*V[,4]^3

#a0 und a1 für organischen Boden
a0<-1.3
a1<-7.8
#theta berechnen
theta<-(eps-a0)/a1
#theta plotten
matplot(theta)
#die referenzmessung (theta_cal) dazu plotten
points(1:4,bf_cal$teta/100)

#unterschied zwischen theta und theta_cal
diff<-bf_cal$teta/100-theta
diffs<-apply(diff,2,mean)

#korrigierte Werte dazuplotten
matpoints(1:4,t((diffs)+t(theta)),pch=20)

