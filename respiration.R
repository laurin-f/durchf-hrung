#respiration

source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/Versuchsdesign/read_vaisala.R")
co2pfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/"
resp<-read_vaisala(name="respi_undist",pfad=co2pfad,Sonde=1,aggregate = T)
resp<-resp[-nrow(resp),]
plot(resp$date,resp$CO2_raw)


starts<-which(diff(resp$CO2_raw[1:(nrow(resp)-1)])<0&diff(resp$CO2_raw[2:nrow(resp)])>0)+1
starts<-starts[-length(starts)]
sub<-resp[starts,]

points(sub$date,sub$CO2_raw,col=2)
startsub<-vector("list",length(starts))
for (i in 1:length(starts)){
  sub<-resp[starts[i]:(starts[i]+4),]
  sub$t_min<-as.numeric(difftime(sub$date,resp$date[starts[i]],units = "min"))
  startsub[[i]]<-sub
}

sub<-do.call("rbind",startsub)
plot(sub$t_min,sub$CO2_raw)
respfm<-glm(CO2_raw~t_min,data=sub)
slope<-respfm$coefficients[2]
slope#ppm/min
abline(respfm)
max(diff(resp$CO2_raw))
