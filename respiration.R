#######################################
#respiration undisturbed soil

source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/Versuchsdesign/read_vaisala.R")
co2pfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/"
resp<-read_vaisala(name="respi_undist",pfad=co2pfad,Sonde=1,aggregate = T)
resp<-resp[-nrow(resp),]
plot(resp$date,resp$CO2_raw)


starts<-which(diff(resp$CO2_raw[1:(nrow(resp)-1)])<(-2)&diff(resp$CO2_raw[2:nrow(resp)])>0)+1
#starts<-starts[-length(starts)]
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
abline(respfm)


r<-7.5#cm Radius
A<-pi*r^2#cm2 area

volprobe<-5^3
Vol<-1.803*1000-volprobe

resp_undist<-slope*Vol/A/10^6#ppm/min/cm2

#######################################
#respiration disturbed soil

source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/Versuchsdesign/read_vaisala.R")
co2pfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/"
resp<-read_vaisala(name="respi_dist",pfad=co2pfad,Sonde=1,aggregate = T)
resp<-resp[-((nrow(resp)-20):nrow(resp)),]
plot(resp$date,resp$CO2_raw)


starts<-which(diff(resp$CO2_raw[1:(nrow(resp)-1)])<(-3)&diff(resp$CO2_raw[2:nrow(resp)])>0)+1
#starts<-starts[-length(starts)]
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
abline(respfm)


r<-7.5#cm Radius
A<-pi*r^2#cm2 area
volprobe<-5^3
  Vol<-1.803*1000-volprobe

resp_dist<-slope*Vol/A/10^6#ppm/min/cm2


#######################
resp_undist
resp_dist
