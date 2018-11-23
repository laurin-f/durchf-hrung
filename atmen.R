source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/Versuchsdesign/read_vaisala.R")
co2pfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/"
atmen<-read_vaisala(name="atmen",pfad=co2pfad,Sonde=1,aggregate = T)
atmen2<-read_vaisala(name="atmen2",pfad=co2pfad,Sonde=1,aggregate = T)


plot(atmen$date,atmen$CO2_raw,ylab="CO2 [ppm]",xlab="Uhrzeit")


as.numeric("0.3E+01")
