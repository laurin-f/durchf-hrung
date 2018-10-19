library(readxl)
library(stringr)
capath<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/ca/"
lfpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/leitf/"
#ca17.10<-read_xlsx(paste0(capath,"20180117_Tuttlingen_FVA_Lenny.xlsx"),sheet = 1,skip = 5)
kat17.10<-read.csv(paste0(capath,"ca_17.10.csv"),sep=";",skip=6,na.strings = "n.a.",stringsAsFactors = F)
lf<-read.csv(paste0(lfpfad,"lf_saugkerzen.csv"),sep=";",stringsAsFactors = F)
lf
probe<-kat17.10[grep("T ?[12345]",kat17.10[,2]),2]
ca<-kat17.10[grep("T ?[12345]",kat17.10[,2]),7]
tiefe<-
probe
ca
plot(ca)
tiefe<-as.numeric(str_extract(str_extract(probe,"T ?[12345]"),"[12345]"))
tiefe<-as.numeric(str_extract(str_extract(probe,"T ?[12345]"),"[12345]"))
lf$tiefe
ca[6:10]
probe[6:10]
plot(ca[6:10],lf$lf.æS.cm.)
