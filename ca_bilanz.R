load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")


hydruspfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/"
projektpfad1<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed/"
projektpfad2<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed2/"
programmpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/programme/Hydrus-1D 4.xx/"
mcpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/montecarlo/"
plotpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/abbildungen/plots/mc/"
load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")
#load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/bodenparameter/params.R")

source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/modellierung/hydrus_input.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/modellierung/montecarlo_function.R")

library(ggplot2)

#tmax_all<-as.numeric(difftime(max(all$date),min(all$date),units = "min"))

tiefenstufen<-c(-2,-6,-10,-14)
####################################
#Monte Carlo
###################################
fixed<-data.frame(thr=0.11,
                  ths=0.75,
                  thr2=0.13,
                  ths2=0.64,
                  thr_bot=0.13,
                  ths_bot=0.64,
                  hseep=-100,
                  l=0.5,
                  bulk=0.7561984,
                  bulk2=1.1480438,
                  difuz=0,
                  difuz2=0,
                  disperl=1.7,
                  disperl2=1.7,
                  cec=0,
                  cec2=0,
                  calcit=0.2,
                  calcit2=0.2,
                  CaAds=500,
                  CaPrec=500)


fixed_co2<-data.frame(act_en=6677,
                      h_crit=-10^6,
                      michaelis=0.19,
                      DispW=0.00106181,
                      Disper=5)
loadfile<-"mc_out-nr_20000-11-25_12.02"
load(file = paste0(mcpfad,loadfile,".R"))

par<-mc[[2]]
rmse<-mc[[1]]
nse<-mc[[3]]

pars<-cbind(par[which.min(rmse),],fixed,fixed_co2)
#
out<-hydrus(params=pars,sleep = 10,dtmin = 0.0001,dtmax = 1,n_nodes = 9,Mat = c(rep(1,3),rep(2,5),3))

out$tiefe<-as.numeric(out$tiefe)
ca_means<-aggregate(out[out$t_min>0,c(1:4,13)],list(out$treatment[out$t_min>0],out$tiefe[out$t_min>0]),function(x) mean(x,na.rm=T))
#######################
#caplot

capath<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/ca/"
load(file=paste0(capath,"cafm.R"))


library(ggplot2)
legendtitle<-expression("Intensität [mm*h"^{-1}*"]")
ggplot()+
  geom_point(data=subset(ic,!is.na(rain_mm_h)),aes(ca,tiefe,col=as.factor(round(rain_mm_h)),shape=as.factor(round(rain_mm_h))))+
  geom_path(data=ca_means,aes(Ca_mod,tiefe,col=as.factor(treatment)))+
  labs(x=expression("Ca"^{"2+"}*"  [mg * l"^{-1}*"]"),y="tiefe [cm]",col=legendtitle,shape=legendtitle)+theme_classic()

ggplot()+geom_point(data=subset(out,tiefe%in%tiefenstufen&!is.na(Ca_mod)),aes(Ca_mod,tiefe))