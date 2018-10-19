plot_all<-function(data,event,save=F,name,height=9,width=7,point=F){
  library(ggplot2)
  library(gridExtra)
  library(grid)
  library(cowplot)
  
  co2_plot<-ggplot()+
    geom_rect(data=event,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")+
    labs(y=expression("CO"[2]*"  [ppm]"),x="",col="tiefe")+
    theme_classic()+
    scale_x_datetime(limits = range(data$date))
  
  if(point==T){
      co2_plot<-co2_plot+
        geom_point(data=subset(data,!is.na(CO2)),aes(x=date,y=CO2_raw,col=as.factor(tiefe)),shape=20,size=0.5)+ 
        guides(colour = guide_legend(override.aes = list(size=3)))
    }else{
      co2_plot<-co2_plot+
        geom_line(data=subset(data,!is.na(CO2)),aes(x=date,y=CO2_raw,col=as.factor(tiefe)))
      }
  leg_15.10<-get_legend(co2_plot)
  
  co2_plot<-co2_plot+theme(legend.position = "none")
  
  
  bf_plot<-ggplot()+
    geom_rect(data=event,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")+
    labs(y=expression(theta*"  [VOl %]"),x="",col="tiefe")+
    theme_classic()+scale_x_datetime(limits = range(data$date))
  
  if(point==T){
    bf_plot<-bf_plot+
      geom_point(data=subset(data,!is.na(theta)),aes(date,theta,col=as.factor(tiefe)),show.legend = F,shape=20,size=0.5)
    }else{
      bf_plot<-bf_plot+
        geom_line(data=subset(data,!is.na(theta)),aes(date,theta,col=as.factor(tiefe)),show.legend = F)
    }
  
  p<-plot_grid(co2_plot,bf_plot,align = "v",ncol=1,rel_heights = c(2,1))
  
  if(length(data$lf)!=0){  
    lf_plot<-ggplot()+
      geom_line(data=subset(data,!is.na(lf)),aes(date,lf),show.legend = F)+
      geom_rect(data=event,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")+
    labs(y=expression("LF  ["*mu*"S * cm"^{-1}*"]"),x="")+
      theme_classic()+scale_x_datetime(limits = range(data$date))
    
    p<-plot_grid(co2_plot,bf_plot,lf_plot,align = "v",ncol=1,rel_heights = c(2,1,1))
  }

  if(length(data$q)!=0){ 
      q_plot<-ggplot()+
        geom_line(data=subset(data,!is.na(q)),aes(date,q),show.legend = F)+
        geom_rect(data=event,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")+
    labs(y=expression("q  [ml * min"^{-1}*"]"),x="")+
        theme_classic()+scale_x_datetime(limits = range(data$date))
      
      p<-plot_grid(co2_plot,bf_plot,q_plot,align = "v",ncol=1,rel_heights = c(2,1,1))
  }
  
  if(length(data$q)!=0 & length(data$lf)!=0){
  p<-plot_grid(co2_plot,bf_plot,q_plot,lf_plot,align = "v",ncol=1,rel_heights = c(2,1,1,1))
  }
  
  if(save==T){
    plotpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/abbildungen/plots/"
    
    pdf(paste0(plotpfad,name,".pdf"),width = width,height = height)
      grid.arrange(p,leg_15.10,layout_matrix=rbind(c(rep(1,11),2),c(rep(1,11),NA)))
    dev.off()
  }else{
  return(grid.arrange(p,leg_15.10,layout_matrix=rbind(c(rep(1,11),2),c(rep(1,11),NA))))
}
}