################################################################
#Funktion um alle gemessenen Zeitreihen übereinander zu plotten

plot_all<-function(data,#datensatz
                   event=subset(events,start>=min(data$date)&stop<=max(data$date)),#zeitspanne in der beregnet wurde
                   name=NULL,#wenn ein name angegeben wird dann wird eine .pdf datei mit diesem namen gespeichert
                   height=9,#höhe der .pdf
                   width=7,#breite der .pdf
                   point=F){#wenn point =T dann werden für den Plot Punkte anstatt Linien verwendet
  #package für schöne plots
  library(ggplot2)
  #packages um plots zu arrangieren
  library(gridExtra)
  library(cowplot)
 
  #plot der CO2 daten 
  co2_plot<-ggplot()+
    geom_rect(data=event,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")+
    labs(y=expression("CO"[2]*"  [ppm]"),x="",col="tiefe")+
    theme_classic()+
    scale_x_datetime(limits = range(data$date))
  
  #wenn point=T wird geom_point verwendet ...
  if(point==T){
      co2_plot<-co2_plot+
        geom_point(data=subset(data,tiefe!=-17),aes(x=date,y=CO2_raw,col=as.factor(tiefe)),shape=20,size=0.5,na.rm = T)+ 
        guides(colour = guide_legend(override.aes = list(size=3)))
    }else{#ansonsten geom_line
      co2_plot<-co2_plot+
        geom_line(data=subset(data,tiefe!=-17),aes(x=date,y=CO2_raw,col=as.factor(tiefe)),na.rm = T)
    }
  #extrahiern der Legende
  leg<-get_legend(co2_plot)
  #entfernen der Legende
  co2_plot<-co2_plot+theme(legend.position = "none")
  p<-co2_plot
  
  #plot der Bodenfeuchte falls vorhanden
  if(length(data$theta)!=0){   
  bf_plot<-ggplot()+
    geom_rect(data=event,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")+
    labs(y=expression(theta*"  [VOl %]"),x="",col="tiefe")+
    theme_classic()+scale_x_datetime(limits = range(data$date))
  
  #wenn point=T wird geom_point verwendet ...
  if(point==T){
    bf_plot<-bf_plot+
      geom_point(data=subset(data,tiefe!=-17),aes(date,theta,col=as.factor(tiefe)),show.legend = F,shape=20,size=0.5,na.rm = T)
    }else{#ansonsten geom_line
      bf_plot<-bf_plot+
        geom_line(data=subset(data,tiefe!=-17),aes(date,theta,col=as.factor(tiefe)),show.legend = F,na.rm = T)
    }
  
  #vertikalarrangement der zwei plots mit übereinstimmender x-achse
  p<-plot_grid(co2_plot,bf_plot,align = "v",ncol=1,rel_heights = c(2,1))
  }
  
  #plot der Leitfähigkeit falls vorhanden
  if(length(data$lf)!=0){  
    lf_plot<-ggplot()+
      geom_line(data=subset(data,tiefe==-17),aes(date,lf),show.legend = F,na.rm = T)+
      geom_rect(data=event,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")+
    labs(y=expression("LF  ["*mu*"S * cm"^{-1}*"]"),x="")+
      theme_classic()+scale_x_datetime(limits = range(data$date))
    
    #vertikalarrangement der drei plots mit übereinstimmender x-achse
    p<-plot_grid(co2_plot,bf_plot,lf_plot,align = "v",ncol=1,rel_heights = c(2,1,1))
  }

  #plot der Abflussdaten falls vorhanden
  if(length(data$q)!=0){ 
      q_plot<-ggplot()+
        geom_line(data=subset(data,tiefe==-17),aes(date,q),show.legend = F,na.rm = T)+
        geom_rect(data=event,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")+
    labs(y=expression("q  [ml * min"^{-1}*"]"),x="")+
        theme_classic()+scale_x_datetime(limits = range(data$date))
      
      #vertikalarrangement der drei plots mit übereinstimmender x-achse
      p<-plot_grid(co2_plot,bf_plot,q_plot,align = "v",ncol=1,rel_heights = c(2,1,1))
  }
  
  #falls alle Vier datensätze vorliegen
  if(length(data$q)!=0 & length(data$lf)!=0){
    #vertikalarrangement der vier plots mit übereinstimmender x-achse
  p<-plot_grid(co2_plot,bf_plot,q_plot,lf_plot,align = "v",ncol=1,rel_heights = c(2,1,1,1))
  }
  
  #falls der Name angegeben wurde ...
  if(!is.null(name)){
    #dateipfad für plots
    plotpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/abbildungen/plots/"
    #wird eine pdf mit dem angegebenen name gespeichert
    pdf(paste0(plotpfad,name,".pdf"),width = width,height = height)
      grid.arrange(p,leg,layout_matrix=rbind(c(rep(1,11),2),c(rep(1,11),NA)))
    dev.off()
  }else{
    #ansonsten wird der plot angezeigt
  return(grid.arrange(p,leg,layout_matrix=rbind(c(rep(1,11),2),c(rep(1,11),NA))))
}
}