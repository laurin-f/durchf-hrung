################################################################
#Funktion um alle gemessenen Zeitreihen übereinander zu plotten

plot_all<-function(data,#datensatz
                   
                   name=NULL,#wenn ein name angegeben wird dann wird eine .pdf datei mit diesem namen gespeichert
                   height=9,#höhe der .pdf
                   width=7,#breite der .pdf
                   point=F,
                   show.legend=T,
                   ylabs=c(expression("CO"[2]*"  [ppm]"),
                           expression(theta*"  [VOl %]"),
                           expression("LF  ["*mu*"S / cm]"),
                           expression("q  [ml / min]")),
                   scale=T,
                   lfmin=250,
                   date_breaks=T){#wenn point =T dann werden für den Plot Punkte anstatt Linien verwendet
  #package für schöne plots
  library(ggplot2)
  #packages um plots zu arrangieren
  library(gridExtra)
  library(cowplot)
  
  source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/event.R")
  events<-event()
  
  #zeitspanne in der beregnet wurde
  event<-subset(events,start>=min(data$date)&stop<=max(data$date))
  day<-as.numeric(format(data$date,"%d"))
  #plot der CO2 daten 
  co2_plot<-ggplot()+
    geom_rect(data=event,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
    labs(y=ylabs[1],x="",col="Tiefe [cm]")+
    theme_classic()+scale_fill_manual(name="Beregnung",values="blue")+
    geom_line(data=subset(data,tiefe!=-17&tiefe!=0),aes(x=date,y=CO2_raw,col=as.factor(tiefe)),na.rm = T)
  
  if(date_breaks==T){
  co2_plot<-co2_plot+scale_x_datetime(breaks=data$date[format(data$date,"%H%M")=="0000"&ifelse(day>10,day%%2==0,day%%2!=0)], date_labels = "%d%b",limits = range(data$date))
  }else{
    co2_plot<-co2_plot+scale_x_datetime(limits = range(data$date))
  }

  if(scale==F){
    co2_plot<-co2_plot+theme(axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.line.y = element_blank())
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
    labs(y=ylabs[2],x="",col="Tiefe [cm]")+
    geom_line(data=subset(data,tiefe!=-17&tiefe!=0),aes(date,theta,col=as.factor(tiefe)),show.legend = F,na.rm = T)+
    theme_classic()
  
  if(date_breaks==T){
      bf_plot<-bf_plot+scale_x_datetime(breaks=data$date[format(data$date,"%H%M")=="0000"&ifelse(day>10,day%%2==0,day%%2!=0)], date_labels = "%d%b",limits = range(data$date))+theme(axis.text.x = element_blank())
  }else{
    bf_plot<-bf_plot+scale_x_datetime(limits = range(data$date))
  }
  
  if(scale==F){
    bf_plot<-bf_plot+theme(axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.line.y = element_blank())
  }
  #vertikalarrangement der zwei plots mit übereinstimmender x-achse
  p<-plot_grid(co2_plot,bf_plot,align = "v",ncol=1,rel_heights = c(1,1))
  }
  
  #plot der Leitfähigkeit falls vorhanden
  if(length(data$lf)!=0){  
    lf_plot<-ggplot()+
      geom_line(data=subset(data,tiefe==-17),aes(date,lf),show.legend = F,na.rm = T)+
      geom_rect(data=event,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")+
    labs(y=ylabs[3],x="")+
      theme_classic()+scale_y_continuous(limits = c(lfmin,max(data$lf)))#+scale_y_continuous(limits = c(250,max(all_plot$lf,na.rm = T)))
    if(date_breaks==T){
      lf_plot<-lf_plot+scale_x_datetime(breaks=data$date[format(data$date,"%H%M")=="0000"&ifelse(day>10,day%%2==0,day%%2!=0)], date_labels = "%d%b",limits = range(data$date))
    }else{
      lf_plot<-lf_plot+scale_x_datetime(limits = range(data$date))
    }
    #vertikalarrangement der drei plots mit übereinstimmender x-achse
    p<-plot_grid(co2_plot,bf_plot,lf_plot,align = "v",ncol=1,rel_heights = c(1,1,1))
  }

  #plot der Abflussdaten falls vorhanden
  if(length(data$q)!=0){ 
      q_plot<-ggplot()+
        geom_line(data=subset(data,tiefe==-17),aes(date,q_interpol),show.legend = F,na.rm = T)+
        geom_rect(data=event,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")+
    labs(y=ylabs[4],x="")+
        theme_classic()+theme(axis.text.x = element_blank())#+scale_y_continuous(limits = range(all_plot$q_interpol,na.rm = T))
      if(date_breaks==T){
        q_plot<-q_plot+scale_x_datetime(breaks=data$date[format(data$date,"%H%M")=="0000"&ifelse(day>10,day%%2==0,day%%2!=0)], date_labels = "%d%b",limits = range(data$date))
      }else{
        q_plot<-q_plot+scale_x_datetime(limits = range(data$date))
      }
      #vertikalarrangement der drei plots mit übereinstimmender x-achse
      p<-plot_grid(co2_plot,bf_plot,q_plot,align = "v",ncol=1,rel_heights = c(2,1,1))
  }
  
  if(scale==F){
    lf_plot<-lf_plot+theme(axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.line.y = element_blank())
  }
  if(scale==F){
    q_plot<-q_plot+theme(axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.line.y = element_blank())
  }
  #falls alle Vier datensätze vorliegen
  if(length(data$q)!=0 & length(data$lf)!=0){
    #vertikalarrangement der vier plots mit übereinstimmender x-achse
  p<-plot_grid(co2_plot,bf_plot,q_plot,lf_plot,align = "v",ncol=1,rel_heights = c(1.8,1.2,1,1))
  }
  
  #falls der Name angegeben wurde ...
  if(!is.null(name)){
    #dateipfad für plots
    plotpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/abbildungen/plots/"
    #wird eine pdf mit dem angegebenen name gespeichert
    if(show.legend==T){
    pdf(paste0(plotpfad,name,".pdf"),width = width,height = height)

      grid.arrange(p,leg,layout_matrix=rbind(c(rep(1,11),2),c(rep(1,11),NA)))    
      dev.off()
    }else{
      p+ggsave(paste0(plotpfad,name,".pdf"),width = width,height = height)
    }

  }else{
    #ansonsten wird der plot angezeigt    
    if(show.legend==T){
  return(grid.arrange(p,leg,layout_matrix=rbind(c(rep(1,11),2),c(rep(1,11),NA))))
    }else{
      return(p)}
}
}