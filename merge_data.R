merge_data<-function(co2,bf,lf=0,q=0){
  #co2$date<-ymd_hm(format(co2$date,"%y%m%d%H%M"),tz="CET")
  merged<-merge(co2,bf,all=T)
  if(length(lf)!=0){
  merged<-merge(merged,lf,all=T)}
  if(length(q)!=0){
    merged<-merge(merged,q,all=T)}
  return(merged)
}

