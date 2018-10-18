crop.waage<-function(datum){
library(imager)
jpgpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/waage/"
files<-list.files(paste0(jpgpfad,datum),pattern = ".JPG")
id<-substr(files,5,9)

for (i in 1:length(files)){
  image<-load.image(paste0(jpgpfad,datum,"/",files[i]))
  croped<-imsub(image,1486+713>x&x>1486,1330+345>y&y>1330)
save.image(croped,paste0(jpgpfad,datum,"/cropped/",id[i],"jpg"))
if((i/length(files)*100)%%10==0){
print(paste(i/length(files)*100,"% complete"))}
}
}
crop.waage("10.10")
