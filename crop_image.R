##############################################################################
#Funktion um den Auschnitt der Waage-Fotos auszuschneiden auf dem das Gewicht zu erkennen ist
#dadurch sind die werte auch in der miniaturansicht des explorers erkennbar 

crop.waage<-function(datum,xpos=1326,ypos=1431,dx=679,dy=438){
  #package für bildbearbeitung
library(imager)
  #dateipfad
jpgpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/waage/"
  #einlesen der dateiname aller .JPGs
files<-list.files(paste0(jpgpfad,datum),pattern = ".JPG")
id<-substr(files,nchar(files)-7,nchar(files)-3)#id aus dateiname extrahieren

#schleife für das Zuschneiden der Bilder
for (i in 1:length(files)){
  #einlesen des i-ten Bildes 
  image<-load.image(paste0(jpgpfad,datum,"/",files[i]))
  #zuschneiden des i-ten Bildes
  croped<-imsub(image,xpos+dx>x&x>xpos,ypos+dy>y&y>ypos)

  #falls nicht vorhanden unterordner /cropped erstellen
  if(!dir.exists(paste0(jpgpfad,datum,"/cropped/"))){
  dir.create(paste0(jpgpfad,datum,"/cropped/"))}
  #abspeichern des zugeschnittenen bildes im unterordner "/cropped"
  save.image(croped,paste0(jpgpfad,datum,"/cropped/",id[i],"jpg"))
  
  #ausgabe des Fortschritts des Prozesses in %
  if((i/length(files)*100)%%10==0){
  print(paste(i/length(files)*100,"% complete"))}
  }
}#ende funktion

#anwenden der Funktion
crop.waage("17.12")


