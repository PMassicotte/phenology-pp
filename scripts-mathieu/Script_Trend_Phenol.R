#########################Timing and PP calculations #####################
Mat=read.table("~/Desktop/Phenol_Data/Chukchi Sea (7)/outA-MODIS.csv",sep=",",header=F) # A modifier

Mat_Phenol_Out=rep(0,17)
year=Mat[,1]
pixel=Mat[,2]
julien=Mat[,5]
state=Mat[,8]
pp=Mat[,11]

for (y in 1:length(levels(as.factor(year)))){
  ind_Year= year == levels(as.factor(year))[y]
  Mat_Year=Mat[ind_Year,]
  pixel_Year=Mat_Year[,2]
  # Calcul pour chaque pixel
  for (i in 1:length(levels(as.factor(pixel_Year)))){
    print(c(levels(as.factor(year))[y],i))
    ind_Pix= pixel_Year == levels(as.factor(pixel_Year))[i]
    Mat_Year_Pix=Mat_Year[ind_Pix,]
    pixel_Year_Pix=Mat_Year_Pix[,2]
    julien_Year_Pix=Mat_Year_Pix[,5]
    state_Year_Pix=Mat_Year_Pix[,8]
    pp_Year_Pix=Mat_Year_Pix[,11]

    if(length(levels(as.factor(state_Year_Pix)))==1){
      Mat_Phenol_Out=rbind(Mat_Phenol_Out,cbind(Mat_Year_Pix[1,1],Mat_Year_Pix[1,2],length(levels(as.factor(pixel_Year))),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
    }else{
      # Calcul de la saison de croissance
      Growing_Season=max(julien_Year_Pix)-min(julien_Year_Pix)
      PP_Growing_Season=sum(pp_Year_Pix)

      # Calcul des index pour chaque stade phénologique (au dessus de 10, des zones côtières)
      ind_Prebloom= state_Year_Pix == 0 # | state_Year_Pix == 10 # Pre-bloom period
      ind_Bloom= state_Year_Pix == 1 # | state_Year_Pix == 11 # Bloom period
      ind_Postbloom= state_Year_Pix == 2 # | state_Year_Pix == 12# Post-Bloom period
      ind_Postbloom_SCM= state_Year_Pix == 3 #| state_Year_Pix == 13 # Post-Bloom period with SCM
      ind_Wint= state_Year_Pix == 4 #| state_Year_Pix == 14 # Post-Bloom period with SCM
      ind_Fall_Bloom= state_Year_Pix == 5 #| state_Year_Pix == 15 # Fall Bloom

      # Calcul des durées et contribution in PP
      Dur_Prebloom=length(Mat_Year_Pix[ind_Prebloom,5])
      PP_Prebloom=sum(Mat_Year_Pix[ind_Prebloom,11])
      Dur_Bloom=length(Mat_Year_Pix[ind_Bloom,5])
      PP_Bloom=sum(Mat_Year_Pix[ind_Bloom,11])
      Dur_Postbloom=length(Mat_Year_Pix[ind_Postbloom,5])
      PP_Postbloom=sum(Mat_Year_Pix[ind_Postbloom,11])
      Dur_Postbloom_SCM=length(Mat_Year_Pix[ind_Postbloom_SCM,5])
      PP_Postbloom_SCM=sum(Mat_Year_Pix[ind_Postbloom_SCM,11])
      Dur_Fall_Bloom=length(Mat_Year_Pix[ind_Fall_Bloom,5])
      PP_Fall_Bloom=sum(Mat_Year_Pix[ind_Fall_Bloom,11])
      Dur_Winter=length(Mat_Year_Pix[ind_Wint,5])
      PP_Winter=sum(Mat_Year_Pix[ind_Wint,11])

      Mat_Phenol_Out=rbind(Mat_Phenol_Out,cbind(Mat_Year_Pix[1,1],Mat_Year_Pix[1,2],length(levels(as.factor(pixel_Year))),Growing_Season,PP_Growing_Season,Dur_Prebloom,Dur_Bloom,Dur_Postbloom,Dur_Postbloom_SCM,Dur_Fall_Bloom,Dur_Winter,PP_Prebloom,PP_Bloom,PP_Postbloom,PP_Postbloom_SCM,PP_Fall_Bloom,PP_Winter))
    }
  }
}
Mat_Phenol_Out=Mat_Phenol_Out[2:length(Mat_Phenol_Out[,1]),]

write.table(Mat_Phenol_Out,"~/Desktop/Phenol_Data/Chukchi Sea (7)/Output_Phenol_Chukchi_A.csv",sep=";",row.names=F,col.names=F) # A modifier

# Graphic Output
par(mfrow=c(2,2))
boxplot(Mat_Phenol_Out[,2] ~ Mat_Phenol_Out[,1],data=Mat_Phenol_Out,main="Index of pixel",col=c(rep("darkgreen",9),rep("darkred",3)),las=2) # Représentation des num de pix
boxplot(Mat_Phenol_Out[,3] ~ Mat_Phenol_Out[,1],data=Mat_Phenol_Out,main="Number of pixel",col=c(rep("darkgreen",9),rep("darkred",3)),las=2) # Nombre de pixels
boxplot(Mat_Phenol_Out[,4] ~ Mat_Phenol_Out[,1],data=Mat_Phenol_Out,main="Timing of the growing season",col=c(rep("darkgreen",9),rep("darkred",3)),las=2) # Growing season
boxplot(Mat_Phenol_Out[,5] ~ Mat_Phenol_Out[,1],data=Mat_Phenol_Out,ylim=c(0,100000),main="Annual PP",col=c(rep("darkgreen",9),rep("darkred",3)),las=2) # PP

par(mfrow=c(2,2))
boxplot(Mat_Phenol_Out[,7] ~ Mat_Phenol_Out[,1],data=Mat_Phenol_Out,main="Timing of the bloom period",col=c(rep("darkgreen",9),rep("darkred",3)),las=2) # Growing season
boxplot(Mat_Phenol_Out[,8] ~ Mat_Phenol_Out[,1],data=Mat_Phenol_Out,main="Timing of the post-bloom period",col=c(rep("darkgreen",9),rep("darkred",3)),las=2)
boxplot(Mat_Phenol_Out[,9] ~ Mat_Phenol_Out[,1],data=Mat_Phenol_Out,main="Timing of the post-bloom period with a SCM",col=c(rep("darkgreen",9),rep("darkred",3)),las=2)
boxplot(Mat_Phenol_Out[,10] ~ Mat_Phenol_Out[,1],data=Mat_Phenol_Out,main="Timing of the fall bloom period",col=c(rep("darkgreen",9),rep("darkred",3)),las=2)

par(mfrow=c(2,2))
boxplot(Mat_Phenol_Out[,13] ~ Mat_Phenol_Out[,1],data=Mat_Phenol_Out,main="PP of the bloom period",ylim=c(0,80000),col=c(rep("darkgreen",9),rep("darkred",3)),las=2) # Growing season
boxplot(Mat_Phenol_Out[,14] ~ Mat_Phenol_Out[,1],data=Mat_Phenol_Out,main="PP of the post-bloom period",ylim=c(0,20000),col=c(rep("darkgreen",9),rep("darkred",3)),las=2)
boxplot(Mat_Phenol_Out[,15] ~ Mat_Phenol_Out[,1],data=Mat_Phenol_Out,main="PP of the post-bloom period with a SCM",ylim=c(0,15000),col=c(rep("darkgreen",9),rep("darkred",3)),las=2)
boxplot(Mat_Phenol_Out[,16] ~ Mat_Phenol_Out[,1],data=Mat_Phenol_Out,main="PP of the fall bloom period",ylim=c(0,25000),col=c(rep("darkgreen",9),rep("darkred",3)),las=2)
