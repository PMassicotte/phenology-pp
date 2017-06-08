# Script externe
source("~/Dropbox/Script/R_codes/Graphique_ggplot2/Plot_area/Plot_area.R")

# Baffin Bay
Cont_PP_Pre=Mat_Baffin$PP_Pre/Mat_Baffin$PP_Growing*100
Cont_PP_Bloom=Mat_Baffin$PP_Bloom/Mat_Baffin$PP_Growing*100
Cont_PP_Post=Mat_Baffin$PP_Post/Mat_Baffin$PP_Growing*100
Cont_PP_Post_SCM=Mat_Baffin$PP_Post_SCM/Mat_Baffin$PP_Growing*100
Cont_PP_Fall=Mat_Baffin$PP_Fall_Bloom/Mat_Baffin$PP_Growing*100

# Greenland Sea
Cont_PP_Pre=Mat_Greenland$PP_Pre/Mat_Greenland$PP_Growing*100
Cont_PP_Bloom=Mat_Greenland$PP_Bloom/Mat_Greenland$PP_Growing*100
Cont_PP_Post=Mat_Greenland$PP_Post/Mat_Greenland$PP_Growing*100
Cont_PP_Post_SCM=Mat_Greenland$PP_Post_SCM/Mat_Greenland$PP_Growing*100
Cont_PP_Fall=Mat_Greenland$PP_Fall_Bloom/Mat_Greenland$PP_Growing*100

# Beaufort Sea
Cont_PP_Pre=Mat_Beaufort$PP_Pre/Mat_Beaufort$PP_Growing*100
Cont_PP_Bloom=Mat_Beaufort$PP_Bloom/Mat_Beaufort$PP_Growing*100
Cont_PP_Post=Mat_Beaufort$PP_Post/Mat_Beaufort$PP_Growing*100
Cont_PP_Post_SCM=Mat_Beaufort$PP_Post_SCM/Mat_Beaufort$PP_Growing*100
Cont_PP_Fall=Mat_Beaufort$PP_Fall_Bloom/Mat_Beaufort$PP_Growing*100

# Chukchi Sea
Cont_PP_Pre=Mat_Chukchi$PP_Pre/Mat_Chukchi$PP_Growing*100
Cont_PP_Bloom=Mat_Chukchi$PP_Bloom/Mat_Chukchi$PP_Growing*100
Cont_PP_Post=Mat_Chukchi$PP_Post/Mat_Chukchi$PP_Growing*100
Cont_PP_Post_SCM=Mat_Chukchi$PP_Post_SCM/Mat_Chukchi$PP_Growing*100
Cont_PP_Fall=Mat_Chukchi$PP_Fall_Bloom/Mat_Chukchi$PP_Growing*100

# Kara Sea
Cont_PP_Pre=Mat_Kara$PP_Pre/Mat_Kara$PP_Growing*100
Cont_PP_Bloom=Mat_Kara$PP_Bloom/Mat_Kara$PP_Growing*100
Cont_PP_Post=Mat_Kara$PP_Post/Mat_Kara$PP_Growing*100
Cont_PP_Post_SCM=Mat_Kara$PP_Post_SCM/Mat_Kara$PP_Growing*100
Cont_PP_Fall=Mat_Kara$PP_Fall_Bloom/Mat_Kara$PP_Growing*100

par(mfrow=c(3,2))
boxplot(Cont_PP_Pre~Mat_Beaufort$Duration_Growing,col="red")
boxplot(Cont_PP_Bloom~Mat_Beaufort$Duration_Growing,col="red")
boxplot(Cont_PP_Post~Mat_Beaufort$Duration_Growing,col="red")
boxplot(Cont_PP_Post_SCM~Mat_Beaufort$Duration_Growing,col="red")
boxplot((Cont_PP_Post_SCM+Cont_PP_Post)~Mat_Beaufort$Duration_Growing,col="red")
boxplot(Cont_PP_Fall~Mat_Beaufort$Duration_Growing,col="red")

# Graphique des aires de contributions de PP
Dur=Mat_Chukchi$Duration_Growing
ind_Val=!is.na(Dur) & Dur >= 15

Dur=Dur[ind_Val]
Cont_PP_Pre=Cont_PP_Pre[ind_Val]
Cont_PP_Bloom=Cont_PP_Bloom[ind_Val]
Cont_PP_Post=Cont_PP_Post[ind_Val]
Cont_PP_Post_SCM=Cont_PP_Post_SCM[ind_Val]
Cont_PP_Fall=Cont_PP_Fall[ind_Val]

ind_Val=!is.na(Cont_PP_Pre)
Dur=Dur[ind_Val]
Cont_PP_Pre=Cont_PP_Pre[ind_Val]
Cont_PP_Bloom=Cont_PP_Bloom[ind_Val]
Cont_PP_Post=Cont_PP_Post[ind_Val]
Cont_PP_Post_SCM=Cont_PP_Post_SCM[ind_Val]
Cont_PP_Fall=Cont_PP_Fall[ind_Val]

Mat=matrix(0,10,5)
# Intervall de temps
for (i in 1:13){
  if (i==1){
    day=15
    day_2=15+7
  }
  if (i==19){
    day_2=max(Dur)
  }
  ind= Dur >= day & Dur < day_2
  Med_Pre=mean(Cont_PP_Pre[ind])
  Med_Bloom=mean(Cont_PP_Bloom[ind])
  Med_Post=mean(Cont_PP_Post[ind])
  Med_Post_SCM=mean(Cont_PP_Post[ind])
  Med_Fall=mean(Cont_PP_Fall[ind])
  Sum_Med=Med_Pre+Med_Bloom+Med_Post+Med_Post_SCM+Med_Fall
  
  Med_Pre_Norm=mean(Cont_PP_Pre[ind])/Sum_Med*100
  Med_Bloom_Norm=mean(Cont_PP_Bloom[ind])/Sum_Med*100
  Med_Post_Norm=mean(Cont_PP_Post[ind])/Sum_Med*100
  Med_Post_SCM_Norm=mean(Cont_PP_Post[ind])/Sum_Med*100
  Med_Fall_Norm=mean(Cont_PP_Fall[ind])/Sum_Med*100
  Mat[i,]=c(Med_Pre_Norm,Med_Bloom_Norm,Med_Post_Norm,Med_Post_SCM_Norm,Med_Fall_Norm)
  
  day=day+7
  day_2=day_2+7
}

t_int=1:10
grps=c("Pre-Bloom","Bloom","Post-Bloom","SCM","Fall")
grp.dat<-data.frame(Mat,row.names=t_int)
names(grp.dat)<-grps
plot.area(grp.dat,horiz = T,col=c('red','lightgreen','purple'))

