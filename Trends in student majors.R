library(mosaic)
library(MASS)
library(scales)
library(dplyr)
library(tidyr)
library("colorspace")
library("ggplot2")
pal <- choose_palette()
install.packages("extrafont")
library("extrafont")
font_import()
#__________________
majors<-read.csv("~/Downloads/Trends in student majors.csv", header=TRUE,check.names=FALSE)
majors<-majors[1:20,]
pmajors<-majors[11:20,]
nmajors<-majors[1:10,]
pmajors<-mutate(pmajors,Year=as.numeric(Year))
nmajors<-mutate(nmajors,Year=as.numeric(Year))
pmajors<-mutate(pmajors,Year=Year+2007)
nmajors<-mutate(nmajors,Year=Year+2007)

View(pmajors)
Year<-pmajors$Year
Finearts<-pmajors$Finearts
Humanities<-pmajors$Humanities
IGS<-pmajors$IGS
STEM<-pmajors$STEM
SS<-pmajors$SS
df1<-data.frame(Year=Year,"Fine arts"=Finearts, "Humanities"=Humanities,"Interdisciplinary"=IGS, "STEM"=STEM, "Social Science"=SS)
df2<-gather(df1,Type_of_major,Percent_of_majors,-Year)

#___________________
g1<-ggplot(df2,aes(x=Year,y=Percent_of_majors,shape=Type_of_major))+geom_point(alpha=1,size=2)+geom_smooth(method="auto",se=FALSE,size=1,color="black")+labs(x="Year",y="Percent of majors")+theme(panel.background = element_rect(hsv(0.57,1,1,alpha=0.3)))+theme(text=element_text(family="Times New Roman", face="bold", size=12))
g1

g2<-ggplot(df2,aes(x=Year,y=Percent_of_majors,shape=Type_of_major))+geom_point(alpha=1,size=2)+theme(legend.position="none")+geom_smooth(method="auto",se=FALSE,size=1,color="black")+labs(x="Year",y="Percent of majors")+theme(panel.background = element_rect(hsv(0.57,1,1,alpha=0.3)))+theme(text=element_text(family="Times New Roman", face="bold", size=12))
g2

plot(Year~Religion,data=)

#___________________
#demographic trends

dmajors<-read.csv("~/Downloads/Demographic data - majors, graduation rates.csv", header=TRUE,check.names=FALSE)    
View(dmajors)
head(dmajors)
Year<-dmajors$Year
Domestic_Multicultural<-dmajors$Domestic_Multicultural
First_Generation<-dmajors$First_Generation
Low_Income<-dmajors$Low_Income
LIFG<-dmajors$LIFG
Humanities<-dmajors$Humanities
IGS<-dmajors$IGS
FA<-dmajors$FA
SS<-dmajors$SS
NSM<-dmajors$NSM
HumanitiesFG<-dmajors$HumanitiesFG
IGSFG<-dmajors$IGSFG
FAFG<-dmajors$FAFG
SSFG<-dmajors$SSFG
NSMFG<-dmajors$NSMFG
HumanitiesLI<-dmajors$HUM_LI
IGSLI<-dmajors$IGS_LI
FALI<-dmajors$FA_LI
SSLI<-dmajors$SS_LI
NSMLI<-dmajors$NSM_LI
HumanitiesFGLI<-dmajors$HUM_FGLI
IGSFGLI<-dmajors$IGS_FGLI
FAFGLI<-dmajors$FA_FGLI
SSFGLI<-dmajors$SS_FGLI
NSMFGLI<-dmajors$NSM_FGLI

df3<-data.frame(Year=Year, "Domestic Multicultural" = Domestic_Multicultural, "First Generation (FG)" = First_Generation, "Low Income" = Low_Income, "Low Income & First Generation" = LIFG)
df4<-gather(df3, Demographic_group, Percent_of_students, -Year)
df5<-data.frame(Year=Year,"Percent of non-white domestic humanities majors" = Humanities, "Percent of non-white fine arts majors"=FA, "Percent of non-white domestic interdisciplinary & general studies majors"=IGS, "Percent of non-white domestic social science majors"=SS, "Percent of non-white domestic natural science & math majors"=NSM)
df6<-gather(df5,Legend,Percent,-Year)
g3<-ggplot(df6,aes(x=Year,y=Percent,shape=Legend,linetype=Legend))+geom_point(alpha=1,size=2)+geom_smooth(method="auto",se=FALSE,size=1,color="black")+xlim(2009,2018)+labs(x="Year",y="Percent of students in each type of major")+theme_bw()+theme(text=element_text(family="Times New Roman", face="bold", size=12))
g3

df7<-data.frame(Year=Year,"Percent of first generation students" = First_Generation, "Percent of first generation humanities majors" = HumanitiesFG, "Percent of first generation Interdisciplinary & General studies majors" = IGSFG, "Percent of first generation fine arts majors" = FAFG, "Percent of first generation social science majors" = SSFG, "Percent of first generation natural science and math majors" = NSMFG)
df8<-gather(df7,Legend,Percent,-Year)
g4<-ggplot(df8,aes(x=Year,y=Percent,color=Legend,shape=Legend))+geom_point(alpha=1,size=2)+geom_smooth(method="auto",se=FALSE,size=1)+labs(x="Year",y="Percent of students in each type of major")+theme(panel.background = element_rect(hsv(0.57,1,1,alpha=0.2)))+theme(text=element_text(family="Times New Roman", face="bold", size=12))
g4

df9<-data.frame(Year=Year, "Percent of low income students"=Low_Income, "Percent of low income humanities majors"=Humanities_LI,"Percent of low income Interdisciplinary & General Studies majors"=IGSLI, "Percent of low income fine arts majors"= FALI, "Percent of low income social sciences majors"=SSLI, "Percent of low income natural sciences and math majors"=NSMLI)
df10<-gather(df9,Legend,Percent,-Year)
g5<-ggplot(df10,aes(x=Year,y=Percent,color=Legend,shape=Legend))+geom_point(alpha=1,size=2)+geom_smooth(method="auto",se=FALSE,size=1)+labs(x="Year",y="Percent of students in each type of major")+theme(panel.background = element_rect(hsv(0.57,1,1,alpha=0.2)))+theme(text=element_text(family="Times New Roman", face="bold", size=12))
g5

HumanitiesFG<-dmajors$HumanitiesFG
IGSFG<-dmajors$IGSFG
FAFG<-dmajors$FAFG
SSFG<-dmajors$SSFG
NSMFG<-dmajors$NSMFG
HumanitiesLI<-dmajors$HUM_LI
IGSLI<-dmajors$IGS_LI
FALI<-dmajors$FA_LI
SSLI<-dmajors$SS_LI
NSMLI<-dmajors$NSM_LI
HumanitiesFGLI<-dmajors$HUM_FGLI
IGSFGLI<-dmajors$IGS_FGLI
FAFGLI<-dmajors$FA_FGLI
SSFGLI<-dmajors$SS_FGLI
NSMFGLI<-dmajors$NSM_FGLI
