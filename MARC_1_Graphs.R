# MARC Plants --------------------------------------------------------------
#brought both versions of the MARC trial 1 plants into the system. Starting to create graphs to show growth over time. 

MARC_TRIAL1_A.series<-read.csv("/Users/matthewbarnes/Desktop/Practical Computing /PracCompFinal/Data/MTB_MARC_1_Plant.csv", header=TRUE)
MARC_TRIAL1_A.series

MARC_TRIAL1_B.series<-as.data.frame(read.csv("/Users/matthewbarnes/Desktop/Practical Computing /PracCompFinal/Data/MTB_MARC_1_Plant.BSseries.csv", header = TRUE))
MARC_TRIAL1_B.series

library(ggplot2)
#Started with scatter plots of root and shoot lengths to see how distribution looked across tanks 

marcplant.Bseries.SL<-ggplot(data=MARC_TRIAL1_B.series, aes(x=MARC_TRIAL1_B.series$Tank, y=Shoot.Length)) +
  geom_point(color="Red", alpha=0.9) +
  ggtitle("Shoot Length for All Tanks")
marcplant.Bseries.SL + theme(axis.text.x = element_text(size=8))
marcplant.Bseries.SL

marcplant.Bseries.RL<-ggplot(data=MARC_TRIAL1_B.series, aes(x=MARC_TRIAL1_B.series$Tank, y=Root.Length)) +
  geom_point(color="Blue", alpha=0.9) +
  ggtitle("Root Length for All Tanks")
marcplant.Bseries.RL + theme(axis.text.x = element_text(size=8))
marcplant.Bseries.RL

#Created scatter plot of data to show distribution of root vs shoot length of all 3 species

plot(MARC_TRIAL1_B.series$Root.Length ~ MARC_TRIAL1_B.series$Shoot.Length, col=factor(MARC_TRIAL1_B.series$Species))

J.roemerianus<-MARC_TRIAL1_B.series[which(MARC_TRIAL1_B.series$Species == "Juncus"),]
S.alterniflora<-MARC_TRIAL1_B.series[which(MARC_TRIAL1_B.series$Species == "Spartina"),]
D.spicata<-MARC_TRIAL1_B.series[which(MARC_TRIAL1_B.series$Species == "Distichlis"),]

J.roemerianus.reg<-lm(Juncus$Root.Length ~ Juncus$Shoot.Length)
S.alterniflora.reg<-lm(Spartina$Root.Length ~ Spartina$Shoot.Length)
D.spicata.reg<-lm(Distichlis$Root.Length ~ Distichlis$Shoot.Length)

abline(J.roemerianus.reg, col="black")
abline(S.alterniflora.reg, col="red", lty=2)
abline(D.spicata.reg, col="green", lty=3)

#Boxplots and violin plots to show distribution
boxplot(MARC_TRIAL1_B.series$Shoot.Length ~ MARC_TRIAL1_B.series$Species)
boxplot(MARC_TRIAL1_B.series$Shoot.Length ~ MARC_TRIAL1_B.series$Tank)
boxplot(MARC_TRIAL1_B.series$Root.Length ~ MARC_TRIAL1_B.series$Tank)
