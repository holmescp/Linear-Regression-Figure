#scatter plot showing relationship
#creating shape palette

attach(PipelineComparisons2)

palette2 <- data.frame(x = rep(seq(1,5,1),5))
palette2$y <- c(rep(5,5),rep(4,5),rep(3,5),rep(2,5),rep(1,5))
a<-lm(PipelineComparisons2$Kallisto~PipelineComparisons2$Sailfish)
summary(lm(a))$r.squared
a
library(ggplot2)

#Normalized mean expression comparison plot
aplot<-ggplot(PipelineComparisons2,aes(Kallisto,Sailfish))+
  geom_point(aes(colour=Gene,shape=Group),size=5,show.legend = FALSE)+
  scale_shape_manual(values=c(1:10),name="Group",breaks=c(levels(PipelineComparisons$Group)))+
  geom_smooth(method=lm,se=FALSE,color="black",size=1)+
  theme(legend.position=c(.2,.7))+
  xlab("\n Normalized mean expression in Kallisto")+
  ylab("Normalized mean expression in Sailfish \n")+
  theme(axis.text.y=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
  theme(axis.text.x=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
  theme(panel.background=element_rect(fill="white",color="white"))+
  theme(axis.ticks=(element_line(color="black")),axis.line=(element_line(color="black")))
aplot

b<-lm(PipelineComparisons2$K2~PipelineComparisons2$S2)
summary(lm(b))$r.squared
b

#log2 normalized mean expression comparison plot
bplot<-ggplot(PipelineComparisons2,aes(K2,S2))+
  geom_point(aes(colour=Gene,shape=Group),size=5,show.legend = FALSE)+
  scale_shape_manual(values=c(1:10),name="Group",breaks=c(levels(PipelineComparisons$Group)))+
  geom_smooth(method=lm,se=FALSE,color="black",size=1)+
  theme(legend.position=c(.2,.7))+
  xlab("\n log2 Normalized mean expression in Kallisto")+
  ylab("log2 Normalized mean expression in Sailfish \n")+
  theme(axis.text.y=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
  theme(axis.text.x=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
  theme(panel.background=element_rect(fill="white",color="white"))+
  teme(axis.ticks=(element_line(color="black")),axis.line=(element_line(color="black")))

bplot
