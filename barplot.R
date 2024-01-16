df <- read.csv("variance.csv", header= T , sep = ";")
# Basic barplot
library(ggplot2)
p<-ggplot(data=df, aes(x=variable, y=perc, fill = variable)) +
  geom_bar(stat="identity")+
  ylab("% of variance explained")+ xlab("Predictors")+
  guides(colour=guide_legend(override.aes=list(alpha=1, size=3)))+
  theme_bw()+theme(legend.position = "none")+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 

p

# Horizontal bar plot
p + coord_flip()
