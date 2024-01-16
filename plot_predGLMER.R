
sjPlot::plot_model(r4,type = "pred", terms= c("div", "tipo2"), pred.type = "re")

library(ggeffects)
pr<- ggpredict(r4, c( "N [all]", "tipo2"), type = "re")
plot(pr)
library(ggplot2)
ggplot(pr ,aes(x , predicted, colour = group )) + 
  facet_wrap(~factor(group))+
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess" , size=0.1, colour = "black", se = F) +
  #xlab("Mean precipitation scaled (mm)")+ ylab("Predicted nonnative proportion")+
  guides(colour=guide_legend(override.aes=list(alpha=1, size=3)))+
  theme_bw()+theme(legend.position = "bottom")+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 

library(effects)
ef <- effect("tipo2", r4)

ggplot(as.data.frame(ee),
       aes(depth,fit,colour=cut,fill=cut))+
  geom_line()+
  ## colour=NA suppresses edges of the ribbon
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  ## add rug plot based on original data
  geom_rug(data=ee$data,aes(y=NULL),sides="b")