#####Ploteo el efecto aleatorio######
ggCaterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE) {
  require(ggplot2)
  f <- function(x) {
    pv   <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
    pDf  <- data.frame(y=unlist(x)[ord],
                       ci=1.96*se[ord],
                       nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                       ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                       ind=gl(ncol(x), nrow(x), labels=names(x)))
    
    if(QQ) {  ## normal QQ-plot
      p <- ggplot(pDf, aes(nQQ, y))
      p <- p + facet_wrap(~ ind, scales="free")
      p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
    } else {  ## caterpillar dotplot
      p <- ggplot(pDf, aes(ID, y)) + coord_flip()
      if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap(~ ind)
      } else {           ## different scales for random effects
        p <- p + facet_grid(ind ~ ., scales="free_y")
      }
      p <- p + xlab("Urban typologies") + ylab("Random effects intercept")
    }
    
    p <- p + theme(legend.position="none")
    #p <- p + geom_hline(yintercept=0)
    p <- p + geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="darkgrey", size = 0.1, alpha = 0.5)
    p <- p + geom_point(colour="hotpink")
    #p<- p + geom_line(aes(as.factor(ID), 
               #   group = 1), 
              #color = "hotpink", size = 1)
    p<- p + theme_minimal()#+theme(axis.text.y=element_blank())
    return(p)
  }
  
  lapply(re, f)
}

ggCaterpillar(ranef(r4,condVar=TRUE), QQ=FALSE, likeDotplot=FALSE)[["tipo2"]]  ## using ggplot2

ranef(r4,condVar=TRUE)

fund <- as.data.frame(ranef(r4,condVar=TRUE)[["fundacionm"]])


sjPlot::plot_model(modelo_b,type = "re",
                   #axis.labels =c("Metropoli", "Big cities", "Intermediate cities", "Small cities", "Town", "Remote" ),
                   vline.color = "grey",
                   show.intercept =TRUE, show.values = TRUE, value.offset = .5, show.p=TRUE) 

gg+ ggplot2::scale_y_continuous(limits = c(-0.04, 0.04))

sjPlot::plot_model(r4,type = "re",
                   axis.labels =c("Metropoli", "Big cities", "Intermediate cities", "Small cities", "Town", "Remote" ),
                   vline.color = "grey",
                   show.intercept =TRUE, show.values = TRUE, value.offset = .5, show.p=TRUE, )

sjPlot::plot_model(r4, type = "diag", show.values = TRUE)
sjPlot::plot_model(r5, type = "diag", show.values = TRUE)



re = ranef(r4)$tempmean
qplot(x = re, geom = 'density', xlim = c(-3, 3))


empirical_bayes_data <- ranef(r5) # extract random effects for each school

empirical_bayes_intercepts <- empirical_bayes_data$tipo2["(Intercept)"]

empirical_bayes_slopes <- empirical_bayes_data$tipo2["tempmean"] # extracts the SES/slope EB estimates from the list

bind_cols(empirical_bayes_intercepts, empirical_bayes_slopes) %>%  # combine EB slopes and intercepts into a useable dataframe for graphing
  ggplot(mapping = aes(x = tempmean, y = '(Intercept)')) +
  geom_point() 



