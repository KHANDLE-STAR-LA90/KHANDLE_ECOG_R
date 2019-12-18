################################################################################
## plot it !

## @knitr Ecog_vs_SENAS

# raw_data_averages <- read.csv("/Users/fcorlier/Box Sync/Khandle_data_analysis_FABIAN/R_KHANDLE_project_data/raw_data_averages.csv")[,2:538]
 ListOfVars <- c("semantic_memory","adj_verbal_episodic_mem","executive_function","age","GENDER","race","EDUCATION","yrEDUCATION",Ecog_12items, "RELATIVE_DEMENTIA","SIBLING_DEMENTIA","PARENTAL_DEMENTIA", "Ecog12_including_partial_averages","logEcog12","depression_01")
 
 DF1<-raw_data_averages[,ListOfVars]


require(ggplot2)
require(reshape2)
require(Hmisc)
require(rms)

##plot Ecog scores by SENAS sub-scores
meltDF <- melt(data = DF1,
               measure.vars=colnames(raw_data_averages[,c("adj_verbal_episodic_mem","executive_function")])) 
xmarks <- scale_x_continuous(breaks=seq(-2.5,2.5,0.5))
#CS: this plot doesn't run for me if I try to do the facetting
gg<- ggplot(data = meltDF,aes(x=meltDF$value,y=meltDF$logEcog12))+
  geom_point(alpha =0.9)+
  geom_smooth(method = "loess", span= 0.6)+
  facet_wrap(.~variable)+
  theme(axis.title.x = element_blank(),axis.text.x=element_text(angle=90,size=11),axis.title.y = element_text("log(ECog scores)"))+
  xmarks

tiff(here("loess_pot.tif"),res = 400,width = 2400,height = 1600 )
ggarrange(gg)
dev.off()

# ggplot(data = meltDF,aes(x=meltDF$value,y=meltDF$logEcog12, color=factor(cut(age,c(65,80,90)))))+
#   geom_point(alpha =0.9)+
#   geom_smooth(method = "loess", span= 1)+
#   facet_wrap(.~variable)+
#   theme(axis.title.x = element_blank(),axis.text.x=element_text(angle=90,size=11))+
#   xlim(c(-3,3.2))
# 
# meltDF<-na.omit(meltDF[meltDF$age>79,])
# ggplot(data = meltDF,aes(x=meltDF$value,y=meltDF$logEcog12, color=factor(cut(age,c(80,89)))))+
#   geom_point(alpha =0.9)+
#   geom_smooth(method = "loess", span= 1.2)+
#   facet_wrap(.~variable)+
#   theme(axis.title.x = element_blank(),axis.text.x=element_text(angle=90,size=11))+
#   xlim(c(-3,3.2))
# 
# ggplot(data = meltDF,aes(x=meltDF$value,y=meltDF$logEcog12, color=factor(cut(depression_01,c(-2,0,4)))))+
#   geom_point(alpha =0.9)+
#   geom_smooth(method = "loess", span= 1)+
#   facet_wrap(.~variable)+
#   theme(axis.title.x = element_blank(),axis.text.x=element_text(angle=90,size=11))+
#   xlim(c(-3,3.2))

# #testing DiagrammeR
# require(DiagrammeR)
# file.edit("Diagrams/Diagam_syntax.gv")
# grViz("Diagrams/Diagam_syntax.gv")


## @knitr restricted-cubic-splines

par(mfrow=c(1,1))

DF2 <- na.omit(DF1[,c("logEcog12","adj_verbal_episodic_mem")])

y<- DF2$logEcog12
x<-DF2[,"adj_verbal_episodic_mem"]


mRCS <- ols(DF2[,1]~rcs(x, 
                  quantile(x,c(0, .05, .275, .5, .775, .95, 1),
                           include.lowest=T,na.rm = F)))
tiff(here("Memory_RCS.tif"),res = 400,width = 2400,height = 1600 )

plot(x,mRCS$fitted.values,
     col = "red",
     xlim = c(min(x),max(x)),
     ylim = c(min(y),max(y)),
     xlab="Episodic memory scores",
     ylab = "Fitted log(ECog scores)")
points(x,y)
#title("Episodic memory scores")

dev.off()
      

      

DF2 <- na.omit(DF1[,c("logEcog12","executive_function")])

y<- DF2$logEcog12
x<-DF2$executive_function
#x<-DF2$executive_function

mRCS <- ols(DF2[,1]~rcs(x, 
                        quantile(x,c(0, .05, .275, .5, .775, .95, 1),
                                 include.lowest=F)))
tiff(here("Exec_fun_RCS.tif"),res = 400,width = 2400,height = 1600 )
plot(x,mRCS$fitted.values,
           col = "red",
           xlim = c(min(x),max(x)),
           ylim = c(min(y),max(y)),
          yaxt="none",
          ylab = "",
          xlab = "Executive function scores")
      points(x,y)
      #title("Executive function scores")
      dev.off()
## @knitr Ecog_vs_SENAS_lsp
      
spmodel <- rms::ols(DF2$logEcog12~ 
                            rms::lsp(DF2$executive_function,c(1.1)))
plot(x,spmodel$fitted.values,
        col = "red",
        xlim = c(min(x),max(x)),
        ylim = c(min(y),max(y)),
        yaxt="none",
        ylab = "",
        xlab="")
points(x,y)

