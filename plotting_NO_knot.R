
  xlab<-"Executive function"
  ########################################################################
  #base model: 
  xba<- c(-1.5,-1,0,0.5,1,2,2.5,-1.5,-1,0,0.5,1,2,2.5,-1.5,-1,0,0.5,1,1,1)
  
  newDFage <- data.frame(
    ex_function=xba,
    AGE_75_d=rep(c(-1,0,1),each=7))
  
  ########################################################################
  #race model
  xre<- c(-1.5,-1,0,0.5,1,2,2.5,-1.5,-1,0,0.5,1,2,2,-1.5,-1,0,0.5,1,2,2,-1.5,-1,0,0.5,1,2,2)
  newDFrace <- data.frame(
    ex_function=xre,
    AGE_75_d=rep(c(0),each=28), 
    race=rep(c("Non-Latino-White","Black","Latino","Asian"),each=7))
  
  ########################################################################
  ##gender model
  xge<- rep(predrange,2)
  newDFgender <- data.frame(
    ex_function=xge,
    AGE_75_d=rep(c(0),each=14),
    GENDER=rep(c("Man","Woman"),each=7))
  
  ########################################################################
  ##education
  xed<- rep(predrange,3)
  newDFedu <- data.frame(
    ex_function=xed,
    AGE_75_d=rep(c(0),each=21),
    yrEDU=rep(c(-4,0,4),each=7),
    race=rep("Non-Latino-White",21))
  
  ########################################################################
  ##familial dementia
  xfh<- rep(predrange,2)
  newDFfhist <- data.frame(
    ex_function=xfh,
    AGE_75_d=rep(c(0),each=14),
    F_Hist=rep(c(0,1),each=7))
  
  ########################################################################
  #depression
  xdpr<- c(-1.5,-1,0,0.5,1,2,2.5,-1.5,-1,0,0.5,1,2,2.5,-1.5,-1,0,0.5,1,2,2)
  newDFdepr <- data.frame(
    ex_function=xdpr,
    AGE_75_d=rep(c(0),each=21),
    depression_01=rep(c(-1,0,1),each=7))
  
  predggba <- predict.lm(results_execfun_nospline[[1]],
                         newDFage, interval = "conf")
  predggra <- predict.lm(results_execfun_nospline[[2]],
                         newDFrace, interval = "conf")
  predggge <- predict.lm(results_execfun_nospline[[3]],
                         newDFgender, interval = "conf")
  predgged <- predict.lm(results_execfun_nospline[[4]],
                         newDFedu, interval = "conf")
  predggfh <- predict.lm(results_execfun_nospline[[5]],
                         newDFfhist, interval = "conf")
  predggdpr <- predict.lm(results_execfun_nospline[[6]],
                          newDFdepr, interval = "conf")



textSize <- 12
PM<- margin(.2, 0.2, .2, .5, "cm")
limin <- -0.02
limax <- 0.8

#plots
#base model with age only

Ba <- ggplot(data = newDFage)+
  geom_ribbon(aes(xba,ymin=predggba[,2],ymax=predggba[,3],
                  group=factor(AGE_75_d,levels = c(1,0,-1)),
                  fill=factor(AGE_75_d,levels = c(1,0,-1))),
              alpha=0.15)+
  geom_line(aes(xba,predggba[,1],
                color=factor(AGE_75_d,
                             levels = c(1,0,-1)),
                linetype=factor(AGE_75_d,
                                levels = c(1,0,-1))),
            size=1)+ 
  theme_minimal()+
  ylab("Predicted log(ECog)")+
  xlab(xlab)+
  theme(legend.justification=c(1,0), 
        legend.position=c(0.95,0.65),
        legend.direction = "vertical",
        legend.text=element_text(size=textSize),
        legend.text.align=1,
        text = element_text(size = textSize))+
  scale_color_discrete(name="Age (years)", 
                       labels = c("85","75","65"))+
  scale_linetype_discrete(name="Age (years)", 
                          labels = c("85","75","65"))+
  coord_cartesian(xlim = NULL, ylim = c(limin,limax), expand = FALSE,
                  default = FALSE, clip = "on")+
  guides(fill = FALSE)

FigureList[[13]]<-Ba

#race/ethnicity
Ra<-ggplot(data = newDFrace)+
  geom_ribbon(aes(xre,ymin=predggra[,2],ymax=predggra[,3],
                  group=race,fill=race),
              alpha=0.15)+
  geom_line(aes(xre,predggra[,1],
                color=race,
                linetype=race),
            size=1)+ 
  theme_minimal()+
  ylab("Predicted log(ECog)")+
  xlab(xlab)+
  theme(legend.justification=c(1,0), 
        legend.position=c(.95,0.62),
        legend.title = element_blank(),
        legend.spacing = unit(.5, 'cm'),
        legend.direction = "vertical",
        legend.text=element_text(size=textSize),
        legend.text.align=0,
        legend.title.align=1,
        text = element_text(size = textSize),
        plot.margin = PM)+
  coord_cartesian(xlim = NULL, ylim = c(limin,limax), expand = FALSE,
                  default = FALSE, clip = "on")+
  guides(fill = FALSE)


FigureList[[14]]<- Ra

#gender
Ge<-ggplot(data = newDFgender)+
  geom_ribbon(
    aes(xge,
        ymin=predggge[,2],
        ymax=predggge[,3],
        group=factor(GENDER,levels = c("Woman","Man")),
        fill=factor(GENDER,levels = c("Woman","Man"))),
    alpha=0.15)+
  geom_line(
    aes(xge,
        predggge[,1],
        color=factor(GENDER,levels = c("Woman","Man")),
        linetype=factor(GENDER,levels = c("Woman","Man"))),
    size=1)+ 
  theme_minimal()+
  ylab("Predicted log(ECog)")+
  xlab(xlab)+
  theme(legend.justification=c(1,0), 
        legend.position=c(0.95,0.70),
        legend.spacing = unit(.1,"cm"),
        legend.direction = "vertical",
        legend.text=element_text(size=textSize),
        legend.title.align=0,
        text = element_text(size = textSize))+
  scale_linetype_discrete(name= "Gender",labels=c("Women","Men"))+
  scale_color_discrete(name= "Gender", labels=c("Women","Men"))+
  coord_cartesian(xlim = NULL, ylim = c(limin,limax), expand = FALSE,
                  default = FALSE, clip = "on")+
  guides(fill = FALSE)

FigureList[[15]]<-Ge

#education duration
Ed <- ggplot(data = newDFedu)+ 
  geom_ribbon(aes(xed,ymin=predgged[,2],ymax=predgged[,3],
                  group=factor(yrEDU),
                  fill=factor(yrEDU)),
              alpha=0.15) +
  geom_line(aes(xed,predgged[,1],
                color=factor(yrEDU),
                linetype=factor(yrEDU)),
            size=1) + 
  theme_minimal() +
  ylab("Predicted log(ECog)") + 
  xlab(xlab)+
  theme(legend.justification=c(1,0), 
        legend.position=c(.95,0.70),
        legend.direction = "vertical",
        legend.text=element_text(size=textSize),
        legend.title = element_text(size=textSize),
        legend.text.align=0,
        legend.title.align=1,
        text = element_text(size = textSize),
        plot.margin = PM)+
  scale_color_discrete(name="Years of education", 
                       labels = c("8 ","12 ","16 "))+
  scale_linetype_discrete(name="Years of education", 
                          labels = c("8 ","12 ","16 "))+
  coord_cartesian(xlim = NULL, ylim = c(limin,limax), expand = FALSE,
                  default = FALSE, clip = "on")+
  guides(fill = FALSE)

FigureList[[16]]<-Ed

#Family history of dementia
Fh <- ggplot(data = newDFfhist)+
  geom_ribbon(aes(xfh,ymin=predggfh[,2],ymax=predggfh[,3],
                  group=factor(F_Hist,levels = c(1,0)),
                  fill=factor(F_Hist,levels= c(1,0))),
              alpha=0.15) + 
  geom_line(aes(xfh,predggfh[,1],
                color=factor(F_Hist,
                             levels = c(1,0)),
                linetype=factor(F_Hist,
                                levels = c(1,0))),
            size=1) + 
  theme_minimal() +
  ylab("Predicted log(ECog)") + 
  xlab(xlab)+
  theme(legend.justification=c(1,0), 
        legend.position=c(0.95,.67),
        legend.spacing = unit(0.2,"cm"),
        legend.text=element_text(size=textSize),
        legend.title = element_text(size=textSize),
        legend.title.align=0,
        text = element_text(size = textSize))+
  scale_color_discrete(name="Family history of dementia", 
                       labels = c("yes","no"))+
  scale_linetype_discrete(name="Family history of dementia", 
                          labels = c("yes","no"))+
  coord_cartesian(xlim = NULL, ylim = c(limin,limax), expand = FALSE,
                  default = FALSE, clip = "on")+
  guides(fill = FALSE)

FigureList[[17]]<-Fh

#depressive symptoms
Dpr <- ggplot(data = newDFdepr)+ 
  geom_ribbon(aes(xdpr,ymin=predggdpr[,2],ymax=predggdpr[,3],
                  group=factor(depression_01,levels = c(1,0,-1)),
                  fill=factor(depression_01,levels = c(1,0,-1))),
              alpha=0.15) + 
  geom_line(aes(xdpr,predggdpr[,1],
                color=factor(depression_01,levels = c(1,0,-1)),
                linetype=factor(depression_01,levels = c(1,0,-1))),
            size=1) + 
  theme_minimal() +
  ylab("Predicted log(ECog)") + 
  xlab(xlab)+
  theme(legend.justification=c(1,0), 
        legend.position=c(0.95,0.73),
        legend.direction = "vertical",
        legend.text=element_text(size=textSize),
        legend.title = element_text(size=textSize),
        legend.text.align=0,
        legend.title.align=0,
        text = element_text(size = textSize),
        plot.margin = PM)+
  scale_linetype_discrete(name="Depressive symptoms")+
  scale_color_discrete(name="Depressive symptoms")+
  coord_cartesian(xlim = NULL, ylim = c(limin,limax), expand = FALSE,
                  default = FALSE, clip = "on")+
  guides(fill = FALSE)

FigureList[[18]]<-Dpr