Ecog_counts <- data.frame(lvl=factor(rep(1:4,each=12)),Nitems=factor(rep(1:12,times=4)) ,count=rep(0,48))
for(lvl in 1:4){
  binary_SCD <- ifelse(DF[,Ecog_12items]== lvl,1,0)
  for(n in 1:12){
    sumSCD<- rowSums(binary_SCD,na.rm = T)
    binSCD <- ifelse(sumSCD >= n,1,0)
    Ecog_counts[lvl*12-12+n,3]<- (sum(binSCD,na.rm = T)/1617)*100
  }
}
#########################################################################
require(ggplot2)
EcogSeverity <-ggplot(Ecog_counts)+
  geom_line(aes(x=Ecog_counts$Nitems,y=Ecog_counts$count,
                group=Ecog_counts$lvl,colour=Ecog_counts$lvl),
            size=1.2)+
  xlab("Number of items")+
  ylab("Percentage of the population")+
  scale_color_discrete(name= "Perceived change in 10 years", 
                       labels=c("no change","sometimes","systematically a little worse","systematically much worse"))+
  theme(legend.justification=c(1,0), 
        legend.position=c(0.99,0.69),
        text = element_text(size=14))
##########################################################################

sumEcog <- rowSums(DF[,Ecog_12items],na.rm = T)
tbl <- sjmisc::row_count(DF[,Ecog_12items],count=NA)
hist(sumEcog-(12)+tbl[,13])

tbl <- sjmisc::row_count(DF[,Ecog_12items],count=1)
df <- data.frame(ones=tbl[,13])
tbl <- sjmisc::row_count(DF[,Ecog_12items],count=2)
df$twos <- tbl[,13]
tbl <- sjmisc::row_count(DF[,Ecog_12items],count=3)
df$threes <- tbl[,13]
tbl <- sjmisc::row_count(DF[,Ecog_12items],count=4)
df$fours <- tbl[,13]
tbl <- sjmisc::row_count(DF[,Ecog_12items],count=NA)
df$missings <- tbl[,13]

df2<-data.frame(matrix(nrow = 12,ncol = 5))
for (n in 12:1) {
  df2[n,1]<-n
  df2[n,2]<-mean(df[df$ones==n,"twos"])
  df2[n,3]<-mean(df[df$ones==n,"threes"])
  df2[n,4]<-mean(df[df$ones==n,"fours"])
  df2[n,5]<-mean(df[df$ones==n,"missings"])
}
colnames(df2)<-c("ones","thoos","threes","fours","Nas")
df3 <- melt(df2,id.vars = "ones")
ggplot(df3)+geom_bar(aes(x = ones, y = value,fill=variable),stat='identity')

############################################################################
 prd <- predict(lm(DF1$adj_verbal_episodic_mem~DF1$age+DF1$GENDER+DF1$yrEDUCATION+DF1$logEcog12+DF1$race+DF1$depression_01),newdata = DF1)
 prdecog <- predict(lm(logEcog12~age+GENDER+yrEDUCATION+adj_verbal_episodic_mem+race+depression_01, data = DF1),newdata = DF1)
 EM_cont <- ggplot(data = DF1) + geom_smooth(aes(x=logEcog12,y=prd,color=race),method = "lm")+geom_point(aes(y=DF1$adj_verbal_episodic_mem,x=DF1$logEcog12,color=DF1$race))+ggtitle('Ep mem by log(ECog) (age,sex,educ,race,dpr)')
 EM_u <- ggplot(data = DF1) + geom_smooth(aes(x=logEcog12,y=adj_verbal_episodic_mem,color=race),method = "lm")+geom_point(aes(y=DF1$adj_verbal_episodic_mem,x=DF1$logEcog12,color=DF1$race))+ggtitle('Ep mem by log(ECog) (uncontrolled)')
 Ec_u<-ggplot(data = DF1) + geom_smooth(aes(y=logEcog12,x=adj_verbal_episodic_mem,color=race),method = "lm")+geom_point(aes(x=DF1$adj_verbal_episodic_mem,y=DF1$logEcog12,color=DF1$race))+ggtitle('log(ECog) by Ep mem (uncontrolled)')
 EC_cont<- ggplot(data = DF1) + geom_smooth(aes(y=prdecog,x=adj_verbal_episodic_mem,color=race),method = "lm")+geom_point(aes(x=DF1$adj_verbal_episodic_mem,y=DF1$logEcog12,color=DF1$race))+ggtitle('log(ECog) by Ep mem,(sex, age, dpress, and educ)')
 
 tiff("./SUB_OBJ_mem.tiff",res = 300,width = 2550, height = 3300)
 ggarrange(EM_u+guides(color=F),EM_cont+guides(color=F),Ec_u+  theme(legend.justification=c(1,0), 
                                                                      +                                                                     legend.position=c(.85,0.60)),EC_cont+guides(color=F),ncol = 2,nrow = 2)
 dev.off()



     