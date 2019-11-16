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

     