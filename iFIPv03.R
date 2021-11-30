library(dplyr)
library(ggplot2)

#Read in data. All data frp, FanGraphs starts with "FG"
bb18<-read.csv("FGBB18.csv",fileEncoding = 'UTF-8-BOM')
pd18<-read.csv("platediscipline2018.csv",fileEncoding = 'UTF-8-BOM')
su18<-read.csv("FGSum18.csv",fileEncoding = 'UTF-8-BOM')
stat18<-read.csv("FGSBB18.csv",fileEncoding = 'UTF-8-BOM')
Summary18<-read.csv("FGxSum18.csv",fileEncoding = 'UTF-8-BOM')

#merge dataframes
df<-NULL
df<-merge(pd18,bb18,by="Name")
df<-merge(df,su18)
df<-merge(df,stat18)
df<-merge(df,Summary18)

#weed out pitchers with less than 100
df<-df[df$IP>100,]


#create a variable for league, grouping
df$League<-ifelse(df$Team%in%c('ARI','ATL','CHC','CIN','COL','LAD','MIA','MIL','NYM','PHI','PIT','SDP','SFG','STL','WSN'),
                  0,
                  1)
#convert all strings to numbers
for (i in 3:ncol(df)){
  df[,i]<-as.numeric(gsub("%","", df[,i]))
}

#find HR rate
df$HR.<-df$HR/df$TBF

#predict HR rate
HRmod<-lm(HR.~EV+Hard.+FB.+LA,df)
summary(HRmod)
df$xHR.<-predict(HRmod,df)

#find other rate stats
df$BB.<-(df$BB+df$HBP)/df$TBF
df$K.<-df$SO/df$TBF

#separate predictors into dataframe for convenience.
df2<-select(df, c(ERA, K.,BB.,xHR.,EV,League,CSAA))

#build model
mod<-lm(ERA~., df2)

summary(mod)

df$iFIP<-predict(mod,df)



#repeat data process for 2019 data
bb19<-read.csv("FGBB19.csv",fileEncoding = 'UTF-8-BOM')
pd19<-read.csv("platediscipline2019.csv",fileEncoding = 'UTF-8-BOM')
su19<-read.csv("FGSum19.csv",fileEncoding = 'UTF-8-BOM')
stat19<-read.csv("FGSBB19.csv",fileEncoding = 'UTF-8-BOM')
Summary19<-read.csv("FGxSum19.csv",fileEncoding = 'UTF-8-BOM')


df19<-merge(pd19,bb19)
df19<-merge(df19,su19)
df19<-merge(df19,stat19)
df19<-merge(df19,Summary19)
df19<-df19[df19$IP>100,]



for (i in 3:ncol(df19)){
  df19[,i]<-as.numeric(gsub("%","", df19[,i]))
}

df19$League<-ifelse(df19$Team%in%c('ARI','ATL','CHC','CIN','COL','LAD','MIA','MIL','NYM','PHI','PIT','SDP','SFG','STL','WSN'),
                    0,
                    1)

#use previous model for predicted HR rate
df19$HR.<-df19$HR/df19$TBF
df19$xHR.<-predict(HRmod,df19)


df19$BB.<-df19$BB/df19$TBF
df19$K.<-df19$SO/df19$TBF

df19$iFIP<-predict(mod,df19)
df19$iFIPr<-df19$ERA-df19$iFIP



#separate data for convenience. in table/figure making
df3<-select(df, c(ERA,FIP, xFIP, K.,BB.,League,CSAA,HR.,EV,LA,Hard.,FB.))
df19_2<-select(df19, c(ERA, FIP,xFIP, K.,BB.,League,CSAA,HR.,EV,LA,Hard.,FB.))


#make 1st table
tab1<-matrix(nrow=12,ncol=3)

for (i in 1:12){
  tab1[i,1]<-colnames(df3)[i]
  tab1[i,2]<-ifelse(is.numeric(df3[,i]), 
                    ifelse(max(df3[,i])==1,paste(round(mean(df3[,i]),3)*100,"%"), 
                           paste0(round(mean(df3[,i]),3)," (",round(sd(df3[,i]),3),")")), NA)
  tab1[i,3]<-ifelse(is.numeric(df19_2[,i]), 
                    ifelse(max(df19_2[,i])==1,paste(round(mean(df19_2[,i]),3)*100,"%"), 
                           paste0(round(mean(df19_2[,i]),3)," (",round(sd(df19_2[,i]),3), ")")), NA)
}


#write.csv(data.frame(tab1), "Table1U.csv")



#TABLE 2
dfS<-df[order(df$CSAA, decreasing = T),]

#write.csv(head(select(dfS, c(Name,CSAA,ERA) ),10), "Table2.csv")



#Figure 1
ggplot(df, aes(ERA))+geom_histogram(color="black",fill="steelblue")+ylab("Frequency")+labs(title = "Figure 1: 2018 ERA Distribution")

#Melt heatmap data
cormat <- round(cor(df3),3)
head(cormat)

library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+xlab("")+ylab("")+labs(title = "Figure 2: Correlation Heatmap")


#Figure 3
ggplot(df, aes(CSAA, ERA))+ geom_point(color="steelblue")+labs(title = "Figure 3: Called Strikes Above Average v.s. ERA")

#Figure 4
ggplot(df, aes(HR.))+geom_histogram(color="black",fill="steelblue")+xlab("Homerun Rate")+ylab("Frequency")+labs(title = "Figure 4: 2018 HR rate Distribution")



#Figure 5
ggplot(df, aes(xHR.,HR.))+geom_point()+geom_abline(slope = 1, color="red")+
  xlab("Expected Home Run Rate")+ylab("Observed Home Run Rate")+labs(title = "Figure 5: 2018 Expected vs Observed Home Run Rate")


#Figure 6
ggplot(df, aes(iFIP,ERA))+geom_point()+geom_abline(slope = 1, color="red")+
  xlab("Expected Home Run Rate")+ylab("Observed Home Run Rate")+labs(title = "Figure 6: 2018 iFIP vs ERA")

#comparisons for xFIP term
avgHR.FB19<-mean(df19$HR.)/mean(df19$FB./100)
df19$xHR.xFIP<-df19$FB.*avgHR.FB19/100
cor(df19$xHR.xFIP,df19$HR.)



#comparison between years
dfB<-merge(df,df19,by="Name")

#BAD ERA corr
cor(dfB$ERA.x,dfB$ERA.y)


#correlation analysis
round(cor(df$FIP,df$ERA),4)
round(cor(df19$FIP,df19$ERA),4)
round(cor(dfB$FIP.x,dfB$ERA.y),4)
round(cor(dfB$FIP.x,dfB$FIP.y),4)

round(cor(df$xFIP,df$ERA),4)
round(cor(df19$xFIP,df19$ERA),4)
round(cor(dfB$xFIP.x,dfB$ERA.y),4)
round(cor(dfB$xFIP.x,dfB$xFIP.y),4)

round(cor(df$iFIP,df$ERA),4)
round(cor(df19$iFIP,df19$ERA),4)
round(cor(dfB$iFIP.x,dfB$ERA.y),4)
round(cor(dfB$iFIP.x,dfB$iFIP.y),4)




#final test model posthoc
test<-lm(ERA~xFIP+CSAA+EV,df)
summary(test)
