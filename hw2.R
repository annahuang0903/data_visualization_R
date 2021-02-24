library(tidyverse)
library(mixtools)
#read data
stocks<-read_csv("stocks.csv")
summary(stocks)
#q1
result1<-prcomp(stocks[,-1])
tibble1 <- tibble(direction=result1$rotation[,1]*(-1), labels=names(stocks[,-1]))
ggplot(tibble1, aes(x=labels, y=direction))+geom_point(aes(color=labels),size=3)+
    ggtitle("Weights of Direction of Maximal Variance")

#q2
cor(stocks[,-1])
pair=dplyr::select(stocks, CXW, MO, DOW, GE, GS)
par(mar=c(1,1,1,1))
pairs(pair, pch=16)

#q3
tibble2 <- tibble(projection=result1$x[,1], date=stocks$X1)
ggplot(tibble2, aes(x=date, y=projection))+geom_point()+
    ggtitle("Projection Onto the First Principle Component")

#q4
result4=mvnormalmixEM(stocks[,-1])
tibble4 <- tibble(posterior=result4$posterior[,'comp.2'], date=stocks$X1, projection=result1$x[,1])
tibble4<-mutate(tibble4,group=ifelse(posterior>0.5,"group1","group2"))
ggplot(tibble4, aes(x=date, y=projection))+geom_point(aes(color=group))+
    ggtitle("Projection Onto the First Principle Component by Gaussian Membership")

#q5
stocks5_1<-mutate(stocks,posterior=result4$posterior[,'comp.2'])%>%
    mutate(group=ifelse(posterior>0.5,"group1","group2"))%>%
    filter(group=="group1")%>%
    dplyr::select(CXW,MO, DOW,GE,GS)
pairs(stocks5_1, pch=16)
stocks5_2<-mutate(stocks,posterior=result4$posterior[,'comp.2'])%>%
    mutate(group=ifelse(posterior>0.5,"group1","group2"))%>%
    filter(group=="group2")%>%
    dplyr::select(CXW,MO, DOW,GE,GS)
pairs(stocks5_2, pch=16)
