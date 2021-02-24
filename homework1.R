library(tidyverse)
library(reshape2)
library(lubridate)

#Restaurant dataset
#read in data
accepts<-read_csv("chefmozaccepts.csv")
cuisine<-read_csv("chefmozcuisine.csv")
hours4<-read_csv("chefmozhours4.csv")
parking<-read_csv("chefmozparking.csv")
geoplace<-read_csv("geoplaces2.csv")
rating<-read_csv("rating_final.csv")
ucuisine<-read_csv("usercuisine.csv")
upayment<-read_csv("userpayment.csv")
uprofile<-read_csv("userprofile.csv")

#mosaic plot of Preference Match vs. Rating
inner_join(rating,ucuisine,"userID")%>%
    rename(Preference=Rcuisine)%>%
    inner_join(cuisine,"placeID")%>%      #join related datasets  
    mutate(PrefMatch=(Preference==Rcuisine),rating=as.factor(rating))%>%  #create new variable
    mutate(PrefMatch=as.factor(PrefMatch))%>%   
    arrange(userID,placeID,desc(PrefMatch))%>%   #arrange preference match
    group_by(userID,placeID)%>% #create tibbles
    filter(row_number()==1)%>%   #get the first row of each combination
    ungroup()%>%
    select(rating,PrefMatch)%>%  #select two columns
    table()%>%  #make contigency table
    mosaicplot(shade =T,main="Mosaic Plot of Preference Match vs. Rating")  #actually can use pipeline

#get rid of 0 rating
#mosaic plot of Preference Match vs. Rating (without 0)
inner_join(rating,ucuisine,"userID")%>%
    rename(Preference=Rcuisine)%>%
    filter(rating==1 | rating==2)%>%  #filter out 0 rating
    inner_join(cuisine,"placeID")%>%    
    mutate(PrefMatch=(Preference==Rcuisine),rating=as.factor(rating))%>% 
    mutate(PrefMatch=as.factor(PrefMatch))%>%   
    arrange(userID,placeID,desc(PrefMatch))%>%   #arrange preference match
    group_by(userID,placeID)%>% #create tibbles
    filter(row_number()==1)%>%   #get the first row of each combination
    ungroup()%>%
    select(rating,PrefMatch)%>% 
    table()%>%  #make contigency table
    mosaicplot(shade =T,main="Mosaic Plot of Preference Match vs. Rating (without 0)") 
    #actually can use pipeline with mosaicplot

#own story
#explore relationship between price match and rating
inner_join(rating, uprofile, "userID")%>%
    inner_join(geoplace,"placeID")%>%
    select(userID, placeID, rating, budget,price)%>%
    filter(budget!="?" & price!="?")%>%
    mutate(PriceMatch=(budget==price),rating=as.factor(rating))%>% 
    mutate(PriceMatch=as.factor(PriceMatch))%>%
    select(rating,PriceMatch)%>%
    table()%>%
    mosaicplot(shade=T, main="Mosaic Plot of Price-Budget Match vs. Rating")

#CitiBike dataset
#read in data
citibike<-read_csv("citibike.csv")

#variation over times of the day
citibike%>%
    mutate(starttime=mdy_hms(citibike$starttime), #extract time
           stoptime=mdy_hms(citibike$stoptime))%>%  
    select(tripduration,starttime,stoptime,`birth year`)%>%
    na.omit()%>%  #omit birth year NAs because they are random customer (more explanation in writeup)
    filter(`birth year`>=1945 & `birth year`<=1999)%>%  #ages between 16-70 
    mutate(start_hour=hour(starttime),stop_hour=hour(stoptime))%>%  #extract hours
    select(start_hour,stop_hour)%>%
    gather(type,hour)%>%  #collapse into type-hour pairs
    ggplot(aes(x=hour))+           
    geom_bar(aes(fill=type),width=0.8,position = "dodge")+ #plot start_hour and stop_hour side by side
    geom_vline(xintercept = 6.5, linetype="dotted", color="red", size=1)+
    geom_vline(xintercept = 9.5, linetype="dotted", color="red", size=1)+
    geom_vline(xintercept = 16.5, linetype="dotted", color="red", size=1)+
    geom_vline(xintercept = 19.5, linetype="dotted", color="red", size=1)+
    theme_minimal()+
    ylab("number of bikes")+
    scale_x_continuous(breaks=seq(0,23,by=1))+
    scale_fill_manual("Types",values = c("blue4","skyblue"),labels=c("starting bikes","stopping bikes"))+
    ggtitle("Number of Bike Trips During Time of Day")+
    theme(plot.title = element_text(hjust = 0.5))

#variation over days of the week
citibike%>%
    mutate(starttime=as.Date(mdy_hms(citibike$starttime)),
           stoptime=as.Date(mdy_hms(citibike$stoptime)))%>%
    select(tripduration,starttime,stoptime,`birth year`)%>%
    na.omit()%>%
    filter(`birth year`>=1945 & `birth year`<=1999)%>%  
    mutate(start_day=weekdays(starttime),stop_day=weekdays(stoptime))%>%
    ggplot()+geom_bar(aes(x=start_day),fill="blue4",width=0.7)+
    theme_minimal()+
    xlab("day")+
    ylab("number of bikes")+
    scale_x_discrete(limits=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
    geom_vline(xintercept = 5.5, linetype="dotted", color="red", size=1)+
    ggtitle("Number of Bike Trips During Day of Week")+
    theme(plot.title = element_text(hjust = 0.5))


