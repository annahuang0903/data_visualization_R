library(tidyverse)
library(ggmap)

#latitude and longitute
ggmap(nyroad) +
    geom_point(aes(x=longitude, y=latitude), color="green",data=locations, size=0.1,shape=13)+
    labs(title="Trees in New York")+
    theme_void()

ggmap(nyroad) + geom_density2d(aes(x=longitude, y=latitude), data=locations, size=.5) +
    xlab("longitude") +
    ylab("latitude")

ggmap(nyroad) + stat_density_2d(aes(x=longitude, y=latitude, fill=..level..), data=locations, geom = "polygon") +
    xlab("longitude") +
    ylab("latitude")

ggmap(nyroad) + stat_density_2d(aes(x=longitude, y=latitude, fill=..level..), data=locations, geom = "polygon", bins=10, alpha=.2) +
    xlab("longitude") +
    ylab("latitude") +
    labs(title="Trees in New York")

#boroname
summary(tree2015$boroname)
ggplot()+geom_bar(aes(x=boroname),data=locations,fill="blue4",width=0.7)+
    theme_minimal()+
    xlab("boroname")+
    ylab("number of trees")+
    ggtitle("Number of Trees in Each Borough")

#species
species<-mutate(species,spc_common=as.factor(species_common_name))
levels(species$spc_common)<-tolower(levels(species$spc_common))
levels(species$spc_common)
levels(species$spc_common)[36]<-"london planetree"
levels(species$spc_common)[40]<-"purple-leaf plum"
levels(species$spc_common)[13]<-"crab apple"
levels(species$spc_common)[23]<-"golden raintree"
locations<-mutate(locations,spc_common=as.factor(spc_common))
levels(locations$spc_common)<-tolower(levels(locations$spc_common))
alldata<-left_join(locations,species,by="spc_common")
trees2=filter(alldata,is.na(species_common_name))
trees2$spc_common

#fall_color
na.omit(alldata)%>%ggplot()+geom_bar(aes(x=fall_color),fill="blue4",width=0.7)+
    theme_minimal()+
    xlab("fall_color")+
    ylab("number of trees")+
    ggtitle("Number of Trees in Each Fall_color")

#tree_size
summary(alldata$tree_size)
na.omit(alldata)%>%ggplot()+geom_bar(aes(x=tree_size),fill="blue4",width=0.7)+
    theme_minimal()+
    xlab("tree_size")+
    ylab("number of trees")+
    ggtitle("Number of Trees in Each Size")

ggplot(na.omit(alldata), aes(x=tree_size, y=tree_dbh)) + 
    stat_summary(fun.y=mean, geom="point") +
    stat_summary(fun.y=mean, geom="line", aes(group=1)) +
    stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)

ggmap(nyroad) +
    geom_point(aes(x=longitude, y=latitude,color=spc_common),data=na.omit(alldata), size=0.1,shape=13)+
    labs(title="Trees in New York")+
    theme_void()

ggmap(nyroad) +
    geom_point(aes(x=longitude, y=latitude,color=tree_size),data=na.omit(alldata), size=0.1,shape=13)+
    labs(title="Trees in New York")+
    theme_void()

#status
ggmap(nyroad) +
    geom_point(aes(x=longitude, y=latitude,color=status),data=alldata, size=0.1,shape=13)+
    labs(title="Trees in New York")+
    theme_void()

ggmap(nyroad) +
    geom_point(aes(x=longitude, y=latitude,color=status),data=alldata%>%filter(status!="Alive"), size=0.1,shape=13)+
    labs(title="Trees in New York")+
    theme_void()

ggmap(nyroad) +  stat_density_2d(aes(x=longitude, y=latitude, fill=..level..), data=alldata%>%filter(status=="Dead"), geom = "polygon", bins=10, alpha=.2) +
    xlab("longitude") +
    ylab("latitude") +
    labs(title="Trees in New York")

ggplot(data=tree_district%>%filter(status=="Dead"))+geom_bar(aes(x=boroname))


#health
ggmap(nyroad) +
    geom_point(aes(x=longitude, y=latitude,color=health),data=alldata, size=0.1,shape=13)+
    labs(title="Trees in New York")+
    theme_void()

ggmap(nyroad) +
    geom_point(aes(x=longitude, y=latitude),data=alldata%>%filter(health=="Poor"), size=0.1,shape=13)+
    labs(title="Trees in New York")+
    theme_void()

ggmap(nyroad) +  stat_density_2d(aes(x=longitude, y=latitude, fill=..level..), data=tree_district%>%filter(health=="Good"), geom = "polygon", bins=10, alpha=.2) +
    xlab("longitude") +
    ylab("latitude") +
    labs(title="Trees in New York")

#poor health trees
ggmap(nyroad) +  stat_density_2d(aes(x=longitude, y=latitude, fill=..level..), data=tree_district%>%filter(health=="Poor"), geom = "polygon", bins=10, alpha=.2) +
    xlab("longitude") +
    ylab("latitude") +
    labs(title="Distribution of Unhealthy Trees in New York")
tree_district<-mutate(tree_district,health2=ifelse(health=="Good","Good","Not Good"))
table(tree_district%>%select(boroname,health2))%>%mosaicplot(shade=T,main="Relationship Between Tree Health and Borough")

#tree problems
summary(tree_district%>%filter(health=="Poor")%>%select(problems))
summary(tree_district%>%filter(health=="Poor")%>%select(spc_common))
summary(tree_district$spc_common)
problem_stone<-filter(tree_district,problems=="Stones")
ggmap(nyroad) +  stat_density_2d(aes(x=longitude, y=latitude, fill=..level..), data=problem_stone, geom = "polygon", bins=10, alpha=.2) +
    xlab("longitude") +
    ylab("latitude") +
    labs(title="Trees in New York")

#income and number of trees
#calculate number of trees per person
tree_district<-mutate(tree_district,zipcode=as.factor(zipcode))
number<-tree_district%>%group_by(boroname)%>%count()
population<-borough%>%select(boroname,population,density,median_household_income,mean_household_income)
tree_district2<-inner_join(number,population,by="boroname")
ggplot(tree_district2)+geom_point(aes(x=density,y=n))
ggplot(tree_district2)+geom_point(aes(x=median_household_income,y=n))

#my part
tree2015<-read.csv("new_york_tree_census_2015.csv")
tree1995<-read.csv("new_york_tree_census_1995.csv")
species<-read.csv("new_york_tree_species.csv")
borough<-read.csv("borough_data.txt",header = T)
names(tree2015)
source = "google" 
locations<-mutate(tree2015,address=as.factor(address),zipcode=as.factor(zipcode),zip_city=as.factor(zip_city),
                  cb_num=as.factor(cb_num),borocode=as.factor(borocode),boroname=as.factor(boroname),
                  cncldist=as.factor(cncldist))
tree_district<-left_join(locations,borough,by="boroname")
tree1995<-mutate(tree1995, boroname=as.factor(borough))
tree_district1995<-left_join(tree1995,borough,by="boroname")
#overall dead tree in 1995
dead_1995<-filter(tree_district1995,status=="Dead")
lon=c(-74.1,-73.7)
lat=c(40.5,40.9)
nyroad <- get_map(location = c(lon = mean(lon), lat = mean(lat)),maptype='roadmap', zoom=10, source=source)
ggmap(nyroad) +  stat_density_2d(aes(x=longitude, y=latitude, fill=..level..), data=dead_1995, geom = "polygon", bins=10, alpha=.2) +
    xlab("longitude") +
    ylab("latitude") +
    labs(title="Distribution of Dead Trees in New York 1995")
#bronx dead satellite 1995
lon=c(-74.1,-73.7)
lat=c(40.7,40.9)
nyroad <- get_map(location = c(lon = mean(lon), lat = mean(lat)),maptype='satellite', zoom=12, source=source)
ggmap(nyroad) +  stat_density_2d(aes(x=longitude, y=latitude, fill=..level..), data=dead_1995, geom = "polygon", bins=10, alpha=.2) +
    xlab("longitude") +
    ylab("latitude") +
    labs(title="Distribution of Dead Trees in Bronx Area 1995")
#overall dead tree in 2015
lon=c(-74.1,-73.7)
lat=c(40.5,40.9)
nyroad <- get_map(location = c(lon = mean(lon), lat = mean(lat)),maptype='roadmap', zoom=10, source=source)
ggmap(nyroad) +  stat_density_2d(aes(x=longitude, y=latitude, fill=..level..), data=tree_district%>%filter(status=="Dead"), geom = "polygon", bins=10, alpha=.2) +
    xlab("longitude") +
    ylab("latitude") +
    labs(title="Distribution of Dead Trees in New York 2015")
#bronx dead satellite 2015
lon=c(-74.1,-73.7)
lat=c(40.7,40.9)
nyroad <- get_map(location = c(lon = mean(lon), lat = mean(lat)),maptype='satellite', zoom=12, source=source)
ggmap(nyroad) +  stat_density_2d(aes(x=longitude, y=latitude, fill=..level..), data=tree_district%>%filter(status=="Dead"), geom = "polygon", bins=10, alpha=.2) +
    xlab("longitude") +
    ylab("latitude") +
    labs(title="Distribution of Dead Trees in Bronx Area 2015")
#mosaic dead 2005
par(mfrow=c(1,1))
tree_district<-mutate(tree_district,status2=ifelse(status=="Dead" | status=="Stump","Dead","Alive"))
table(tree_district%>%select(boroname,status2))%>%mosaicplot(shade=T,main="Relationship Between Tree Status and Borough")

#dead percentage and helpful guard
death<-tree_district%>%filter(status=="Dead")%>%group_by(boroname)%>%count()
boro<-tree_district%>%group_by(boroname)%>%count()
death_data<-inner_join(death,boro,by="boroname")%>%mutate(percentage_death=n.x/n.y)
guard<-tree_district%>%filter(guards=="Helpful")%>%group_by(boroname)%>%count()
boro<-tree_district%>%group_by(boroname)%>%count()
guard_data<-inner_join(guard,boro,by="boroname")%>%mutate(percentage_guard=n.x/n.y)
death_guard<-inner_join(death_data,guard_data,by="boroname")
guard_health<-tree_district%>%filter((health=="Fair"|health=="Good"|health=="Poor")&(guards=="Harmful"|guards=="Helpful"))
table(guard_health$guards,guard_health$health)[2:4,2:4]%>%mosaicplot(shade=T,main="Relationship Between Types of Guards and Tree Health")

guard_district<-tree_district%>%filter(guards=="Harmful"|guards=="Helpful")%>%group_by(boroname,guards)%>%count()%>%mutate(number_of_trees=n)
health_district<-tree_district%>%filter(health=="Fair"|health=="Good"|health=="Poor")%>%group_by(boroname,health)%>%count()%>%mutate(number_of_trees=n)
ggplot(data=guard_district,aes(x=boroname,y=number_of_trees,fill=guards))+geom_bar(stat="identity", position="dodge")+
    ggtitle("Types of Tree Guards in Different Boroughs")
guard<-guard_district%>%filter(guards=="Helpful")

#final
library(reshape2)
library(tidyverse)
library(scales)
library(ggmap)
library(MASS)
library(mapproj)
library(ggsubplot)
tree2015<-read.csv("new_york_tree_census_2015.csv")
tree1995<-read.csv("new_york_tree_census_1995.csv")
borough<-read.csv("borough_data.txt",header = T)
source = "google" 
locations<-mutate(tree2015,address=as.factor(address),zipcode=as.factor(zipcode),zip_city=as.factor(zip_city),
                  cb_num=as.factor(cb_num),borocode=as.factor(borocode),boroname=as.factor(boroname),
                  cncldist=as.factor(cncldist))
tree_district<-left_join(locations,borough,by="boroname")
tree1995<-mutate(tree1995, boroname=as.factor(borough))
tree_district1995<-left_join(tree1995,borough,by="boroname")
#overall dead tree in 1995
dead_1995<-filter(tree_district1995,status=="Dead")
lon=c(-74.1,-73.7)
lat=c(40.5,40.9)

dead_2015<-tree_district%>%filter(status=="Dead")%>%dplyr::select(status,latitude,longitude,boroname)%>%mutate(year=2015)
dead_1995<-dead_1995%>%dplyr::select(status,latitude,longitude,boroname)%>%mutate(year=1995)%>%filter(latitude!=1)
dead_tree<-rbind(dead_1995,dead_2015)
dead_tree<-mutate(dead_tree,year=as.factor(year))
write.csv(dead_tree,file="dead_tree.csv")

ggmap(nyroad) +  
    stat_density_2d(aes(x=longitude, y=latitude,fill=year,alpha=..level..),data=dead_tree, geom = "polygon", bins=10, alpha=.2) +
    labs(title="Change in Density of Dead Trees over NYC in 20 Years")+
    theme( axis.text.x = element_blank(), 
           axis.text.y = element_blank(), 
           axis.title.x = element_blank(), 
           axis.title.y = element_blank())

# Calculate the common x and y range for geyser1 and geyser2
xrng = range(c(dead_1995$longitude, dead_2015$longitude))
yrng = range(c(dead_1995$latitude, dead_2015$latitude))


# Calculate the 2d density estimate over the common range
d1 = kde2d(dead_1995$longitude,dead_1995$latitude, lims=c(xrng, yrng), n=300)
d2 = kde2d(dead_2015$longitude,dead_2015$latitude, lims=c(xrng, yrng), n=300)

# Confirm that the grid points for each density estimate are identical
identical(d1$x, d2$x) # TRUE
identical(d1$y, d2$y) # TRUE

# Calculate the difference between the 2d density estimates
diff12 = d1 
diff12$z = d2$z - d1$z

## Melt data into long format
# First, add row and column names (x and y grid values) to the z-value matrix
rownames(diff12$z) = diff12$x
colnames(diff12$z) = diff12$y

# Now melt it to long format
diff12.m = melt(diff12$z, id.var=rownames(diff12))
names(diff12.m) = c("Lon","Lat","z")

nyroad <- get_map(location = c(lon = mean(lon), lat = mean(lat)),maptype='terrain',zoom=10, source=source)
# Plot difference between density
ggmap(nyroad)+
    stat_contour(aes(Lon, Lat, z=z, fill=..level..), geom="polygon", alpha=0.5, data=diff12.m, binwidth=3) +
    scale_fill_gradient2(low="blue4",mid="seashell", high="brown", midpoint=0,breaks = c(-10,0,20),label=c("decreasing","no change","increasing")) +
    scale_colour_gradient2(low=muted("blue4"), mid="seashell", high=muted("brown"), midpoint=0) +
    guides(fill=guide_colorbar(title="Change in Density of Dead Trees"),color=F,label.position = "right")+
    labs(title="Change in Density of Dead Trees Over NYC in 20 Years and Number of Tree Guards")+
    theme(axis.text.x = element_blank(), 
          axis.text.y = element_blank(), 
           axis.title.x = element_blank(), 
           axis.title.y = element_blank())+
    geom_errorbar(size =3, colour = "darkorange1", alpha = .6, width = 0,aes(x = -74.02, ymin = 40.77, ymax = 40.85))+ #M
    geom_errorbar(size =3, colour = "darkorange1", alpha = .6, width = 0,aes(x = -74.15, ymin = 40.59, ymax = 40.60))+ #S
    geom_errorbar(size =3, colour = "darkorange1", alpha = .6, width = 0,aes(x = -73.87, ymin = 40.85, ymax = 40.87))+ #Bx
    geom_errorbar(size =3, colour = "darkorange1", alpha = .6, width = 0,aes(x = -73.8, ymin = 40.77, ymax = 40.81))+ #Q
    geom_errorbar(size =3, colour = "darkorange1", alpha = .6, width = 0,aes(x = -73.9, ymin = 40.55, ymax = 40.65))+ #Bk
    geom_text(aes(-74.02, 40.86), size=3, colour = "brown", 
              label = "17559")+
    geom_text(aes(-74.15, 40.61),size=3, colour = "brown", 
              label = "3078")+
    geom_text(aes(-73.87, 40.88),size=3, colour = "brown", 
              label = "4605")+
    geom_text(aes(-73.8, 40.82),size=3, colour = "brown", 
              label = "7153")+
    geom_text(aes(-73.9, 40.66),size=3, colour = "brown", 
              label = "19471")+
    geom_text(aes(-73.65, 40.45),size=3, colour = "brown", 
              label = "* Bar Represents Number of Tree Guards")
    








