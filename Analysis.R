getwd()
setwd("D:\\R PROGRAMMING - ADVANCED ANALYTICS IN R FOR DATA SCIENCE\\03 Lists in R")
util<- read.csv("Machine-Utilization.csv")
head(util,24)
util$Utlization<- 1 - util$Percent.Idle
util$PosixTime <- as.POSIXct(util$Timestamp, format="%d/%m/%Y %H:%M")
head(util,15)
util$Timestamp<-NULL
util<-util[,c(4,1,2,3)]


RL1<- util[util$Machine=="RL1",]
summary(RL1)
RL1$Machine<-factor(RL1$Machine)

#STATS PART
rl1_stats<- c(min(RL1$Utlization, na.rm = T),mean(RL1$Utlization, na.rm = T),max(RL1$Utlization, na.rm = T))
rl1_stats

rl1_under90_flag<-length(which(RL1$Utlization<0.90))>0
rl1_under90_flag


#Constructing a List

list_rl1 <- list("RL1",rl1_stats,rl1_under90_flag)
list_rl1
names(list_rl1)<-c("MachineName","Stats","LowThreshold")


#Excessing the components of the list
list_rl1[1]  #returns the list
list_rl1[[1]] #returns the vector
list_rl1$MachineName #returns the vector but prettier


#Adding a component into the list
list_rl1$UnknownHours <- RL1[is.na(RL1$Utlization),"PosixTime"]
list_rl1
# Assign NULL to a component to remove it

list_rl1$Data<- RL1
list_rl1$UnknownHours[2]


#Subsetting a list
subset_list_rl1 <- list_rl1[1:3]
subset_list_rl1
subset_list_rl1 <- list_rl1[c(1,3,4)]
subset_list_rl1


#Double Square Brackets are for accesing the members only not for subsetting
# Single Square brackets are for subsetting only

#Time-Series_PLOT

util$
library(ggplot2)
p <- ggplot(data=util)
p + geom_line(aes(x=PosixTime,y=Utlization,colour=Machine),size=1.2)+
  facet_grid(Machine~.) + geom_hline(yintercept = 0.9,
                                     size=1.2, colour="Gray",
                                     linetype=3)
myplot<- p + geom_line(aes(x=PosixTime,y=Utlization,colour=Machine),size=1.2)+
  facet_grid(Machine~.) + geom_hline(yintercept = 0.9,
                                     size=1.2, colour="Gray",
                                     linetype=3)

list_rl1$Plot<- myplot
list_rl1
