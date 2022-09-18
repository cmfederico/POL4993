###ANES 2020

#import csv
anes20<-read.csv("anes_timeseries_2020_csv_20220210.csv")

#load libraries
library(car)
library(psych)
library(ggplot2)
library(AER)
library(estimatr)
library(summarytools)
library(dotwhisker)
library(dplyr)
library(lavaan)
library(semPlot)
library(semTools)
library(hrbrthemes)
library(survey)
library(ggplot2)
library(tidyverse)
library(interactions)


############making authoritarianism scale 
##anes20$V202266 is independence vs respect for elders
##anes20$V202267 is curiosity vs good manners
##anes20$V202268 is obedience vs. self-reliance
##anes20$V202269 is consideration vs. well-behaved

#making all values that are not 1 or 2 NA
anes20$V202266[anes20$V202266<1]<-NA
anes20$V202267[anes20$V202267<1]<-NA
anes20$V202268[anes20$V202268<1]<-NA
anes20$V202269[anes20$V202269<1]<-NA

anes20$V202266[anes20$V202266>2]<-NA
anes20$V202267[anes20$V202267>2]<-NA
anes20$V202268[anes20$V202268>2]<-NA
anes20$V202269[anes20$V202269>2]<-NA

#0-1 function
std01<-function(x){
  min.x<-min(x, na.rm=T)
  max.x<-max(x-min.x, na.rm=T)
  return((x-min.x)/max.x)
}
##0-1 the four auth variables
anes20$V202266<-std01(anes20$V202266)
anes20$V202267<-std01(anes20$V202267)
anes20$V202268<-std01(anes20$V202268)
anes20$V202269<-std01(anes20$V202269)
table(anes20$V202268)

anes20$auth<-anes20$V202266+anes20$V202267+anes20$V202268+anes20$V202269

##variable anes20$auth is the 0-4, 0 being fluid and 4 being fixed
