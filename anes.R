###ANES 2020
rm(list=ls())
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
library(corrplot)
library(Hmisc)


## demographics, 2020.
anes20$age1<-anes20$V201507x
anes20$age101<-std01(anes20$V201507x)
anes20$educ1<-ifelse(anes20$V201511x>0, anes20$V201511x, NA)  
anes20$educ101<-std01(anes20$educ1)
anes20$rinc1<-ifelse(anes20$V201617x>0, anes20$V201617x, NA)  
anes20$rinc101<-std01(anes20$rinc1)
anes20$white1<-ifelse(anes20$V201549x==1, 1, ifelse(anes20$V201549x!=1 & anes20$V201549x>0, 0, NA))
anes20$black1<-ifelse(anes20$V201549x==2, 1, ifelse(anes20$V201549x!=2 & anes20$V201549x>0, 0, NA))
anes20$latin1<-ifelse(anes20$V201549x==3, 1, ifelse(anes20$V201549x!=3 & anes20$V201549x>0, 0, NA))
anes20$asian1<-ifelse(anes20$V201549x==4, 1, ifelse(anes20$V201549x!=4 & anes20$V201549x>0, 0, NA))
anes20$male1<-ifelse(anes20$V201600==1, 1, 0)
anes20$college1<-ifelse(anes20$V201511x>3, 1, ifelse(anes20$V201511x<4 & anes20$V201511x>0, 0, NA))
anes20$rhet1<-ifelse(anes20$V201601==1, 1, ifelse(anes20$V201601>0, 0, NA))

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

anes20$auth<-(anes20$V202266+anes20$V202267+anes20$V202268+anes20$V202269)/4

##variable anes20$auth is the 0-1, 0 being fluid and 1 being fixed

#making variable PUBASST 0-1
table(anes20$V202563)
anes20$V202563[anes20$V202563<1]<-NA
anes20$V202563<-std01(anes20$V202563)
anes20$pubasst1<-ifelse(anes20$V202563==1, 0, 1)
table(anes20$pubasst1)



#eco pref
## services and spending
anes20$rserv1<-ifelse(anes20$V201246>0 & anes20$V201246<8,((7-anes20$V201246)/6),NA)
rserv1<-anes20$rserv1
## govt health insurance
anes20$rhel1<-ifelse(anes20$V201252>0 & anes20$V201252<8,((anes20$V201252-1)/6),NA)
rhel1<-anes20$rhel1
## guaranteed jobs
anes20$rjob1<-ifelse(anes20$V201255>0 & anes20$V201255<8,((anes20$V201255-1)/6),NA)
rjob1<-anes20$rjob1
##favor/oppose government trying to reduce income inequality
anes20$ineqredstr<-ifelse(anes20$V202255x>0, (7-anes20$V202255x)/6, NA)
ineqredstr<-anes20$ineqredstr

ecopref<-cbind.data.frame(anes20$rserv1, anes20$rhel1,anes20$rjob1,anes20$ineqredstr)
ecocorr<-rcorr(as.matrix(ecopref), type="pearson")
corrplot(ecocorr$r)
