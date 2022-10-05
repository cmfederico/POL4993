rm(list=ls())
library(haven)
library(psych)
library(corrplot)
library(Hmisc)
ess8 <- read_dta(file="ess8.dta")

#federico 0-1 recode
std01<-function(x){
  min.x<-min(x, na.rm=T)
  max.x<-max(x-min.x, na.rm=T)
  return((x-min.x)/max.x)}

######demographics
## income
ess8$rinc01<-std01(ess8$hinctnta)

## age
ess8$age<-as.numeric(ess8$agea)
ess8$rage<-std01(ess8$age)

## gender
ess8$rsex<-as.numeric(as_factor(ess8$gndr))
ess8$male<-as_factor(Recode(ess8$rsex, "1=1; 2=0; 3=NA"))

## education
ess8$edr1<-as_factor(ess8$eisced)
ess8$edr2<-ifelse(as.numeric(as_factor(ess8$eisced))<9, 
                  as.numeric(as_factor(ess8$eisced))-2, NA)
ess8$edr2r<-ess8$edr2/6

ess8$edr3<-Recode(ess8$edr2, "0=0; 1=0; 2=1; 3=1; 4=2; 5=3; 6=3")
ess8$edr3<-factor(ess8$edr3, labels = c("Lower Secondary or Less", 
                                        "Upper Secondary", "Advanced Vocational",
                                        "College Degree"))

## minority
ess8$rmin<-as.numeric(as_factor(ess8$blgetmg))
ess8$rmin<-as_factor(Recode(ess8$rmin, "1=1; 2=0; 3:5=NA"))

## union member
ess8$union<-ifelse(ess8$mbtru<3, 1, 0)


#####authoritarianism/conservation
#### value recodes.
ess8$rsec1<-(6-ess8$impsafe)
ess8$rsec2<-(6-ess8$ipstrgv)
ess8$rcon1<-(6-ess8$ipfrule)
ess8$rcon2<-(6-ess8$ipbhprp)
ess8$rtrad1<-(6-ess8$ipmodst)
ess8$rtrad2<-(6-ess8$imptrad)
ess8$runi1<-(6-ess8$ipeqopt)
ess8$runi2<-(6-ess8$ipudrst)
ess8$runi3<-(6-ess8$impenv)
ess8$rben1<-(6-ess8$iphlppl)
ess8$rben2<-(6-ess8$iplylfr)
ess8$rselfd1<-(6-ess8$ipcrtiv)
ess8$rselfd2<-(6-ess8$impfree)
ess8$rstim1<-(6-ess8$impdiff)
ess8$rstim2<-(6-ess8$ipadvnt)
ess8$rhed1<-(6-ess8$ipgdtim)
ess8$rhed2<-(6-ess8$impfun)
ess8$rach1<-(6-ess8$ipsuces)
ess8$rach2<-(6-ess8$ipshabt)
ess8$rpow1<-(6-ess8$imprich)
ess8$rpow2<-(6-ess8$iprspot)

## reversed value items.
ess8$rrstim1<-(ess8$impdiff-1)
ess8$rrstim2<-(ess8$ipadvnt-1)
ess8$rrselfd1<-(ess8$ipcrtiv-1)
ess8$rrselfd2<-(ess8$impfree-1)
ess8$rrhed1<-(ess8$ipgdtim-1)
ess8$rrhed2<-(ess8$impfun-1)
ess8$rrach1<-(ess8$ipsuces-1)
ess8$rrach2<-(ess8$ipshabt-1)
ess8$rrpow1<-(ess8$imprich-1)
ess8$rrpow2<-(ess8$iprspot-1)
## security values
ess8$rsec<-std01(rowMeans(with(ess8, cbind(rsec1, rsec2))))

## conformity values
ess8$rcon<-std01(rowMeans(with(ess8, cbind(rcon1, rcon2))))

## tradition values
ess8$rtrad<-std01(rowMeans(with(ess8, cbind(rtrad1, rtrad2))))

## conservation values
psych::alpha(with(ess8, cbind(rsec1, rsec2, rcon1, rcon2, rtrad1, rtrad2)))
ess8$rcv<-std01(rowMeans(with(ess8, cbind(rsec1, rsec2, rcon1, rcon2, rtrad1, rtrad2))))


####################benefits received 0-1
table(ess8$hincsrca)
table(ess8$pubasst1)
ess8$pubasst1<-ifelse(ess8$hincsrca==6|ess8$hincsrca==5,1,0)


#######################economic preferences
ess8$rec1<-ifelse(ess8$gincdif<6, (ess8$gincdif-1)/4, NA)
table(ess8$gincdif)
table(ess8$rec1)
ess8$rec2<-ifelse(ess8$gvslvol<11, (10-ess8$gvslvol)/10, NA)
table(ess8$gvslvol)
table(ess8$rec2)
ess8$rec3<-ifelse(ess8$gvslvue<11, (10-ess8$gvslvue)/10, NA)
table(ess8$gvslvue)
table(ess8$rec3)
ess8$rec4<-ifelse(ess8$gvcldcr<11, (10-ess8$gvcldcr)/10, NA)
table(ess8$gvcldcr)
table(ess8$rec4)
##economic style
psych::alpha(with(ess8, cbind(rec1, rec2, rec3, rec4)))
ess8$recsc1<-rowMeans(with(ess8, cbind(rec1, rec2, rec3, rec4)))

##check correlations for eco pref items
ecopref<-cbind.data.frame(ess8$rec1, ess8$rec2,ess8$rec3,ess8$rec4)
ecocorr<-rcorr(as.matrix(ecopref), type="pearson")
