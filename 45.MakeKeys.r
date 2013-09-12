# 45.MakeKeys
#
# source("45.MakeKeys.r",echo=TRUE)
#
rm(list=ls())

#library(knitr)
library(gdata)
library(rms)
library(gdata)
library(gmodels)
library(RMySQL)
library(Hmisc)

load("../Data/mihow.RData")
load('../Data/birth.RData')

options('digits'=10)
options('scipen'=10)
options('stringAsFactors'=FALSE)
#
# create keys
#
mihow$momdob<-paste(substr(mihow$momdob,7,10),'-',substr(mihow$momdob,1,5),sep='')

mihow$key9  <-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),
mihow$momdobyear, mihow$momdobmonth, mihow$childdobyear, mihow$childdobmonth,
mihow$mommailingzip,mihow$momage,mihow$mommarried,mihow$momwhite,sep=":")

birth$key9 <-paste(trim(birth$momnamelast),trim(birth$momnamefirst),
birth$momdobyear, birth$momdobmonth, birth$childdobyear, birth$childdobmonth,
birth$mommailingzip,birth$momage,birth$mommarried,birth$momwhite,sep=":")

mihow$key8<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),
mihow$momdobyear, mihow$momdobmonth, mihow$childdobyear, mihow$childdobmonth,
mihow$mommailingzip,mihow$momage,mihow$mommarried,sep=":")

birth$key8<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),
birth$momdobyear, birth$momdobmonth, birth$childdobyear, birth$childdobmonth,
birth$mommailingzip,birth$momage,birth$mommarried,sep=":")

mihow$key7<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),
mihow$momdobyear, mihow$momdobmonth, mihow$childdobyear, mihow$childdobmonth,
mihow$mommailingzip,mihow$momage,sep=":")

birth$key7<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),
birth$momdobyear, birth$momdobmonth, birth$childdobyear, birth$childdobmonth,
birth$mommailingzip,birth$momage,sep=":")

mihow$key6<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),
mihow$momdobyear, mihow$momdobmonth, mihow$childdobyear, mihow$childdobmonth,
mihow$mommailingzip,sep=":")

birth$key6<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),
birth$momdobyear, birth$momdobmonth, birth$childdobyear, birth$childdobmonth,
birth$mommailingzip,sep=":")

mihow$key5<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),
mihow$momdobyear, mihow$momdobmonth, mihow$childdobyear, mihow$childdobmonth,
mihow$mommailingzip,sep=":")

birth$key5<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),
birth$momdobyear, birth$momdobmonth, birth$childdobyear, birth$childdobmonth,
birth$mommailingzip,sep=":")

mihow$key4<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),
mihow$momdobyear, mihow$momdobmonth, mihow$childdobyear, mihow$childdobmonth,
sep=":")

birth$key4<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),
birth$momdobyear, birth$momdobmonth, birth$childdobyear, birth$childdobmonth,
sep=":")

mihow$key3<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),
mihow$momdobyear, mihow$momdobmonth, 
sep=":")
birth$key3<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),
birth$momdobyear, birth$momdobmonth, 
sep=":")

mihow$key2<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),sep=":")
birth$key2<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),sep=":")
mihow$key1<-paste(trim(mihow$momnamelast),sep=":")
birth$key1<-paste(trim(birth$momnamelast),sep=":")
#
save(mihow,file="../Data/mihowK.RData")   #with keys
save(birth,file="../Data/birthK.RData")
