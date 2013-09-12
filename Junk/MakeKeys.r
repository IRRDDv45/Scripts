# 40.MakeKeys
#
# source("4.MakeKeys",echo=TRUE)
#
library(knitr)
library(gdata)
library(rms)
library(gdata)
library(gmodels)
library(RMySQL)
library(Hmisc)

load("../Data/mihow.RData")
load('../Data/birth.RData')
mihow$momnamemaiden<-tolower(mihow$momnamemaiden)
options('digits'=10)
options('scipen'=10)
options('stringAsFactors'=FALSE)
#
# create keys
#

mihow$key9 <-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),mihow$momdob,mihow$childdob,mihow$mommailingzip,mihow$momeducationcode,mihow$momage,mihow$mommarried,mihow$momwhite,sep=":")
mihow$key9a<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),mihow$momdob,mihow$est_childdob,mihow$mommailingzip,mihow$momeducationcode,mihow$momage,mihow$mommarried,mihow$momwhite,sep=":")

birth$key9 <-paste(trim(birth$momnamelast),trim(birth$momnamefirst),birth$momdob,birth$childdob,birth$mommailingzip,birth$momeducationcode,birth$momage,birth$mommarried,birth$momwhite,sep=":")
birth$key9a<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),birth$momdob,birth$childdob,birth$mommailingzip,birth$momeducationcode,birth$momage,birth$mommarried,birth$momwhite,sep=":")

mihow$key8<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),mihow$momdob,mihow$childdob,mihow$mommailingzip,mihow$momeducationcode,mihow$momage,mihow$mommarried,sep=":")
mihow$key8a<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),mihow$momdob,mihow$estchilddob,mihow$mommailingzip,mihow$momeducationcode,mihow$momage,mihow$mommarried,sep=":")
birth$key8<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),birth$momdob,birth$childdob,birth$mommailingzip,birth$momeducationcode,birth$momage,birth$mommarried,sep=":")
birth$key8a<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),birth$momdob,birth$childdob,birth$mommailingzip,birth$momeducationcode,birth$momage,birth$mommarried,sep=":")
mihow$key7<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),mihow$momdob,mihow$childdob,mihow$mommailingzip,mihow$momeducationcode,mihow$momage,sep=":")
mihow$key7a<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),mihow$momdob,mihow$est_childdob,mihow$mommailingzip,mihow$momeducationcode,mihow$momage,sep=":")
birth$key7<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),birth$momdob,birth$childdob,birth$mommailingzip,birth$momeducationcode,birth$momage,sep=":")
birth$key7a<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),birth$momdob,birth$childdob,birth$mommailingzip,birth$momeducationcode,birth$momage,sep=":")
mihow$key6<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),mihow$momdob,mihow$childdob,mihow$mommailingzip,mihow$momeducationcode,sep=":")
mihow$key6a<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),mihow$momdob,mihow$est_childdob,mihow$mommailingzip,mihow$momeducationcode,sep=":")
birth$key6<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),birth$momdob,birth$childdob,birth$mommailingzip,birth$momeducationcode,sep=":")
birth$key6a<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),birth$momdob,birth$childdob,birth$mommailingzip,birth$momeducationcode,sep=":")
mihow$key5<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),mihow$momdob,mihow$childdob,mihow$mommailingzip,sep=":")
mihow$key5a<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),mihow$momdob,mihow$est_childdob,mihow$mommailingzip,sep=":")
birth$key5<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),birth$momdob,birth$childdob,birth$mommailingzip,sep=":")
birth$key5a<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),birth$momdob,birth$childdob,birth$mommailingzip,sep=":")
mihow$key4<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),mihow$momdob,mihow$childdob,sep=":")
mihow$key4a<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),mihow$momdob,mihow$est_childdob,sep=":")
birth$key4<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),birth$momdob,birth$childdob,sep=":")
birth$key4a<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),birth$momdob,birth$childdob,sep=":")
mihow$key3<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),mihow$momdob,sep=":")
birth$key3<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),birth$momdob,sep=":")
mihow$key2<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),sep=":")
birth$key2<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),sep=":")
mihow$key1<-paste(trim(mihow$momnamelast),sep=":")
birth$key1<-paste(trim(birth$momnamelast),sep=":")
#
save(mihow,file="../Data/mihow.RData")
save(birth,file="../Data/birth.RData")
