# 15.MIHOWPrep.r  
# Process tab delimited MIHOW source data for 2007, 2008, 2009
# OUTPUT Data/Mihow.RData
#
# cut and paste the line below, without the '#', into R to run this script
#
# source("15.MIHOWPrep.r",echo=TRUE)
#
rm(list=ls())

library(Hmisc)
library(knitr)
library(gdata)
library(stringr)

load('../Data/ds.1.RData')
#
# get program year from clientid
#
ds2007.1$programyear<-substr(ds2007.1[[1]],nchar(ds2007.1[[1]])-1,nchar(ds2007.1[[1]]))
ds2007.1$programyear<-ifelse(ds2007.1$programyear =='7a', '7',ds2007.1$programyear)
ds2007.1$programyear<-as.integer(ds2007.1$programyear) + 2000
ds2007.1$mid<-ifelse(!is.na(ds2007.1[[1]]) & nchar(ds2007.1[[1]])==7,
      as.numeric(substr(ds2007.1[[1]],1,nchar(ds2007.1[[1]])-4))+1000,
      as.numeric(substr(ds2007.1[[1]],1,3)))+20070000

ds2008.1$programyear<-substr(ds2008.1[[1]],nchar(ds2008.1[[1]])-1,nchar(ds2008.1[[1]]))
ds2008.1$programyear<-ifelse(ds2008.1$programyear =='7a', '7',ds2008.1$programyear)
ds2008.1$programyear<-as.integer(ds2008.1$programyear) + 2000
ds2008.1$mid<-ifelse(nchar(ds2008.1[[1]])==7,
      as.numeric(substr(ds2008.1[[1]],1,nchar(ds2008.1[[1]])-4))+1000,
      as.numeric(substr(ds2008.1[[1]],1,3)))+20080000

ds2009.1$programyear<-substr(ds2009.1[[1]],nchar(ds2009.1[[1]])-1,nchar(ds2009.1[[1]]))
ds2009.1$programyear<-ifelse(ds2009.1$programyear =='7a', '7',ds2009.1$programyear)
ds2009.1$programyear<-as.integer(ds2009.1$programyear) + 2000
ds2009.1$mid<-ifelse(nchar(ds2009.1[[1]])==7,
      as.numeric(substr(ds2009.1[[1]],1,nchar(ds2009.1[[1]])-4))+1000,
      as.numeric(substr(ds2009.1[[1]],1,3)))+20090000
#
# mommarried Y N U
ds2007.1$mommarried<-ifelse(ds2007.1$married == 'Married', "Y","N")#
ds2008.1$married<-ifelse(ds2008.1$married == 'married', "Married",ds2008.1$married)
ds2008.1$mommarried<-ifelse(ds2008.1$married == 'Married', "Y","N")

ds2009.1$mommarried<-ifelse(ds2009.1$married == 'Married', "Y","N")

ds2007.1<-remove.vars(ds2007.1,c("education","primaryadultedulevel","duedateofpregnancy","married","primaryadultjobstatus"))
ds2008.1<-remove.vars(ds2008.1,c("education","duedateofpregnancy","married"))
ds2009.1<-remove.vars(ds2009.1,c("education","duedateofpregnancy","married", "primarylanguage"))
#
# verify that all datasets now have the same names
#
sort(names(ds2007.1))
sort(names(ds2008.1))
sort(names(ds2009.1))

gg<-cbind(sort(names(ds2007.1)),sort(names(ds2008.1)),sort(names(ds2009.1)))
options(width=128)
gg
rm(gg)

contents(ds2007.1)
contents(ds2008.1)
contents(ds2009.1)

describe(ds2007.1,descript="ds2007")
describe(ds2008.1,descript="ds2008")
describe(ds2009.1,descript="ds2009")
#
# select clients who are pregnant and rename datasets
mihow2007<-subset(ds2007.1, ds2007.1$pregnant == TRUE & ds2007.1$programyear == 2007 & ds2007.1$mihow_participant == TRUE,
   select=c(clientid,momnamefirst,momnamelast,momage,childdob,dropreason,pregnant,language,mihow_participant,
            momwhite,mommailingzip,programyear,mid,duedate,dropdate,mommarried, momdob))

mihow2007$momdob<-ifelse(mihow2007$momdob == "01-01-1900",NA,mihow2007$momdob)
mihow2007$momnamemaiden<-mihow2007$momnamelast
mihow2007$momnamelast<-tolower(mihow2007$momnamelast)
mihow2007$momnamefirst<-tolower(mihow2007$momnamefirst)
mihow2007$momnamelast<-sub("-","",mihow2007$momnamelast)
mihow2007$momnamelast<-sub("'","",mihow2007$momnamelast)
momwhite<- 9
momwhite<-ifelse(mihow2007$momwhite %in% c("Black, African-American","Black, African-sAmerican"),2,momwhite)
momwhite<-ifelse(mihow2007$momwhite %in% c("White, Caucasian","Mexican, Hispanic, Latino, Spanish"),1,momwhite)
momwhite<-ifelse(momwhite == 9, NA,momwhite)
mihow2007$momwhite<-momwhite
rm(momwhite)

mihow2008<-subset(ds2008.1, ds2008.1$pregnant == TRUE & ds2008.1$programyear == 2008 & ds2008.1$mihow_participant == TRUE,select=c(clientid,momnamefirst,momnamelast,momage,childdob,dropreason,pregnant,language,mihow_participant,momwhite,mommailingzip,programyear,mid,duedate,dropdate,mommarried,momdob))
mihow2008$momdob<-ifelse(mihow2008$momdob == "01-01-1900",NA,mihow2008$momdob)
mihow2008$momnamemaiden<-mihow2008$momnamelast
mihow2008$momnamelast<-tolower(mihow2008$momnamelast)
mihow2008$momnamefirst<-tolower(mihow2008$momnamefirst)
mihow2008$momnamelast<-sub("-","",mihow2008$momnamelast)
mihow2008$momnamelast<-sub("'","",mihow2008$momnamelast)
momwhite<- 9
momwhite<-ifelse(mihow2008$momwhite %in% c("Black, African-American","Black, African-sAmerican"),2,momwhite)
momwhite<-ifelse(mihow2008$momwhite %in% c("White, Caucasian","Mexican, Hispanic, Latino, Spanish"),1,momwhite)
momwhite<-ifelse(momwhite == 9, NA,momwhite)
mihow2008$momwhite<-momwhite
rm(momwhite)

mihow2009<-subset(ds2009.1, ds2009.1$pregnant == TRUE & ds2009.1$programyear == 2009 & ds2009.1$mihow_participant == TRUE,select=c(clientid,momnamefirst,momnamelast,momage,childdob,dropreason,pregnant,language,mihow_participant,momwhite,mommailingzip,programyear,mid,duedate,dropdate,mommarried,momdob))
mihow2009$momdob<-ifelse(mihow2009$momdob == "01-01-1900",NA,mihow2009$momdob)
mihow2009$momnamemaiden<-mihow2009$momnamelast
mihow2009$momnamelast<-tolower(mihow2009$momnamelast)
mihow2009$momnamefirst<-tolower(mihow2009$momnamefirst)
mihow2009$momnamelast<-sub("-", "",mihow2009$momnamelast)
mihow2009$momnamelast<-sub("'", "",mihow2009$momnamelast)
momwhite<- 9
momwhite<-ifelse(mihow2009$momwhite %in% c("Black, African-American","Black, African-sAmerican"),2,momwhite)
momwhite<-ifelse(mihow2009$momwhite %in% c("White, Caucasian","Mexican, Hispanic, Latino, Spanish"),1,momwhite)
momwhite<-ifelse(momwhite == 9, NA,momwhite)
mihow2009$momwhite<-momwhite
rm(momwhite)
reliable<-function(var,var_name){
pieces<- strsplit(var, ",")
var12<-var[sapply(pieces,"[",1) != sapply(pieces,"[",2)]
var13<-var[sapply(pieces,"[",1) != sapply(pieces,"[",3)]
var14<-var[sapply(pieces,"[",1) != sapply(pieces,"[",4)]
zz<-c(var12[!is.na(var12)],var13[!is.na(var13)],var14[!is.na(var14)])
zz
}
#
# save datasets in one R data
mihow<-rbind(mihow2009,mihow2008,mihow2007)  #stack programyears into one dataset
# create a unique identifier (uid) for each row in the combined mihow dataset
mihow$uid<-row.names(mihow)
pos<-str_locate(mihow$momnamelast,'\n')

gg<-ifelse(grep('\n',mihow$momnamelast),substr(mihow$momnamelast,1,str_locate(mihow$momnamelast,'\n')[,1]-1),mihow$momnamelast)

mihow$est_childdob <- mihow$childdob
mihow$est_childdob[is.na(mihow$childdob)] <- mihow$duedate[is.na(mihow$childdob)]

getYear <-function(date){substr(date,1,4)}
getMonth<-function(date){substr(date,6,7)}
getDay  <-function(date){substr(date,9,10)}

mihow$childdobyear<-getYear(mihow$childdob)
mihow$childdobmonth<-getMonth(mihow$childdob)
mihow$childdobday<-getDay(mihow$childdob)

mihow$duedateyear<-getYear(mihow$duedate)
mihow$duedatemonth<-getMonth(mihow$duedate)
mihow$duedateday<-getDay(mihow$duedate)

mihow$dropdateyear<-getYear(mihow$dropdate)
mihow$dropdatemonth<-getMonth(mihow$dropdate)
mihow$dropdateday<-getDay(mihow$dropdate)

mihow$momdobyear<-substr(mihow$momdob,7,10)
mihow$momdobmonth<-substr(mihow$momdob,1,2)
mihow$momdobday<-substr(mihow$momdob,4,5)

mihow$est_childdobyear<-getYear(mihow$est_childdob)
mihow$est_childdobmonth<-getMonth(mihow$est_childdob)
mihow$est_childdobday<-getDay(mihow$est_childdob)

save(mihow,mihow2007,mihow2008,mihow2009,file='../Data/mihow.RData')
