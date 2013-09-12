# 10.MIHOWPrep.r  
# Process tab delimited MIHOW source data for 2007, 2008, 2009
# OUTPUT Data/Mihow.RData
#
# cut and paste the line below, without the '#', into R to run this script
#
# source("10.MIHOWPrep.r",echo=TRUE)
#
rm(list=ls())

library(Hmisc)
library(knitr)
library(gdata)
library(stringr)
#
# getinfo --  loads data from tab delimited text file
#             converts variable names to lowercase
#             R is casesensitive
#
getinfo<-function(file){
dataset<-read.delim(file,sep='\t',stringsAsFactors=FALSE)
names(dataset)<-tolower(names(dataset))
dataset
}
# 2007 data
#
ds2007<-getinfo('../Data/2007MIHOWClientDemographics_2.txt')
ds2007$primaryadultage <-ifelse(ds2007$primaryadultage < 0, NA, ds2007$primaryadultage)
ds2007<-rename.vars(ds2007,c("clientid."),c("clientid"))
ds2007$dropreason<-sub('\xd5',"'",ds2007$dropreason)
#
# 2008 data
#

ds2008<-getinfo('../Data/VU2008ClientDemographics1_MIHOW.txt')
ds2008<-rename.vars(ds2008,c("familyid.","primaryadutlzip"),c("clientid","primaryadultzip"))
#
# delete variables
ds2008$x<-NULL
ds2008$x38106<-NULL
ds2008$x62<-NULL
ds2008Preg<-getinfo('../Data/VU2008MIHOW Dbase Pregn_Deliveries.txt')
ds2008Preg<-rename.vars(ds2008Preg,c("clientid."),c("clientid"))
# fix drop reason with '\xd5' in string
#
ds2008Preg$dropreason<-sub('\xd5',"'",ds2008Preg$dropreason)#
#
# 2009 data
ds2009Info<-getinfo('../Data/VU2009MIHOWInfo.txt')
ds2009Info<-rename.vars(ds2009Info,c("familyid.","familyrace","primaryadutlzip"),c("clientid","race","primaryadultzip"))
#
save(ds2007,ds2008,ds2008Preg,ds2009Info,file='../Data/ds.RData')
#
#select and harmonize variable names
#

ds2007.1<-subset(ds2007,select=c(clientid,primaryadultfname,primaryadultlname,primaryadultage,familydropdate,dropreason,
actualdeliverydate,duedateofpregnancy,pregnant,language,primaryadultmaritalstatus,mihow_participant,race,primaryadultzip))
ds2007.1$duedate<-as.character( as.Date(ds2007.1$duedateofpregnancy,format="%m/%d/%y"))
ds2007.1$duedateofpregnancy<-NULL

ds2007.1<-rename.vars(ds2007.1,
c("primaryadultfname","primaryadultlname","primaryadultage","familydropdate","actualdeliverydate","primaryadultmaritalstatus","race","primaryadultzip"),
c("momnamefirst","momnamelast","momage","dropdate","childdob","married","momwhite","mommailingzip"))
ds2007.1$dropreason<-sub('\xd5',"'",ds2007.1$dropreason)
ds2007.1$momdob<-"1900-01-01"
ds2007.1$momdobday<-NA
ds2007.1$momdobmonth<-NA
ds2007.1$momdobyear<-NA
ds2007.1$childdob<-as.character( as.Date(ds2007.1$childdob,format="%m/%d/%y"))

ds2008.2<-subset(ds2008,select=c(clientid,primaryadultfname,primaryadultlname,language,primaryadultmaritalstatus,mihow_participant,familyrace, primaryadultzip))
ds2008.3<-subset(ds2008Preg,select=c(clientid,primaryadultfname,primaryadultlname,actualdeliverydate,duedateofpregnancy, dropdate,dropreason,pregnant))
ds2008.3$duedate<-as.character( as.Date(ds2008.3$duedateofpregnancy,format="%m/%d/%y"))
ds2008.3$duedateofpregnancy<-NULL

ds2008.1<-merge(ds2008.2, ds2008.3, by.x = "clientid", by.y = "clientid", all = TRUE)

ds2008.1<-rename.vars(ds2008.1,
c("primaryadultfname.x","primaryadultlname.x","actualdeliverydate","primaryadultmaritalstatus","familyrace","primaryadultzip"),
c("momnamefirst","momnamelast","childdob","married","momwhite","mommailingzip"))
sort(names(ds2008.1))

ds2008.1<-remove.vars(ds2008.1,c("primaryadultfname.y","primaryadultlname.y","duedateofpregnancy"))
ds2008.1$momage<-NA
ds2008.1$momdob<-"1900-01-01"
ds2008.1$momdobday<-NA
ds2008.1$momdobmonth<-NA
ds2008.1$momdobyear<-NA
ds2008.1$childdob<-as.character( as.Date(ds2008.1$childdob,format="%d-%b-%y"))

ds2009.1<-subset(ds2009Info,select=c(clientid,primaryadultfname,primaryadultlname,primaryadultage,primaryadultbirthmon,primaryadultbirthday_,primaryadultbirthyear_,dropdate,dropreason,
actualdeliverydate,duedateofpregnancy,pregnant,
language,primaryadultmaritalstatus,mihow_participant,race,primaryadultzip))
ds2009.1$primaryadultbirthyear<- sprintf("%4d",as.numeric(ds2009.1$primaryadultbirthyear_))
ds2009.1$primaryadultbirthyear_<-NULL
ds2009.1$primaryadultbirthmon<- sprintf("%2.2d",as.numeric(ds2009.1$primaryadultbirthmon))
ds2009.1$primaryadultbirthday_<- sprintf("%2.2d",as.numeric(ds2009.1$primaryadultbirthday_))
ds2009.1$momdob<-paste(ds2009.1$primaryadultbirthyear,"-",ds2009.1$primaryadultbirthmon,"-",ds2009.1$primaryadultbirthday_,sep="")
ds2009.1$duedate<-as.character( as.Date(ds2009.1$duedateofpregnancy,format="%d-%b-%y"))
ds2009.1$duedateofpregnancy<-NULL
#
# first and last name reversed in ds2009.1
ds2009.1<-rename.vars(ds2009.1,
c("primaryadultfname","primaryadultlname","actualdeliverydate","primaryadultmaritalstatus","race",   "primaryadultzip"),
c("momnamelast",      "momnamefirst",     "childdob",          "married",                 "momwhite","mommailingzip"  ))
ds2009.1<-rename.vars(ds2009.1,
c("primaryadultbirthmon","primaryadultbirthday_","primaryadultbirthyear","primaryadultage"),
c("momdobmonth",         "momdobday",            "momdobyear",            "momage"))
ds2009.1$childdob<-as.character( as.Date(ds2009.1$childdob,format="%d-%b-%y"))

save(ds2007.1,ds2008.1,ds2008.2,ds2008.3,ds2009.1,file='../Data/ds.1.RData')
sort(names(ds2007.1))
sort(names(ds2008.1))
sort(names(ds2009.1))

