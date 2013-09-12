# 20.BirthPrep.r
# Birth forms were revised in 2004.  Some variables were added some deleted or renames
# source("20.BirthPrep.r",echo=TRUE)
#
rm(list=ls())

library(knitr)
library(gdata)
library(rms)
library(gdata)
library(RMySQL)
library(Hmisc)
load("../Data/mihow.RData")

#> names(mihow)
# [1] "clientid"                 "momnamelast"             
# [3] "momnamegiven"             "momage"                  
# [5] "dropreason"               "pregnant"                
# [7] "jobstatus"                "language"                
# [9] "mihow_participant"        "primaryadultpovertylevel"
#[11] "momwhite"                 "mommailingzip"           
#[13] "programyear"              "childdob_date"           
#[15] "due_date"                 "momdob_date"             
#[17] "drop_date"                "mommarried"              
#[19] "datayr"                   "momeducationcode" 
#[20] "mid"                   


con<-dbConnect(MySQL(),user="root",password='emma!',dbname='mihow')
sql = "select momnamefirst, momnamelast,datayr, childdob, mommailingzip,  "
sql = paste( sql, " birthwtgrams, gestationweeksestimated ,gestationweeksgenerated",sep=" ")
sql = paste( sql," ,APGAR5minute ,deathind ,NoCongenitalAnomaliesInd ,NoAbnormalconditionsInd",sep=" ")
sql = paste( sql," ,NoComplicationsInd ,hospital_id ,NoMedicalRiskFactorsInd ",sep=" ")
sql = paste( sql," ,Deliveryvaginal,livebirthstotal ,momhispanicorigin ,NoObstetricProcedureInd",sep=" ")
sql = paste( sql," ,ChildIDNum ,momssn ,prenatalcarebeganpregnancy ,previouspretermbirth",sep=" ")
sql = paste( sql," ,antibioticsmom ,ICUAdmission ,NICU ,ChildTransferred",sep=" ")
sql = paste( sql," ,DeathCertNum,paternitySigned,NoInfectionsInd,momwhite,momage,momeducationcode",sep=" ")
sql = paste( sql," ,momdob,mommarried,id,MomNameMaidenLast",sep=" ")
sql = paste( sql, " from birth.birth_all",sep=" ")
sql = paste( sql, " where datayr in (2007,2008,2009,2010) and momrescounty = 'shelby'",sep=' ')
#
# create hospital_id from BirthPlaceCode
#
sql2 = "union select momnamefirst, momnamelast,datayr, childdob, mommailingzip,  "
sql2 = paste( sql2, " birthwtgrams, gestationweeksestimated ,gestationweeksgenerated",sep=" ")
sql2 = paste( sql2," ,APGAR5minute ,deathind ,NoCongenitalAnomaliesInd ,NoAbnormalconditionsInd",sep=" ")
sql2 = paste( sql2," ,NoComplicationsInd ,BirthPlaceCode hospital_id,NoMedicalRiskFactorsInd ",sep=" ")
sql2 = paste( sql2," ,DeliveryVaginal,livebirthstotal ,momhispanicorigin ,NoObstetricProcedureInd",sep=" ")
sql2 = paste( sql2," ,ChildIDNum ,momssn ,prenatalcarebeganpregnancy ,previouspretermbirth",sep=" ")
sql2 = paste( sql2," ,antibioticsmom ,ICUAdmission ,NICU ,ChildTransferred",sep=" ")
sql2 = paste( sql2," ,DeathCertNum,paternitySigned,NoInfectionsInd,momwhite,momage,momeducationcode",sep=" ")
sql2 = paste( sql2," ,momdob,mommarried,id,MomNameMaidenLast",sep=" ")
sql2 = paste( sql2, " from birth_tdh.birth_2010",sep=" ")
sql2 = paste( sql2, " where datayr in (2007,2008,2009,2010) and momrescounty = 'shelby'",sep=' ')

sql <- paste( sql, sql2,sep=" ")
res<-dbSendQuery(con,sql)
birth<-fetch(res,n= -1)
names(birth)<-tolower(names(birth))
birth$momnamelast<-tolower(birth$momnamelast)
birth$momnamefirst<-tolower(birth$momnamefirst)
birth$momnamemaidenlast<-tolower(birth$momnamemaidenlast)
birth$momssn<-ifelse(birth$momssn %in% c('000000000','11111111','222222222','333333333',
'444444444','555555555','666666666','777777777','888888888','999999999') ,NA,birth$momssn)
birth$momssn<-ifelse(birth$momssn > " ",birth$momssn,NA)

gg<-birth[(substr(birth$momssn,1,1)==substr(birth$momssn,2,2)) && (substr(birth$momssn,1,1)==substr(birth$momssn,3,3)),'momssn']
#
# 
# 
# 
# 
#
birth$momnamelast<-sub("-","",birth$momnamelast)
birth$momnamelast<-gsub("'","",birth$momnamelast)
birth$momnamemaidenlast<-sub("-"," ",birth$momnamemaidenlast)
birth$momnamemaidenlast<-gsub("'","",birth$momnamelast)
birth$momnamefirst<-gsub("'","",birth$momnamefirst)
birth$momnamefirst<-gsub("-"," ",birth$momnamefirst)

mihow$momnamelast<-sub("-"," ",mihow$momnamelast)
mihow$momnamelast<-tolower(mihow$momnamelast)
mihow$momnamefirst<-tolower(mihow$momnamefirst)

birth$mommailingzip<-substr(birth$mommailingzip,1,5)
birth$birthwtgrams<-ifelse(birth$birthwtgrams==9999,NA,birth$birthwtgrams)
birth$gestationweeksestimated<-as.numeric(birth$gestationweeksestimated)
birth$gestationweeksestimated<-ifelse(    birth$gestationweeksestimated==99,NA,birth$gestationweeksestimated)

birth$gestationweeksgenerated<-as.numeric(birth$gestationweeksgenerated)
birth$gestationweeksgenerated<-ifelse(birth$gestationweeksgenerated > 50 || birth$gestationweeksgenerated == 0 ,NA,birth$gestationweeksgenerated)

birth$apgar5minute<-as.numeric(birth$apgar5minute)
birth$apgar5minute<-ifelse(    birth$apgar5minute==99,NA,birth$apgar5minute)

birth$hospital_id<-as.numeric(birth$hospital_id)
birth$hospital_id<-ifelse(    birth$hospital_id==0,NA,birth$hospital_id)

birth$livebirthstotal<-as.numeric(birth$livebirthstotal)
birth$livebirthstotal<-ifelse(    birth$livebirthstotal==99,NA,birth$livebirthstotal)

birth$momhispanicorigin<-as.numeric(birth$momhispanicorigin)
birth$momhispanicorigin<-ifelse(    birth$momhispanicorigin==9,NA,birth$momhispanicorigin)

birth$prenatalcarebeganpregnancy<-as.numeric(birth$prenatalcarebeganpregnancy)
birth$prenatalcarebeganpregnancy<-ifelse(    birth$prenatalcarebeganpregnancy==99,NA,birth$prenatalcarebeganpregnancy)
 
birth$uid<-row.names(birth)
birth$childdobyear<-substr(birth$childdob,1,4)
birth$childdobmonth<-subtr(birth$childdob,6,7)
birth$childdobday<-substr(birth$childdob,9,10)
birth$momdobyear<-substr(birth$momdob,1,4)
birth$momdobmonth<-substr(birth$momdob,6,7)
birth$momdobday<-substr(birth$momdob,9,10)

save(birth,file='../Data/birth.RData')
#
#save update birth records in mihow db
#
gg<-dbWriteTable(con,"birth",birth,row.names=FALSE,overwrite=TRUE)
gg<-dbDisconnect(coSn)
rm(gg)
