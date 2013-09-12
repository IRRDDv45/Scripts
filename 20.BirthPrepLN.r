     1  # 20.BirthPrep.r
     2  # Birth forms were revised in 2004.  Some variables were added some deleted or renames
     3  # source("20.BirthPrep.r",echo=TRUE)
     4  #
     5  rm(list=ls())
     6  
     7  library(knitr)
     8  library(gdata)
     9  library(rms)
    10  library(gdata)
    11  library(RMySQL)
    12  library(Hmisc)
    13  load("../Data/mihow.RData")
    14  
    15  #> names(mihow)
    16  # [1] "clientid"                 "momnamelast"             
    17  # [3] "momnamegiven"             "momage"                  
    18  # [5] "dropreason"               "pregnant"                
    19  # [7] "jobstatus"                "language"                
    20  # [9] "mihow_participant"        "primaryadultpovertylevel"
    21  #[11] "momwhite"                 "mommailingzip"           
    22  #[13] "programyear"              "childdob_date"           
    23  #[15] "due_date"                 "momdob_date"             
    24  #[17] "drop_date"                "mommarried"              
    25  #[19] "datayr"                   "momeducationcode" 
    26  #[20] "mid"                   
    27  
    28  
    29  con<-dbConnect(MySQL(),user="root",password='emma!',dbname='mihow')
    30  sql = "select momnamefirst, momnamelast,datayr, childdob, mommailingzip,  "
    31  sql = paste( sql, " birthwtgrams, gestationweeksestimated ,gestationweeksgenerated",sep=" ")
    32  sql = paste( sql," ,APGAR5minute ,deathind ,NoCongenitalAnomaliesInd ,NoAbnormalconditionsInd",sep=" ")
    33  sql = paste( sql," ,NoComplicationsInd ,hospital_id ,NoMedicalRiskFactorsInd ",sep=" ")
    34  sql = paste( sql," ,Deliveryvaginal,livebirthstotal ,momhispanicorigin ,NoObstetricProcedureInd",sep=" ")
    35  sql = paste( sql," ,ChildIDNum ,momssn ,prenatalcarebeganpregnancy ,previouspretermbirth",sep=" ")
    36  sql = paste( sql," ,antibioticsmom ,ICUAdmission ,NICU ,ChildTransferred",sep=" ")
    37  sql = paste( sql," ,DeathCertNum,paternitySigned,NoInfectionsInd,momwhite,momage,momeducationcode",sep=" ")
    38  sql = paste( sql," ,momdob,mommarried,id,MomNameMaidenLast",sep=" ")
    39  sql = paste( sql, " from birth.birth_all",sep=" ")
    40  sql = paste( sql, " where datayr in (2007,2008,2009,2010) and momrescounty = 'shelby'",sep=' ')
    41  #
    42  # create hospital_id from BirthPlaceCode
    43  #
    44  sql2 = "union select momnamefirst, momnamelast,datayr, childdob, mommailingzip,  "
    45  sql2 = paste( sql2, " birthwtgrams, gestationweeksestimated ,gestationweeksgenerated",sep=" ")
    46  sql2 = paste( sql2," ,APGAR5minute ,deathind ,NoCongenitalAnomaliesInd ,NoAbnormalconditionsInd",sep=" ")
    47  sql2 = paste( sql2," ,NoComplicationsInd ,BirthPlaceCode hospital_id,NoMedicalRiskFactorsInd ",sep=" ")
    48  sql2 = paste( sql2," ,DeliveryVaginal,livebirthstotal ,momhispanicorigin ,NoObstetricProcedureInd",sep=" ")
    49  sql2 = paste( sql2," ,ChildIDNum ,momssn ,prenatalcarebeganpregnancy ,previouspretermbirth",sep=" ")
    50  sql2 = paste( sql2," ,antibioticsmom ,ICUAdmission ,NICU ,ChildTransferred",sep=" ")
    51  sql2 = paste( sql2," ,DeathCertNum,paternitySigned,NoInfectionsInd,momwhite,momage,momeducationcode",sep=" ")
    52  sql2 = paste( sql2," ,momdob,mommarried,id,MomNameMaidenLast",sep=" ")
    53  sql2 = paste( sql2, " from birth_tdh.birth_2010",sep=" ")
    54  sql2 = paste( sql2, " where datayr in (2007,2008,2009,2010) and momrescounty = 'shelby'",sep=' ')
    55  
    56  sql <- paste( sql, sql2,sep=" ")
    57  res<-dbSendQuery(con,sql)
    58  birth<-fetch(res,n= -1)
    59  names(birth)<-tolower(names(birth))
    60  birth$momnamelast<-tolower(birth$momnamelast)
    61  birth$momnamefirst<-tolower(birth$momnamefirst)
    62  birth$momnamemaidenlast<-tolower(birth$momnamemaidenlast)
    63  birth$momssn<-ifelse(birth$momssn %in% c('000000000','11111111','222222222','333333333',
    64  '444444444','555555555','666666666','777777777','888888888','999999999') ,NA,birth$momssn)
    65  birth$momssn<-ifelse(birth$momssn > " ",birth$momssn,NA)
    66  
    67  gg<-birth[(substr(birth$momssn,1,1)==substr(birth$momssn,2,2)) && (substr(birth$momssn,1,1)==substr(birth$momssn,3,3)),'momssn']
    68  #
    69  # 
    70  # 
    71  # 
    72  # 
    73  #
    74  birth$momnamelast<-sub("-","",birth$momnamelast)
    75  birth$momnamelast<-gsub("'","",birth$momnamelast)
    76  birth$momnamemaidenlast<-sub("-"," ",birth$momnamemaidenlast)
    77  birth$momnamemaidenlast<-gsub("'","",birth$momnamelast)
    78  birth$momnamefirst<-gsub("'","",birth$momnamefirst)
    79  birth$momnamefirst<-gsub("-"," ",birth$momnamefirst)
    80  
    81  mihow$momnamelast<-sub("-"," ",mihow$momnamelast)
    82  mihow$momnamelast<-tolower(mihow$momnamelast)
    83  mihow$momnamefirst<-tolower(mihow$momnamefirst)
    84  
    85  birth$mommailingzip<-substr(birth$mommailingzip,1,5)
    86  birth$birthwtgrams<-ifelse(birth$birthwtgrams==9999,NA,birth$birthwtgrams)
    87  birth$gestationweeksestimated<-as.numeric(birth$gestationweeksestimated)
    88  birth$gestationweeksestimated<-ifelse(    birth$gestationweeksestimated==99,NA,birth$gestationweeksestimated)
    89  
    90  birth$gestationweeksgenerated<-as.numeric(birth$gestationweeksgenerated)
    91  birth$gestationweeksgenerated<-ifelse(birth$gestationweeksgenerated > 50 || birth$gestationweeksgenerated == 0 ,NA,birth$gestationweeksgenerated)
    92  
    93  birth$apgar5minute<-as.numeric(birth$apgar5minute)
    94  birth$apgar5minute<-ifelse(    birth$apgar5minute==99,NA,birth$apgar5minute)
    95  
    96  birth$hospital_id<-as.numeric(birth$hospital_id)
    97  birth$hospital_id<-ifelse(    birth$hospital_id==0,NA,birth$hospital_id)
    98  
    99  birth$livebirthstotal<-as.numeric(birth$livebirthstotal)
   100  birth$livebirthstotal<-ifelse(    birth$livebirthstotal==99,NA,birth$livebirthstotal)
   101  
   102  birth$momhispanicorigin<-as.numeric(birth$momhispanicorigin)
   103  birth$momhispanicorigin<-ifelse(    birth$momhispanicorigin==9,NA,birth$momhispanicorigin)
   104  
   105  birth$prenatalcarebeganpregnancy<-as.numeric(birth$prenatalcarebeganpregnancy)
   106  birth$prenatalcarebeganpregnancy<-ifelse(    birth$prenatalcarebeganpregnancy==99,NA,birth$prenatalcarebeganpregnancy)
   107   
   108  birth$uid<-row.names(birth)
   109  birth$childdobyear<-substr(birth$childdob,1,4)
   110  birth$childdobmonth<-subtr(birth$childdob,6,7)
   111  birth$childdobday<-substr(birth$childdob,9,10)
   112  birth$momdobyear<-substr(birth$momdob,1,4)
   113  birth$momdobmonth<-substr(birth$momdob,6,7)
   114  birth$momdobday<-substr(birth$momdob,9,10)
   115  
   116  save(birth,file='../Data/birth.RData')
   117  #
   118  #save update birth records in mihow db
   119  #
   120  gg<-dbWriteTable(con,"birth",birth,row.names=FALSE,overwrite=TRUE)
   121  gg<-dbDisconnect(coSn)
   122  rm(gg)
