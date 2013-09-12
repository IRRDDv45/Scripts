     1  # 45.MakeKeys
     2  #
     3  # source("45.MakeKeys.r",echo=TRUE)
     4  #
     5  rm(list=ls())
     6  
     7  #library(knitr)
     8  library(gdata)
     9  library(rms)
    10  library(gdata)
    11  library(gmodels)
    12  library(RMySQL)
    13  library(Hmisc)
    14  
    15  load("../Data/mihow.RData")
    16  load('../Data/birth.RData')
    17  
    18  options('digits'=10)
    19  options('scipen'=10)
    20  options('stringAsFactors'=FALSE)
    21  #
    22  # create keys
    23  #
    24  mihow$momdob<-paste(substr(mihow$momdob,7,10),'-',substr(mihow$momdob,1,5),sep='')
    25  
    26  mihow$key9  <-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),
    27  mihow$momdobyear, mihow$momdobmonth, mihow$childdobyear, mihow$childdobmonth,
    28  mihow$mommailingzip,mihow$momage,mihow$mommarried,mihow$momwhite,sep=":")
    29  
    30  birth$key9 <-paste(trim(birth$momnamelast),trim(birth$momnamefirst),
    31  birth$momdobyear, birth$momdobmonth, birth$childdobyear, birth$childdobmonth,
    32  birth$mommailingzip,birth$momage,birth$mommarried,birth$momwhite,sep=":")
    33  
    34  mihow$key8<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),
    35  mihow$momdobyear, mihow$momdobmonth, mihow$childdobyear, mihow$childdobmonth,
    36  mihow$mommailingzip,mihow$momage,mihow$mommarried,sep=":")
    37  
    38  birth$key8<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),
    39  birth$momdobyear, birth$momdobmonth, birth$childdobyear, birth$childdobmonth,
    40  birth$mommailingzip,birth$momage,birth$mommarried,sep=":")
    41  
    42  mihow$key7<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),
    43  mihow$momdobyear, mihow$momdobmonth, mihow$childdobyear, mihow$childdobmonth,
    44  mihow$mommailingzip,mihow$momage,sep=":")
    45  
    46  birth$key7<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),
    47  birth$momdobyear, birth$momdobmonth, birth$childdobyear, birth$childdobmonth,
    48  birth$mommailingzip,birth$momage,sep=":")
    49  
    50  mihow$key6<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),
    51  mihow$momdobyear, mihow$momdobmonth, mihow$childdobyear, mihow$childdobmonth,
    52  mihow$mommailingzip,sep=":")
    53  
    54  birth$key6<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),
    55  birth$momdobyear, birth$momdobmonth, birth$childdobyear, birth$childdobmonth,
    56  birth$mommailingzip,sep=":")
    57  
    58  mihow$key5<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),
    59  mihow$momdobyear, mihow$momdobmonth, mihow$childdobyear, mihow$childdobmonth,
    60  mihow$mommailingzip,sep=":")
    61  
    62  birth$key5<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),
    63  birth$momdobyear, birth$momdobmonth, birth$childdobyear, birth$childdobmonth,
    64  birth$mommailingzip,sep=":")
    65  
    66  mihow$key4<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),
    67  mihow$momdobyear, mihow$momdobmonth, mihow$childdobyear, mihow$childdobmonth,
    68  sep=":")
    69  
    70  birth$key4<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),
    71  birth$momdobyear, birth$momdobmonth, birth$childdobyear, birth$childdobmonth,
    72  sep=":")
    73  
    74  mihow$key3<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),
    75  mihow$momdobyear, mihow$momdobmonth, 
    76  sep=":")
    77  birth$key3<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),
    78  birth$momdobyear, birth$momdobmonth, 
    79  sep=":")
    80  
    81  mihow$key2<-paste(trim(mihow$momnamelast),trim(mihow$momnamefirst),sep=":")
    82  birth$key2<-paste(trim(birth$momnamelast),trim(birth$momnamefirst),sep=":")
    83  mihow$key1<-paste(trim(mihow$momnamelast),sep=":")
    84  birth$key1<-paste(trim(birth$momnamelast),sep=":")
    85  #
    86  save(mihow,file="../Data/mihowK.RData")   #with keys
    87  save(birth,file="../Data/birthK.RData")
