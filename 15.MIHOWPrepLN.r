     1  # 15.MIHOWPrep.r       2  # Process tab delimited MIHOW source data for 2007, 2008, 2009     3  # OUTPUT Data/Mihow.RData     4  #     5  # cut and paste the line below, without the '#', into R to run this script     6  #     7  # source("15.MIHOWPrep.r",echo=TRUE)     8  #     9  rm(list=ls())    10      11  library(Hmisc)    12  library(knitr)    13  library(gdata)    14  library(stringr)    15      16  load('../Data/ds.1.RData')    17  #    18  # get program year from clientid    19  #    20  ds2007.1$programyear<-substr(ds2007.1[[1]],nchar(ds2007.1[[1]])-1,nchar(ds2007.1[[1]]))    21  ds2007.1$programyear<-ifelse(ds2007.1$programyear =='7a', '7',ds2007.1$programyear)    22  ds2007.1$programyear<-as.integer(ds2007.1$programyear) + 2000    23  ds2007.1$mid<-ifelse(!is.na(ds2007.1[[1]]) & nchar(ds2007.1[[1]])==7,    24        as.numeric(substr(ds2007.1[[1]],1,nchar(ds2007.1[[1]])-4))+1000,    25        as.numeric(substr(ds2007.1[[1]],1,3)))+20070000    26      27  ds2008.1$programyear<-substr(ds2008.1[[1]],nchar(ds2008.1[[1]])-1,nchar(ds2008.1[[1]]))    28  ds2008.1$programyear<-ifelse(ds2008.1$programyear =='7a', '7',ds2008.1$programyear)    29  ds2008.1$programyear<-as.integer(ds2008.1$programyear) + 2000    30  ds2008.1$mid<-ifelse(nchar(ds2008.1[[1]])==7,    31        as.numeric(substr(ds2008.1[[1]],1,nchar(ds2008.1[[1]])-4))+1000,    32        as.numeric(substr(ds2008.1[[1]],1,3)))+20080000    33      34  ds2009.1$programyear<-substr(ds2009.1[[1]],nchar(ds2009.1[[1]])-1,nchar(ds2009.1[[1]]))    35  ds2009.1$programyear<-ifelse(ds2009.1$programyear =='7a', '7',ds2009.1$programyear)    36  ds2009.1$programyear<-as.integer(ds2009.1$programyear) + 2000    37  ds2009.1$mid<-ifelse(nchar(ds2009.1[[1]])==7,    38        as.numeric(substr(ds2009.1[[1]],1,nchar(ds2009.1[[1]])-4))+1000,    39        as.numeric(substr(ds2009.1[[1]],1,3)))+20090000    40  #    41  # mommarried Y N U    42  ds2007.1$mommarried<-ifelse(ds2007.1$married == 'Married', "Y","N")#    43  ds2008.1$married<-ifelse(ds2008.1$married == 'married', "Married",ds2008.1$married)    44  ds2008.1$mommarried<-ifelse(ds2008.1$married == 'Married', "Y","N")    45      46  ds2009.1$mommarried<-ifelse(ds2009.1$married == 'Married', "Y","N")    47      48  ds2007.1<-remove.vars(ds2007.1,c("education","primaryadultedulevel","duedateofpregnancy","married","primaryadultjobstatus"))    49  ds2008.1<-remove.vars(ds2008.1,c("education","duedateofpregnancy","married"))    50  ds2009.1<-remove.vars(ds2009.1,c("education","duedateofpregnancy","married", "primarylanguage"))    51  #    52  # verify that all datasets now have the same names    53  #    54  sort(names(ds2007.1))    55  sort(names(ds2008.1))    56  sort(names(ds2009.1))    57      58  gg<-cbind(sort(names(ds2007.1)),sort(names(ds2008.1)),sort(names(ds2009.1)))    59  options(width=128)    60  gg    61  rm(gg)    62      63  contents(ds2007.1)    64  contents(ds2008.1)    65  contents(ds2009.1)    66      67  describe(ds2007.1,descript="ds2007")    68  describe(ds2008.1,descript="ds2008")    69  describe(ds2009.1,descript="ds2009")    70  #    71  # select clients who are pregnant and rename datasets    72  mihow2007<-subset(ds2007.1, ds2007.1$pregnant == TRUE & ds2007.1$programyear == 2007 & ds2007.1$mihow_participant == TRUE,    73     select=c(clientid,momnamefirst,momnamelast,momage,childdob,dropreason,pregnant,language,mihow_participant,    74              momwhite,mommailingzip,programyear,mid,duedate,dropdate,mommarried, momdob))    75      76  mihow2007$momdob<-ifelse(mihow2007$momdob == "01-01-1900",NA,mihow2007$momdob)    77  mihow2007$momnamemaiden<-mihow2007$momnamelast    78  mihow2007$momnamelast<-tolower(mihow2007$momnamelast)    79  mihow2007$momnamefirst<-tolower(mihow2007$momnamefirst)    80  mihow2007$momnamelast<-sub("-","",mihow2007$momnamelast)    81  mihow2007$momnamelast<-sub("'","",mihow2007$momnamelast)    82  momwhite<- 9    83  momwhite<-ifelse(mihow2007$momwhite %in% c("Black, African-American","Black, African-sAmerican"),2,momwhite)    84  momwhite<-ifelse(mihow2007$momwhite %in% c("White, Caucasian","Mexican, Hispanic, Latino, Spanish"),1,momwhite)    85  momwhite<-ifelse(momwhite == 9, NA,momwhite)    86  mihow2007$momwhite<-momwhite    87  rm(momwhite)    88      89  mihow2008<-subset(ds2008.1, ds2008.1$pregnant == TRUE & ds2008.1$programyear == 2008 & ds2008.1$mihow_participant == TRUE,select=c(clientid,momnamefirst,momnamelast,momage,childdob,dropreason,pregnant,language,mihow_participant,momwhite,mommailingzip,programyear,mid,duedate,dropdate,mommarried,momdob))    90  mihow2008$momdob<-ifelse(mihow2008$momdob == "01-01-1900",NA,mihow2008$momdob)    91  mihow2008$momnamemaiden<-mihow2008$momnamelast    92  mihow2008$momnamelast<-tolower(mihow2008$momnamelast)    93  mihow2008$momnamefirst<-tolower(mihow2008$momnamefirst)    94  mihow2008$momnamelast<-sub("-","",mihow2008$momnamelast)    95  mihow2008$momnamelast<-sub("'","",mihow2008$momnamelast)    96  momwhite<- 9    97  momwhite<-ifelse(mihow2008$momwhite %in% c("Black, African-American","Black, African-sAmerican"),2,momwhite)    98  momwhite<-ifelse(mihow2008$momwhite %in% c("White, Caucasian","Mexican, Hispanic, Latino, Spanish"),1,momwhite)    99  momwhite<-ifelse(momwhite == 9, NA,momwhite)   100  mihow2008$momwhite<-momwhite   101  rm(momwhite)   102     103  mihow2009<-subset(ds2009.1, ds2009.1$pregnant == TRUE & ds2009.1$programyear == 2009 & ds2009.1$mihow_participant == TRUE,select=c(clientid,momnamefirst,momnamelast,momage,childdob,dropreason,pregnant,language,mihow_participant,momwhite,mommailingzip,programyear,mid,duedate,dropdate,mommarried,momdob))   104  mihow2009$momdob<-ifelse(mihow2009$momdob == "01-01-1900",NA,mihow2009$momdob)   105  mihow2009$momnamemaiden<-mihow2009$momnamelast   106  mihow2009$momnamelast<-tolower(mihow2009$momnamelast)   107  mihow2009$momnamefirst<-tolower(mihow2009$momnamefirst)   108  mihow2009$momnamelast<-sub("-", "",mihow2009$momnamelast)   109  mihow2009$momnamelast<-sub("'", "",mihow2009$momnamelast)   110  momwhite<- 9   111  momwhite<-ifelse(mihow2009$momwhite %in% c("Black, African-American","Black, African-sAmerican"),2,momwhite)   112  momwhite<-ifelse(mihow2009$momwhite %in% c("White, Caucasian","Mexican, Hispanic, Latino, Spanish"),1,momwhite)   113  momwhite<-ifelse(momwhite == 9, NA,momwhite)   114  mihow2009$momwhite<-momwhite   115  rm(momwhite)   116  reliable<-function(var,var_name){   117  pieces<- strsplit(var, ",")   118  var12<-var[sapply(pieces,"[",1) != sapply(pieces,"[",2)]   119  var13<-var[sapply(pieces,"[",1) != sapply(pieces,"[",3)]   120  var14<-var[sapply(pieces,"[",1) != sapply(pieces,"[",4)]   121  zz<-c(var12[!is.na(var12)],var13[!is.na(var13)],var14[!is.na(var14)])   122  zz   123  }   124  #   125  # save datasets in one R data   126  mihow<-rbind(mihow2009,mihow2008,mihow2007)  #stack programyears into one dataset   127  # create a unique identifier (uid) for each row in the combined mihow dataset   128  mihow$uid<-row.names(mihow)   129  pos<-str_locate(mihow$momnamelast,'\n')   130     131  gg<-ifelse(grep('\n',mihow$momnamelast),substr(mihow$momnamelast,1,str_locate(mihow$momnamelast,'\n')[,1]-1),mihow$momnamelast)   132     133  mihow$est_childdob <- mihow$childdob   134  mihow$est_childdob[is.na(mihow$childdob)] <- mihow$duedate[is.na(mihow$childdob)]   135     136  getYear <-function(date){substr(date,1,4)}   137  getMonth<-function(date){substr(date,6,7)}   138  getDay  <-function(date){substr(date,9,10)}   139     140  mihow$childdobyear<-getYear(mihow$childdob)   141  mihow$childdobmonth<-getMonth(mihow$childdob)   142  mihow$childdobday<-getDay(mihow$childdob)   143     144  mihow$duedateyear<-getYear(mihow$duedate)   145  mihow$duedatemonth<-getMonth(mihow$duedate)   146  mihow$duedateday<-getDay(mihow$duedate)   147     148  mihow$dropdateyear<-getYear(mihow$dropdate)   149  mihow$dropdatemonth<-getMonth(mihow$dropdate)   150  mihow$dropdateday<-getDay(mihow$dropdate)   151     152  mihow$momdobyear<-substr(mihow$momdob,7,10)   153  mihow$momdobmonth<-substr(mihow$momdob,1,2)   154  mihow$momdobday<-substr(mihow$momdob,4,5)   155     156  mihow$est_childdobyear<-getYear(mihow$est_childdob)   157  mihow$est_childdobmonth<-getMonth(mihow$est_childdob)   158  mihow$est_childdobday<-getDay(mihow$est_childdob)   159     160  save(mihow,mihow2007,mihow2008,mihow2009,file='../Data/mihow.RData')  