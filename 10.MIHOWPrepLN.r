     1  # 10.MIHOWPrep.r  
     2  # Process tab delimited MIHOW source data for 2007, 2008, 2009
     3  # OUTPUT Data/Mihow.RData
     4  #
     5  # cut and paste the line below, without the '#', into R to run this script
     6  #
     7  # source("10.MIHOWPrep.r",echo=TRUE)
     8  #
     9  rm(list=ls())
    10  
    11  library(Hmisc)
    12  library(knitr)
    13  library(gdata)
    14  library(stringr)
    15  #
    16  # getinfo --  loads data from tab delimited text file
    17  #             converts variable names to lowercase
    18  #             R is casesensitive
    19  #
    20  getinfo<-function(file){
    21  dataset<-read.delim(file,sep='\t',stringsAsFactors=FALSE)
    22  names(dataset)<-tolower(names(dataset))
    23  dataset
    24  }
    25  # 2007 data
    26  #
    27  ds2007<-getinfo('../Data/2007MIHOWClientDemographics_2.txt')
    28  ds2007$primaryadultage <-ifelse(ds2007$primaryadultage < 0, NA, ds2007$primaryadultage)
    29  ds2007<-rename.vars(ds2007,c("clientid."),c("clientid"))
    30  ds2007$dropreason<-sub('\xd5',"'",ds2007$dropreason)
    31  #
    32  # 2008 data
    33  #
    34  
    35  ds2008<-getinfo('../Data/VU2008ClientDemographics1_MIHOW.txt')
    36  ds2008<-rename.vars(ds2008,c("familyid.","primaryadutlzip"),c("clientid","primaryadultzip"))
    37  #
    38  # delete variables
    39  ds2008$x<-NULL
    40  ds2008$x38106<-NULL
    41  ds2008$x62<-NULL
    42  ds2008Preg<-getinfo('../Data/VU2008MIHOW Dbase Pregn_Deliveries.txt')
    43  ds2008Preg<-rename.vars(ds2008Preg,c("clientid."),c("clientid"))
    44  # fix drop reason with '\xd5' in string
    45  #
    46  ds2008Preg$dropreason<-sub('\xd5',"'",ds2008Preg$dropreason)#
    47  #
    48  # 2009 data
    49  ds2009Info<-getinfo('../Data/VU2009MIHOWInfo.txt')
    50  ds2009Info<-rename.vars(ds2009Info,c("familyid.","familyrace","primaryadutlzip"),c("clientid","race","primaryadultzip"))
    51  #
    52  save(ds2007,ds2008,ds2008Preg,ds2009Info,file='../Data/ds.RData')
    53  #
    54  #select and harmonize variable names
    55  #
    56  
    57  ds2007.1<-subset(ds2007,select=c(clientid,primaryadultfname,primaryadultlname,primaryadultage,familydropdate,dropreason,
    58  actualdeliverydate,duedateofpregnancy,pregnant,language,primaryadultmaritalstatus,mihow_participant,race,primaryadultzip))
    59  ds2007.1$duedate<-as.character( as.Date(ds2007.1$duedateofpregnancy,format="%m/%d/%y"))
    60  ds2007.1$duedateofpregnancy<-NULL
    61  
    62  ds2007.1<-rename.vars(ds2007.1,
    63  c("primaryadultfname","primaryadultlname","primaryadultage","familydropdate","actualdeliverydate","primaryadultmaritalstatus","race","primaryadultzip"),
    64  c("momnamefirst","momnamelast","momage","dropdate","childdob","married","momwhite","mommailingzip"))
    65  ds2007.1$dropreason<-sub('\xd5',"'",ds2007.1$dropreason)
    66  ds2007.1$momdob<-"1900-01-01"
    67  ds2007.1$momdobday<-NA
    68  ds2007.1$momdobmonth<-NA
    69  ds2007.1$momdobyear<-NA
    70  ds2007.1$childdob<-as.character( as.Date(ds2007.1$childdob,format="%m/%d/%y"))
    71  
    72  ds2008.2<-subset(ds2008,select=c(clientid,primaryadultfname,primaryadultlname,language,primaryadultmaritalstatus,mihow_participant,familyrace, primaryadultzip))
    73  ds2008.3<-subset(ds2008Preg,select=c(clientid,primaryadultfname,primaryadultlname,actualdeliverydate,duedateofpregnancy, dropdate,dropreason,pregnant))
    74  ds2008.3$duedate<-as.character( as.Date(ds2008.3$duedateofpregnancy,format="%m/%d/%y"))
    75  ds2008.3$duedateofpregnancy<-NULL
    76  
    77  ds2008.1<-merge(ds2008.2, ds2008.3, by.x = "clientid", by.y = "clientid", all = TRUE)
    78  
    79  ds2008.1<-rename.vars(ds2008.1,
    80  c("primaryadultfname.x","primaryadultlname.x","actualdeliverydate","primaryadultmaritalstatus","familyrace","primaryadultzip"),
    81  c("momnamefirst","momnamelast","childdob","married","momwhite","mommailingzip"))
    82  sort(names(ds2008.1))
    83  
    84  ds2008.1<-remove.vars(ds2008.1,c("primaryadultfname.y","primaryadultlname.y","duedateofpregnancy"))
    85  ds2008.1$momage<-NA
    86  ds2008.1$momdob<-"1900-01-01"
    87  ds2008.1$momdobday<-NA
    88  ds2008.1$momdobmonth<-NA
    89  ds2008.1$momdobyear<-NA
    90  ds2008.1$childdob<-as.character( as.Date(ds2008.1$childdob,format="%d-%b-%y"))
    91  
    92  ds2009.1<-subset(ds2009Info,select=c(clientid,primaryadultfname,primaryadultlname,primaryadultage,primaryadultbirthmon,primaryadultbirthday_,primaryadultbirthyear_,dropdate,dropreason,
    93  actualdeliverydate,duedateofpregnancy,pregnant,
    94  language,primaryadultmaritalstatus,mihow_participant,race,primaryadultzip))
    95  ds2009.1$primaryadultbirthyear<- sprintf("%4d",as.numeric(ds2009.1$primaryadultbirthyear_))
    96  ds2009.1$primaryadultbirthyear_<-NULL
    97  ds2009.1$primaryadultbirthmon<- sprintf("%2.2d",as.numeric(ds2009.1$primaryadultbirthmon))
    98  ds2009.1$primaryadultbirthday_<- sprintf("%2.2d",as.numeric(ds2009.1$primaryadultbirthday_))
    99  ds2009.1$momdob<-paste(ds2009.1$primaryadultbirthyear,"-",ds2009.1$primaryadultbirthmon,"-",ds2009.1$primaryadultbirthday_,sep="")
   100  ds2009.1$duedate<-as.character( as.Date(ds2009.1$duedateofpregnancy,format="%d-%b-%y"))
   101  ds2009.1$duedateofpregnancy<-NULL
   102  #
   103  # first and last name reversed in ds2009.1
   104  ds2009.1<-rename.vars(ds2009.1,
   105  c("primaryadultfname","primaryadultlname","actualdeliverydate","primaryadultmaritalstatus","race",   "primaryadultzip"),
   106  c("momnamelast",      "momnamefirst",     "childdob",          "married",                 "momwhite","mommailingzip"  ))
   107  ds2009.1<-rename.vars(ds2009.1,
   108  c("primaryadultbirthmon","primaryadultbirthday_","primaryadultbirthyear","primaryadultage"),
   109  c("momdobmonth",         "momdobday",            "momdobyear",            "momage"))
   110  ds2009.1$childdob<-as.character( as.Date(ds2009.1$childdob,format="%d-%b-%y"))
   111  
   112  save(ds2007.1,ds2008.1,ds2008.2,ds2008.3,ds2009.1,file='../Data/ds.1.RData')
   113  sort(names(ds2007.1))
   114  sort(names(ds2008.1))
   115  sort(names(ds2009.1))

