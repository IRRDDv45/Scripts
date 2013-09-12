     1  # 75.SelectControls.r
     2  # source("75.SelectControls.r",echo=TRUE)
     3  # Get controls for probabilistic merge file
     4  rm(list=ls())
     5  
     6  library(knitr)
     7  library(gdata)
     8  library(gtools)
     9  library(rms)
    10  library(gdata)
    11  library(gmodels)
    12  library(RMySQL)
    13  library(Hmisc)
    14  
    15  load_file<-"../Data/prob_merged.RData"
    16  load(load_file); load('../Data/birth.RData'); load('../Data/mihow.RData')
    17  mihow_birth<-merged
    18  
    19  mihow$momdob<-paste(substr(mihow$momdob,7,10),'-',substr(mihow$momdob,1,5),sep='')
    20  
    21  #
    22  # match contorls on mihow_birth == birth # livebirthstotal = livebirthstotal # est_childdob = childdob
    23  # momage.x = momage.y #exclude zipcodes not starting with '3';
    24  #
    25  mihow_birth$mommailingzip.y[!(substr(mihow_birth$mommailingzip.y,1,1) == "3")]<-NA
    26  birth$mommailingzip[!(substr(birth$mommailingzip,1,1) == "3")]<-NA
    27  #table(substr(mihow_birth$mommailingzip.x,1,1))
    28  table(substr(mihow_birth$mommailingzip.y,1,1))
    29  table(substr(birth$mommailingzip,1,1))
    30  
    31  #load('../Data/mihow_birth.RData')
    32  mihow_birth$childdob<-mihow_birth$childdob.y
    33  mihow_birth$childdobday<-mihow_birth$childdobday.y
    34  mihow_birth$childdobmonth<-mihow_birth$childdobmonth.y
    35  mihow_birth$childdobyear<-mihow_birth$childdobyear.y
    36  mihow_birth$momdob<-mihow_birth$momdob.y
    37  mihow_birth$momdobday<-mihow_birth$momdobday.y
    38  mihow_birth$momdobmonth<-mihow_birth$momdobmonth.y
    39  mihow_birth$momdobyear<-mihow_birth$momdobyear.y
    40  mihow_birth$datayr<-mihow_birth$datayr.y
    41  mihow_birth$momage<-mihow_birth$momage.y
    42  
    43  #mihow_birth$momeducationcode<-mihow_birth$momeducationcode.y
    44  mihow_birth$mommailingzip<-mihow_birth$mommailingzip.y
    45  mihow_birth$mommarried<-mihow_birth$mommarried.y
    46  mihow_birth$momnamefirst<-mihow_birth$momnamefirst.y
    47  mihow_birth$momnamelast<-mihow_birth$momnamelast.y
    48  mihow_birth$momwhite<-mihow_birth$momwhite.y
    49  mihow_birth$datayr.y<-NULL
    50  
    51  mihow_birth$uid<-mihow_birth$uid.y
    52  
    53  mihow_birth1<-remove.vars(mihow_birth,c( 
    54   "childdobday.y","childdobmonth.y","childdob.y","childdobyear.y" ,"datayr.y",
    55   "momdobday.y","momdobmonth.y", "momdob.y","momdobyear.y","momage.y",
    56   "momeducationcode.y", "mommailingzip.y", "mommarried.y","momnamefirst.x", "momnamefirst.y","momnamelast.x", 
    57   "momnamelast.y", "momwhite.x","momwhite.y","pregnant","primaryadultpovertylevel","programyear","uid.x", "uid.y"))
    58  
    59  m_b0<-rename.vars(mihow_birth,c('est_childdob','mommarried.y','livebirthstotal','mommailingzip.x'),c('childdob','mommarried','livebirthstotal','mommailingzip.y'))
    60  m_b0<-remove.vars(m_b0,c('uid.x','uid.y'))
    61  
    62  mb<-merge(m_b0,birth,by.x=c('childdob','mommarried','livebirthstotal'),c('childdob','mommarried','livebirthstotal'))
    63  
    64  mb<-rename.vars(mb,c("antibioticsmom.x","apgar5minute.x","birthwtgrams.x",
    65  "antibioticsmom.y","apgar5minute.y","birthwtgrams.y", "childtransferred.x","childtransferred.y",          
    66  "deathind.x","deathind.y", "deliveryvaginal.x","deliveryvaginal.y","est_childdobday",
    67  "gestationweeksestimated.x","gestationweeksestimated.y", "gestationweeksgenerated.x","gestationweeksgenerated.y",
    68  "icuadmission.x","icuadmission.y","momage.x","momage.y", 
    69  "momeducationcode.x","momeducationcode.y", "momhispanicorigin.x","momhispanicorigin.y",
    70  "mommailingzip.x","mommailingzip.y", "momwhite.x","momwhite.y",
    71  "nicu.x","nicu.y", "noabnormalconditionsind.x","noabnormalconditionsind.y",
    72  "nocomplicationsind.x","nocomplicationsind.y", "nocongenitalanomaliesind.x","nocongenitalanomaliesind.y",
    73  "noinfectionsind.x","noinfectionsind.y", "nomedicalriskfactorsind.x","nomedicalriskfactorsind.y",
    74  "noobstetricprocedureind.x","noobstetricprocedureind.y", "paternitysigned.x","paternitysigned.y",
    75  "prenatalcarebeganpregnancy.x","prenatalcarebeganpregnancy.y", "previouspretermbirth.x","previouspretermbirth.y"),
    76  c("antibioticsmom.m","apgar5minute.m","birthwtgrams.m", "antibioticsmom.b","apgar5minute.b","birthwtgrams.b",
    77  "childtransferred.m","childtransferred.b","deathind.m","deathind.b",
    78  "deliveryvaginal.m","deliveryvaginal.b","est_childdobday", "gestationweeksestimated.m","gestationweeksestimated.b",
    79  "gestationweeksgenerated.m","gestationweeksgenerated.b", "icuadmission.m","icuadmission.b",           
    80  "momage.m","momage.b", "momeducationcode.m","momeducationcode.b",
    81  "momhispanicorigin.m","momhispanicorigin.b", "mommailingzip.m","mommailingzip.b",
    82  "momwhite.m","momwhite.b", "nicu.m","nicu.b",
    83  "noabnormalconditionsind.m","noabnormalconditionsind.b", "nocomplicationsind.m","nocomplicationsind.b",
    84  "nocongenitalanomaliesind.m","nocongenitalanomaliesind.b", "noinfectionsind.m","noinfectionsind.b",
    85  "nomedicalriskfactorsind.m","nomedicalriskfactorsind.b", "noobstetricprocedureind.m","noobstetricprocedureind.b",
    86  "paternitysigned.m","paternitysigned.b", "prenatalcarebeganpregnancy.m","prenatalcarebeganpregnancy.b",
    87  "previouspretermbirth.m","previouspretermbirth.b")
    88  )
    89  mb<-remove.vars(mb,c("childdobday.y","childdobmonth.y",
    90  "childdob.y","childdobyear.y",
    91  "childidnum.x",
    92  "deathcertnum.x",
    93  "dropdate","dropdateday",
    94  "dropdatemonth","dropdateyear",
    95  "dropreason","duedate",
    96  "duedateday","duedatemonth",
    97  "duedateyear","est_childdobday",
    98  "est_childdobmonth","est_childdobyear",
    99  "hospital_id.x",
   100  "id.x","key",
   101  "language","mid",
   102  "mihow_participant",
   103  "momdobday.y","momdobmonth.y",
   104  "momdob.y","momdobyear.y",
   105  "mommailingzip.y",
   106  "momnamefirst.y","momnamelast.y",
   107  "momnamemaiden","momnamemaidenlast.x",
   108  "momssn.x",
   109  "pregnant","programyear",
   110  "childdob","childdobday.x",
   111  "childdobmonth.x","childdobyear.x",
   112  "momdob.x","momdobday.x",
   113  "momdobmonth.x","momdobyear.x",
   114  "datayr.x",
   115  "momnamefirst.x","momnamelast.x",
   116  "uid.x",
   117  "momnamefirst.y","momnamelast.y",
   118  "datayr.y","mommailingzip.y",
   119  "hospital_id.y","childidnum.y",
   120  "momssn.y","deathcertnum.y",
   121  "momwhite.y","momage.y",
   122  "momdob.y",
   123  "id.y","momnamemaidenlast.y",
   124  "uid.y","childdobyear.y",
   125  "childdobmonth.y","childdobday.y",
   126  "momdobyear.y","momdobmonth.y",
   127  "momdobday.y","childtransferred.m","childtransferred.b"))
   128  mb$antibioticsmom.m<-ifelse(mb$antibioticsmom.m == "Y",1,0)
   129  mb$antibioticsmom.b<-ifelse(mb$antibioticsmom.b == "Y",1,0)
   130  mb$birthwtgrams.m<-as.numeric(mb$birthwtgrams.m)
   131  mb$birthwtgrams.b<-as.numeric(mb$birthwtgrams.b)
   132  mb$momage.m<-as.numeric(mb$momage.m)
   133  mb$momage.b<-as.numeric(mb$momage.b)
   134  mb$momeducationcode.m<-as.numeric(mb$momeducationcode.m)
   135  mb$momeducationcode.b<-as.numeric(mb$momeducationcode.b)
   136  
   137  mb$deathind.m<-ifelse(mb$deathind.m == "Y",1,0)
   138  mb$deathind.b<-ifelse(mb$deathind.b == "Y",1,0)
   139  mb$deliveryvaginal.m<-ifelse(mb$deliveryvaginal.m == "Y",1,0)
   140  mb$deliveryvaginal.b<-ifelse(mb$deliveryvaginal.b == "Y",1,0)
   141  
   142  mb$icuadmission.m<-ifelse(mb$icuadmission.m == "Y",1,0)
   143  mb$icuadmission.b<-ifelse(mb$icuadmission.b == "Y",1,0)
   144  mb$nicu.m<-ifelse(mb$nicu.m == "Y",1,0)
   145  mb$nicu.b<-ifelse(mb$nicu.b == "Y",1,0)
   146  mb$paternitysigned.m<-ifelse(mb$paternitysigned.m == "Y",1,0)
   147  mb$paternitysigned.b<-ifelse(mb$paternitysigned.b == "Y",1,0)
   148  mb$previouspretermbirth.m<-ifelse(mb$previouspretermbirth.m == "Y",1,0)
   149  mb$previouspretermbirth.b<-ifelse(mb$previouspretermbirth.b == "Y",1,0)
   150  
   151  mb$abnormalconditionsind.m<-ifelse(mb$noabnormalconditionsind.m == "Y",0,1)
   152  mb$abnormalconditionsind.b<-ifelse(mb$noabnormalconditionsind.b == "Y",0,1)
   153  mb$congenitalanomaliesind.m<-ifelse(mb$nocongenitalanomaliesind.m == "Y",0,1)
   154  mb$congenitalanomaliesind.b<-ifelse(mb$nocongenitalanomaliesind.b == "Y",0,1)
   155  mb$complicationsind.m<-ifelse(mb$nocomplicationsind.m == "Y",0,1)
   156  mb$complicationsind.b<-ifelse(mb$nocomplicationsind.b == "Y",0,1)
   157  mb$infectionsind.m<-ifelse(mb$noinfectionsind.m == "Y",0,1)
   158  mb$infectionsind.b<-ifelse(mb$noinfectionsind.b == "Y",0,1)
   159  mb$medicalriskfactorsind.m<-ifelse(mb$nomedicalriskfactorsind.m == "Y",0,1)
   160  mb$medicalriskfactorsind.b<-ifelse(mb$nomedicalriskfactorsind.b == "Y",0,1)
   161  mb$obstetricprocedureind.m<-ifelse(mb$noobstetricprocedureind.m == "Y",0,1)
   162  mb$obstetricprocedureind.b<-ifelse(mb$noobstetricprocedureind.b == "Y",0,1)
   163  mb$birthwtgrams.b[mb$birthwtgrams.b < 200]<-NA 
   164  mb$birthwtgrams.m[mb$birthwtgrams.m < 200]<-NA 
   165  
   166  gg<-remove.vars(mb,c("mommarried","livebirthstotal","clientid","childdob","mommarried","mommailingzip.b",
   167  "mommailingzip.m","noabnormalconditionsind.m","noabnormalconditionsind.b",
   168  "nocongenitalanomaliesind.m","nocongenitalanomaliesind.b",
   169  "nocomplicationsind.m","nocomplicationsind.b",
   170  "noinfectionsind.m","noinfectionsind.b",
   171  "nomedicalriskfactorsind.m","nomedicalriskfactorsind.b",
   172  "noobstetricprocedureind.m","noobstetricprocedureind.b",
   173  "gestationweeksgenerated.m","gestationweeksgenerated.b",
   174  "momage.m","momage.b",
   175  "paternitysigned.m","paternitysigned.b",
   176  "momwhite.m","momwhite.b"
   177  ))
   178  vars<-sort(names(gg))
   179  vars
   180  
   181  ttests<-function(x,y){
   182   t.test(x,y,paired=TRUE)
   183  }
   184  sink("ttests_prob.txt")
   185  for(idx in seq(1,length(vars)-1,2)) {
   186    cat(paste("\n",vars[idx],"\n"))
   187    print(ttests(mb[,vars[idx]],mb[,vars[idx+1]]))
   188    mc<-mean(mb[,vars[idx]],na.rm=TRUE)
   189    mm<-mean(mb[,vars[idx+1]],na.rm=TRUE)
   190    print(paste("controls: ",mean(mb[,vars[idx]],  na.rm=TRUE)))
   191    print(paste("mihow:    ",mean(mb[,vars[idx+1]],na.rm=TRUE)))
   192    print(paste("controls - mihow:",round(mc-mm,2)))
   193    print(paste('====================================='))
   194  }
   195  
   196  sink()
   197  
   198  pop<-mb
   199  pop$method<-'prob'
   200  save(pop,file='../Data/pop_prob.RData')
   201  
