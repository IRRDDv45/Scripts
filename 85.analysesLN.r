     1  # 85.analyses.r
     2  # source("85.analyses.r",echo=TRUE)
     3  #
     4  rm(list=ls())
     5  library(knitr)
     6  library(gdata)
     7  library(rms)
     8  library(gdata)
     9  library(gmodels)
    10  library(RMySQL)
    11  library(Hmisc)
    12  load('../Data/pop_prob.RData')
    13  sink('prob_results.txt')
    14  summary(gestationweeksestimated_n ~ group,data=pop)
    15  t.test(gestationweeksestimated_n ~ group,data=pop)
    16  
    17  options(warn = -1)
    18  my_test<-function(var,var2,name,name2){
    19  print(paste("Table ",name,'(var) by (var2) ',name2,sep=''))
    20  gg<-table(var,var2)
    21  gg.t<-chisq.test(gg)
    22  #gg$dimnames<-c(name,name2)
    23  if( gg.t$p.value < .049) { print(addmargins(gg))}
    24  p<-paste(name," p = ",as.character(round(gg.t$p.value,2)),sep='')
    25  print(p[1])
    26  #if(gg.t$p.value >= 0.04) print(p[1])
    27  }
    28  group<-names(pop["group"])
    29  my_test(pop$apgar5minute,pop$group,names(pop["apgar5minute"]),group)
    30  my_test(pop$birthwtgrams,pop$group,names(pop["birthwtgrams"]),group) 
    31  my_test(pop$deathind,pop$group,names(pop["deathind"]),group)
    32  my_test(pop$childtransferred,pop$group,names(pop["childtransferred"]),group)
    33  my_test(pop$antibioticsmom,pop$group,names(pop["antibioticsmom"]),group)
    34  my_test(pop$deliveryvaginal,pop$group,names(pop["deliveryvaginal"]),group)
    35  my_test(pop$gestationweeksestimated,pop$group,names(pop["gestationweeksestimated"]),group)
    36  my_test(pop$gestationweeksestimated,pop$group,names(pop["icuadmission"]),group)
    37  my_test(pop$livebirthstotal,pop$group,names(pop["livebirthstotal"]),group)
    38  my_test(pop$icuadmission,pop$group,names(pop["icuadmission"]),group)
    39  my_test(pop$noabnormalconditionsind,pop$group,names(pop["noabnormalconditionsind"]),group)
    40  my_test(pop$nocomplicationsind,pop$group,names(pop["nocomplicationsind"]),group)
    41  my_test(pop$icuadmission,pop$group,names(pop["icuadmission"]),group)
    42  my_test(pop$nocongenitalanomaliesind,pop$group,names(pop["nocongenitalanomaliesind"]),group)
    43  my_test(pop$noinfectionsind,pop$group,names(pop["noinfectionsind"]),group)
    44  my_test(pop$nomedicalriskfactorsind,pop$group,names(pop["nomedicalriskfactorsind"]),group)
    45  my_test(pop$noobstetricprocedureind,pop$group,names(pop["noobstetricprocedureind"]),group)
    46  my_test(pop$paternitysigned,pop$group,names(pop["paternitysigned"]),group)
    47  my_test(pop$nicu,pop$group,names(pop["nicu"]),group)
    48  my_test(pop$prenatalcarebeganpregnancy,pop$group,names(pop["prenatalcarebeganpregnancy"]),group)
    49  #
    50  #  outcome variables
    51  #
    52  pop_vars<-intersect(names(pop),c(
    53  "antibioticsmom","apgar5minute","birthwtgrams","birthwtgrams_n","childtransferred","datayr.y","deathcertnum","deathind","deliveryvaginal","drop_date","dropreason","gestationweeksestimated","gestationweeksestimated_n","gestationweeksgenerated","group","group_n","hospital_id","icuadmission","language","livebirthstotal","momage.y","momdob.y","momeducationcode.y","momhispanicorigin","mommailingzip.y","mommarried.y","momwhite.y","nicu","noabnormalconditionsind","nocomplicationsind","nocongenitalanomaliesind","noinfectionsind","nomedicalriskfactorsind","noobstetricprocedureind","paternitysigned","prenatalcarebeganpregnancy","previouspretermbirth"))
    54  
    55  # agar5minute birthwgt livebirthstotal prenatalcarebeganpregnancy 
    56  summary(aov( gestationweeksestimated_n ~ group,pop))
    57  summary(aov( apgar5minute ~ group,pop))
    58  summary(aov( birthwtgrams_n ~ group,pop))
    59  
    60  pop$deathind_n<-ifelse(pop$deathind == 'Y',1,0)
    61  summary(aov( deathind_n ~ group,pop))
    62  #
    63  # no icuadmissions
    64  #
    65  #pop$icuadmission_n<-ifelse(pop$icuadmission == 'Y',1,0)
    66  #summary(aov( icuadmission_n ~ group,pop))
    67  
    68  pop$lbt<-cut(pop$livebirthstotal,c(0,1,2,24))
    69  summary(aov( livebirthstotal ~ group,pop))
    70  with(pop,addmargins(table(lbt,group)))
    71  with(pop,CrossTable(lbt,group,chisq=TRUE))
    72  with(pop,CrossTable(apgar5minute,group,chisq=TRUE))
    73  
    74  pop$momage_n<-as.numeric(pop$momage)
    75  pop$momage_n[pop$momage_n == 99] <- NA
    76  pop$momage_n[pop$momage_n == 0 ] <- NA
    77  summary(aov( momage_n ~ group,pop))
    78  CrossTable(cut(pop$momage_n,5),pop$group,chisq=TRUE)
    79  summary(pop$momage_n ~ pop$group)
    80  
    81  pop$nicu_n<-ifelse(pop$nicu == 'Y',1,0)
    82  summary(aov( nicu_n ~ group,pop))
    83  CrossTable(pop$nicu,pop$group,chisq=TRUE)
    84  
    85  summary(aov( prenatalcarebeganpregnancy ~ group,pop))
    86  summary(prenatalcarebeganpregnancy ~ group,pop)
    87  CrossTable(pop$prenatalcarebeganpregnancy,pop$group,chisq=TRUE)
    88  CrossTable(cut(pop$prenatalcarebeganpregnancy,c(1,2,8,9)),pop$group,chisq=TRUE)
    89  plot(pop$group,pop$prenatalcarebeganpregnancy)
    90  
    91  pop$complicationsind_n<-ifelse(pop$nocomplicationsind == 'Y',0,1)
    92  summary(aov( complicationsind_n ~ group,pop))
    93  CrossTable(pop$complicationsind,pop$group,chisq=TRUE)
    94  
    95  pop$congenitalanomaliesind_n<-ifelse(pop$nocongenitalanomaliesind == 'Y',0,1)
    96  summary(aov( congenitalanomaliesind_n ~ group,pop))
    97  CrossTable(pop$congenitalanomaliesind,pop$group,chisq=TRUE)
    98  
    99  pop$infectionsind_n<-ifelse(pop$noinfectionsind == 'Y',0,1)
   100  summary(aov( infectionsind_n ~ group,pop))
   101  CrossTable(pop$infectionsind,pop$group,chisq=TRUE)
   102  CrossTable(pop$infectionsind_n,pop$group,chisq=TRUE)
   103  
   104  pop$medicalriskfactorsind_n<-ifelse(pop$nomedicalriskfactorsind == 'Y',0,1)
   105  summary(aov( medicalriskfactorsind_n ~ group,pop))
   106  CrossTable(pop$medicalriskfactorsind,pop$group,chisq=TRUE)
   107  
   108  pop$obstetricprocedureind_n<-ifelse(pop$noobstetricprocedureind == 'Y',0,1)
   109  summary(aov( obstetricprocedureind_n ~ group,pop))
   110  summary( obstetricprocedureind_n ~ group,pop)
   111  CrossTable(pop$obstetricprocedureind,pop$group,chisq=TRUE)
   112  
   113  pop$obstetricprocedureind_n<-ifelse(pop$noobstetricprocedureind == 'Y',0,1)
   114  summary(aov( obstetricprocedureind_n ~ group,pop))
   115  summary( obstetricprocedureind_n ~ group,pop)
   116  CrossTable(pop$obstetricprocedureind,pop$group,chisq=TRUE)
   117  
   118  pop$abnormalconditionsind_n<-ifelse(pop$noabnormalconditionsind == 'Y',0,1)
   119  summary(aov(   abnormalconditionsind_n ~ group,pop))
   120  summary(   abnormalconditionsind_n ~ group,pop)
   121  CrossTable(pop$abnormalconditionsind_n,pop$group,chisq=TRUE)
   122  
   123  pop$deathind_n<-ifelse(pop$deathind == 'Y',1,0)
   124  summary(aov(   deathind_n ~ group,pop))
   125  summary(   deathind_n ~ group,pop)
   126  CrossTable(pop$deathind_n,pop$group,chisq=TRUE)
   127  
   128  contents(pop)
   129  describe(pop)
   130  
   131  summary(aov( birthwtgrams_n ~ group,pop))
   132  hist(pop$gestationweeksestimated_n)
   133  plot(density(pop$gestationweeksestimated_n,na.rm=TRUE))
   134  plot(sort(pop$gestationweeksestimated_n),pch='.')
   135  hist(pop$birthwtgrams_n)
   136  plot(density(pop$birthwtgrams_n,na.rm=TRUE))
   137  plot(sort(pop$birthwtgrams_n),pch='.')
   138  
   139  gm<-pop[pop$group=='mihow',]
   140  gc<-pop[pop$group=='control',]
   141  #
   142  # anova
   143  #
   144  #summary(pop)
   145  
   146  summary(group~birthwtgrams_n,data=pop,test=TRUE,method='reverse')
   147  
   148  f1<-aov(birthwtgrams_n ~ group,data=pop)
   149  summary(f1)
   150  f1
   151  f2<-aov(gestationweeksestimated_n ~ group,data=pop)
   152  summary(f2)
   153  f2
   154  summary(group~birthwtgrams_n,data=pop,test=TRUE,method='reverse')
   155  summary(group~gestationweeksestimated_n,data=pop,test=TRUE,method='reverse')
   156  summary(gestationweeksestimated_n~group,data=pop)
   157  
   158  pop$group_n<-0
   159  pop$group_n<-ifelse(pop$group=='mihow',1,pop$group_n)
   160  
   161  plot(group_n ~ gestationweeksestimated_n,pop)
   162  plot(gestationweeksestimated_n ~ group,pop)
   163  plot(birthwtgrams_n ~ group,pop)
   164  
   165  g<-lm(birthwtgrams_n ~ group,pop)
   166  g<-lm(gestationweeksestimated_n ~ group,pop)
   167  gg<-lm(gestationweeksestimated_n ~ group * mommarried,pop)
   168  gg
   169  summary(gg)
   170  do.call(rbind,lapply(split(pop[,'gestationweeksestimated_n'], pop[,'group']), mean,na.rm=TRUE))
   171  do.call(rbind,lapply(split(pop[,'livebirthstotal'], pop[,'group']), mean,na.rm=TRUE))
   172  
   173  d <- datadist(pop )           # add a new variable to an existing datadist
   174  options(datadist="d") 
   175  g<-lrm(group ~ deathind_n+apgar5minute+gestationweeksestimated+birthwtgrams_n+infectionsind_n+abnormalconditionsind_n+medicalriskfactorsind_n+nicu_n,pop)
   176  g
   177  summary(g)
   178   with(pop,(CrossTable(medicalriskfactorsind_n,group)))
   179  sink()
   180  # agar5minute birthwgt livebairthstotal prenatalcarebeganpregnancy 
   181  # noabnormalconditionsind nomedicalriskfactorsind nicu 
