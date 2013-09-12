# 75.SelectControls.r
# source("75.SelectControls.r",echo=TRUE)
# Get controls for probabilistic merge file
rm(list=ls())

library(knitr)
library(gdata)
library(gtools)
library(rms)
library(gdata)
library(gmodels)
library(RMySQL)
library(Hmisc)

load_file<-"../Data/prob_merged.RData"
load(load_file); load('../Data/birth.RData'); load('../Data/mihow.RData')
mihow_birth<-merged

mihow$momdob<-paste(substr(mihow$momdob,7,10),'-',substr(mihow$momdob,1,5),sep='')

#
# match contorls on mihow_birth == birth # livebirthstotal = livebirthstotal # est_childdob = childdob
# momage.x = momage.y #exclude zipcodes not starting with '3';
#
mihow_birth$mommailingzip.y[!(substr(mihow_birth$mommailingzip.y,1,1) == "3")]<-NA
birth$mommailingzip[!(substr(birth$mommailingzip,1,1) == "3")]<-NA
#table(substr(mihow_birth$mommailingzip.x,1,1))
table(substr(mihow_birth$mommailingzip.y,1,1))
table(substr(birth$mommailingzip,1,1))

#load('../Data/mihow_birth.RData')
mihow_birth$childdob<-mihow_birth$childdob.y
mihow_birth$childdobday<-mihow_birth$childdobday.y
mihow_birth$childdobmonth<-mihow_birth$childdobmonth.y
mihow_birth$childdobyear<-mihow_birth$childdobyear.y
mihow_birth$momdob<-mihow_birth$momdob.y
mihow_birth$momdobday<-mihow_birth$momdobday.y
mihow_birth$momdobmonth<-mihow_birth$momdobmonth.y
mihow_birth$momdobyear<-mihow_birth$momdobyear.y
mihow_birth$datayr<-mihow_birth$datayr.y
mihow_birth$momage<-mihow_birth$momage.y

#mihow_birth$momeducationcode<-mihow_birth$momeducationcode.y
mihow_birth$mommailingzip<-mihow_birth$mommailingzip.y
mihow_birth$mommarried<-mihow_birth$mommarried.y
mihow_birth$momnamefirst<-mihow_birth$momnamefirst.y
mihow_birth$momnamelast<-mihow_birth$momnamelast.y
mihow_birth$momwhite<-mihow_birth$momwhite.y
mihow_birth$datayr.y<-NULL

mihow_birth$uid<-mihow_birth$uid.y

mihow_birth1<-remove.vars(mihow_birth,c( 
 "childdobday.y","childdobmonth.y","childdob.y","childdobyear.y" ,"datayr.y",
 "momdobday.y","momdobmonth.y", "momdob.y","momdobyear.y","momage.y",
 "momeducationcode.y", "mommailingzip.y", "mommarried.y","momnamefirst.x", "momnamefirst.y","momnamelast.x", 
 "momnamelast.y", "momwhite.x","momwhite.y","pregnant","primaryadultpovertylevel","programyear","uid.x", "uid.y"))

m_b0<-rename.vars(mihow_birth,c('est_childdob','mommarried.y','livebirthstotal','mommailingzip.x'),c('childdob','mommarried','livebirthstotal','mommailingzip.y'))
m_b0<-remove.vars(m_b0,c('uid.x','uid.y'))

mb<-merge(m_b0,birth,by.x=c('childdob','mommarried','livebirthstotal'),c('childdob','mommarried','livebirthstotal'))

mb<-rename.vars(mb,c("antibioticsmom.x","apgar5minute.x","birthwtgrams.x",
"antibioticsmom.y","apgar5minute.y","birthwtgrams.y", "childtransferred.x","childtransferred.y",          
"deathind.x","deathind.y", "deliveryvaginal.x","deliveryvaginal.y","est_childdobday",
"gestationweeksestimated.x","gestationweeksestimated.y", "gestationweeksgenerated.x","gestationweeksgenerated.y",
"icuadmission.x","icuadmission.y","momage.x","momage.y", 
"momeducationcode.x","momeducationcode.y", "momhispanicorigin.x","momhispanicorigin.y",
"mommailingzip.x","mommailingzip.y", "momwhite.x","momwhite.y",
"nicu.x","nicu.y", "noabnormalconditionsind.x","noabnormalconditionsind.y",
"nocomplicationsind.x","nocomplicationsind.y", "nocongenitalanomaliesind.x","nocongenitalanomaliesind.y",
"noinfectionsind.x","noinfectionsind.y", "nomedicalriskfactorsind.x","nomedicalriskfactorsind.y",
"noobstetricprocedureind.x","noobstetricprocedureind.y", "paternitysigned.x","paternitysigned.y",
"prenatalcarebeganpregnancy.x","prenatalcarebeganpregnancy.y", "previouspretermbirth.x","previouspretermbirth.y"),
c("antibioticsmom.m","apgar5minute.m","birthwtgrams.m", "antibioticsmom.b","apgar5minute.b","birthwtgrams.b",
"childtransferred.m","childtransferred.b","deathind.m","deathind.b",
"deliveryvaginal.m","deliveryvaginal.b","est_childdobday", "gestationweeksestimated.m","gestationweeksestimated.b",
"gestationweeksgenerated.m","gestationweeksgenerated.b", "icuadmission.m","icuadmission.b",           
"momage.m","momage.b", "momeducationcode.m","momeducationcode.b",
"momhispanicorigin.m","momhispanicorigin.b", "mommailingzip.m","mommailingzip.b",
"momwhite.m","momwhite.b", "nicu.m","nicu.b",
"noabnormalconditionsind.m","noabnormalconditionsind.b", "nocomplicationsind.m","nocomplicationsind.b",
"nocongenitalanomaliesind.m","nocongenitalanomaliesind.b", "noinfectionsind.m","noinfectionsind.b",
"nomedicalriskfactorsind.m","nomedicalriskfactorsind.b", "noobstetricprocedureind.m","noobstetricprocedureind.b",
"paternitysigned.m","paternitysigned.b", "prenatalcarebeganpregnancy.m","prenatalcarebeganpregnancy.b",
"previouspretermbirth.m","previouspretermbirth.b")
)
mb<-remove.vars(mb,c("childdobday.y","childdobmonth.y",
"childdob.y","childdobyear.y",
"childidnum.x",
"deathcertnum.x",
"dropdate","dropdateday",
"dropdatemonth","dropdateyear",
"dropreason","duedate",
"duedateday","duedatemonth",
"duedateyear","est_childdobday",
"est_childdobmonth","est_childdobyear",
"hospital_id.x",
"id.x","key",
"language","mid",
"mihow_participant",
"momdobday.y","momdobmonth.y",
"momdob.y","momdobyear.y",
"mommailingzip.y",
"momnamefirst.y","momnamelast.y",
"momnamemaiden","momnamemaidenlast.x",
"momssn.x",
"pregnant","programyear",
"childdob","childdobday.x",
"childdobmonth.x","childdobyear.x",
"momdob.x","momdobday.x",
"momdobmonth.x","momdobyear.x",
"datayr.x",
"momnamefirst.x","momnamelast.x",
"uid.x",
"momnamefirst.y","momnamelast.y",
"datayr.y","mommailingzip.y",
"hospital_id.y","childidnum.y",
"momssn.y","deathcertnum.y",
"momwhite.y","momage.y",
"momdob.y",
"id.y","momnamemaidenlast.y",
"uid.y","childdobyear.y",
"childdobmonth.y","childdobday.y",
"momdobyear.y","momdobmonth.y",
"momdobday.y","childtransferred.m","childtransferred.b"))
mb$antibioticsmom.m<-ifelse(mb$antibioticsmom.m == "Y",1,0)
mb$antibioticsmom.b<-ifelse(mb$antibioticsmom.b == "Y",1,0)
mb$birthwtgrams.m<-as.numeric(mb$birthwtgrams.m)
mb$birthwtgrams.b<-as.numeric(mb$birthwtgrams.b)
mb$momage.m<-as.numeric(mb$momage.m)
mb$momage.b<-as.numeric(mb$momage.b)
mb$momeducationcode.m<-as.numeric(mb$momeducationcode.m)
mb$momeducationcode.b<-as.numeric(mb$momeducationcode.b)

mb$deathind.m<-ifelse(mb$deathind.m == "Y",1,0)
mb$deathind.b<-ifelse(mb$deathind.b == "Y",1,0)
mb$deliveryvaginal.m<-ifelse(mb$deliveryvaginal.m == "Y",1,0)
mb$deliveryvaginal.b<-ifelse(mb$deliveryvaginal.b == "Y",1,0)

mb$icuadmission.m<-ifelse(mb$icuadmission.m == "Y",1,0)
mb$icuadmission.b<-ifelse(mb$icuadmission.b == "Y",1,0)
mb$nicu.m<-ifelse(mb$nicu.m == "Y",1,0)
mb$nicu.b<-ifelse(mb$nicu.b == "Y",1,0)
mb$paternitysigned.m<-ifelse(mb$paternitysigned.m == "Y",1,0)
mb$paternitysigned.b<-ifelse(mb$paternitysigned.b == "Y",1,0)
mb$previouspretermbirth.m<-ifelse(mb$previouspretermbirth.m == "Y",1,0)
mb$previouspretermbirth.b<-ifelse(mb$previouspretermbirth.b == "Y",1,0)

mb$abnormalconditionsind.m<-ifelse(mb$noabnormalconditionsind.m == "Y",0,1)
mb$abnormalconditionsind.b<-ifelse(mb$noabnormalconditionsind.b == "Y",0,1)
mb$congenitalanomaliesind.m<-ifelse(mb$nocongenitalanomaliesind.m == "Y",0,1)
mb$congenitalanomaliesind.b<-ifelse(mb$nocongenitalanomaliesind.b == "Y",0,1)
mb$complicationsind.m<-ifelse(mb$nocomplicationsind.m == "Y",0,1)
mb$complicationsind.b<-ifelse(mb$nocomplicationsind.b == "Y",0,1)
mb$infectionsind.m<-ifelse(mb$noinfectionsind.m == "Y",0,1)
mb$infectionsind.b<-ifelse(mb$noinfectionsind.b == "Y",0,1)
mb$medicalriskfactorsind.m<-ifelse(mb$nomedicalriskfactorsind.m == "Y",0,1)
mb$medicalriskfactorsind.b<-ifelse(mb$nomedicalriskfactorsind.b == "Y",0,1)
mb$obstetricprocedureind.m<-ifelse(mb$noobstetricprocedureind.m == "Y",0,1)
mb$obstetricprocedureind.b<-ifelse(mb$noobstetricprocedureind.b == "Y",0,1)
mb$birthwtgrams.b[mb$birthwtgrams.b < 200]<-NA 
mb$birthwtgrams.m[mb$birthwtgrams.m < 200]<-NA 

gg<-remove.vars(mb,c("mommarried","livebirthstotal","clientid","childdob","mommarried","mommailingzip.b",
"mommailingzip.m","noabnormalconditionsind.m","noabnormalconditionsind.b",
"nocongenitalanomaliesind.m","nocongenitalanomaliesind.b",
"nocomplicationsind.m","nocomplicationsind.b",
"noinfectionsind.m","noinfectionsind.b",
"nomedicalriskfactorsind.m","nomedicalriskfactorsind.b",
"noobstetricprocedureind.m","noobstetricprocedureind.b",
"gestationweeksgenerated.m","gestationweeksgenerated.b",
"momage.m","momage.b",
"paternitysigned.m","paternitysigned.b",
"momwhite.m","momwhite.b"
))
vars<-sort(names(gg))
vars

ttests<-function(x,y){
 t.test(x,y,paired=TRUE)
}
sink("ttests_prob.txt")
for(idx in seq(1,length(vars)-1,2)) {
  cat(paste("\n",vars[idx],"\n"))
  print(ttests(mb[,vars[idx]],mb[,vars[idx+1]]))
  mc<-mean(mb[,vars[idx]],na.rm=TRUE)
  mm<-mean(mb[,vars[idx+1]],na.rm=TRUE)
  print(paste("controls: ",mean(mb[,vars[idx]],  na.rm=TRUE)))
  print(paste("mihow:    ",mean(mb[,vars[idx+1]],na.rm=TRUE)))
  print(paste("controls - mihow:",round(mc-mm,2)))
  print(paste('====================================='))
}

sink()

pop<-mb
pop$method<-'prob'
save(pop,file='../Data/pop_prob.RData')

