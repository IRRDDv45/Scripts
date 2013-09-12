# 80.analyses.r
# source("80.analyses.r",echo=TRUE)
#
rm(list=ls())
library(knitr)
library(gdata)
library(rms)
library(gdata)
library(gmodels)
library(RMySQL)
library(Hmisc)
load('../Data/pop_det.RData')
sink('det_results.txt')
summary(gestationweeksestimated_n ~ group,data=pop)
summary(apgar5minute ~ group,data=pop)

options(warn = -1)
my_test<-function(var,var2,name,name2){
print(paste("Table ",name,'(var) by (var2) ',name2,sep=''))
gg<-table(var,var2)
gg.t<-chisq.test(gg)
#gg$dimnames<-c(name,name2)
if( gg.t$p.value < .049) { print(addmargins(gg))}
p<-paste(name," p = ",as.character(round(gg.t$p.value,2)),sep='')
print(p[1])
#if(gg.t$p.value >= 0.04) print(p[1])
}
group<-names(pop["group"])
my_test(pop$apgar5minute,pop$group,names(pop["apgar5minute"]),group)
my_test(pop$birthwtgrams,pop$group,names(pop["birthwtgrams"]),group) 
my_test(pop$deathind,pop$group,names(pop["deathind"]),group)
my_test(pop$childtransferred,pop$group,names(pop["childtransferred"]),group)
my_test(pop$antibioticsmom,pop$group,names(pop["antibioticsmom"]),group)
my_test(pop$deliveryvaginal,pop$group,names(pop["deliveryvaginal"]),group)
my_test(pop$gestationweeksestimated,pop$group,names(pop["gestationweeksestimated"]),group)
my_test(pop$gestationweeksestimated,pop$group,names(pop["icuadmission"]),group)
my_test(pop$livebirthstotal,pop$group,names(pop["livebirthstotal"]),group)
my_test(pop$icuadmission,pop$group,names(pop["icuadmission"]),group)
my_test(pop$noabnormalconditionsind,pop$group,names(pop["noabnormalconditionsind"]),group)
my_test(pop$nocomplicationsind,pop$group,names(pop["nocomplicationsind"]),group)
my_test(pop$icuadmission,pop$group,names(pop["icuadmission"]),group)
my_test(pop$nocongenitalanomaliesind,pop$group,names(pop["nocongenitalanomaliesind"]),group)
my_test(pop$noinfectionsind,pop$group,names(pop["noinfectionsind"]),group)
my_test(pop$nomedicalriskfactorsind,pop$group,names(pop["nomedicalriskfactorsind"]),group)
my_test(pop$noobstetricprocedureind,pop$group,names(pop["noobstetricprocedureind"]),group)
my_test(pop$paternitysigned,pop$group,names(pop["paternitysigned"]),group)
my_test(pop$nicu,pop$group,names(pop["nicu"]),group)
my_test(pop$prenatalcarebeganpregnancy,pop$group,names(pop["prenatalcarebeganpregnancy"]),group)
#
#  outcome variables
#
pop_vars<-intersect(names(pop),c(
"antibioticsmom","apgar5minute","birthwtgrams","birthwtgrams_n","childtransferred","datayr.y","deathcertnum","deathind","deliveryvaginal","drop_date","dropreason","gestationweeksestimated","gestationweeksestimated_n","gestationweeksgenerated","group","group_n","hospital_id","icuadmission","language","livebirthstotal","momage.y","momdob.y","momeducationcode.y","momhispanicorigin","mommailingzip.y","mommarried.y","momwhite.y","nicu","noabnormalconditionsind","nocomplicationsind","nocongenitalanomaliesind","noinfectionsind","nomedicalriskfactorsind","noobstetricprocedureind","paternitysigned","prenatalcarebeganpregnancy","previouspretermbirth"))

# agar5minute birthwgt livebirthstotal prenatalcarebeganpregnancy 
summary(aov( gestationweeksestimated_n ~ group,pop))
summary(aov( apgar5minute ~ group,pop))
summary(aov( birthwtgrams_n ~ group,pop))

pop$deathind_n<-ifelse(pop$deathind == 'Y',1,0)
summary(aov( deathind_n ~ group,pop))
#
# no icuadmissions
#
#pop$icuadmission_n<-ifelse(pop$icuadmission == 'Y',1,0)
#summary(aov( icuadmission_n ~ group,pop))

pop$lbt<-cut(pop$livebirthstotal,c(0,1,2,24))
summary(aov( livebirthstotal ~ group,pop))
with(pop,addmargins(table(lbt,group)))
with(pop,CrossTable(lbt,group,chisq=TRUE))
with(pop,CrossTable(apgar5minute,group,chisq=TRUE))

pop$momage_n<-as.numeric(pop$momage)
pop$momage_n[pop$momage_n == 99] <- NA
pop$momage_n[pop$momage_n == 0 ] <- NA
summary(aov( momage_n ~ group,pop))
CrossTable(cut(pop$momage_n,5),pop$group,chisq=TRUE)
summary(pop$momage_n ~ pop$group)

pop$nicu_n<-ifelse(pop$nicu == 'Y',1,0)
summary(aov( nicu_n ~ group,pop))
CrossTable(pop$nicu,pop$group,chisq=TRUE)

summary(aov( prenatalcarebeganpregnancy ~ group,pop))
summary(prenatalcarebeganpregnancy ~ group,pop)
CrossTable(pop$prenatalcarebeganpregnancy,pop$group,chisq=TRUE)
CrossTable(cut(pop$prenatalcarebeganpregnancy,c(1,2,8,9)),pop$group,chisq=TRUE)
plot(pop$group,pop$prenatalcarebeganpregnancy)

pop$complicationsind_n<-ifelse(pop$nocomplicationsind == 'Y',0,1)
summary(aov( complicationsind_n ~ group,pop))
CrossTable(pop$complicationsind,pop$group,chisq=TRUE)

pop$congenitalanomaliesind_n<-ifelse(pop$nocongenitalanomaliesind == 'Y',0,1)
summary(aov( congenitalanomaliesind_n ~ group,pop))
CrossTable(pop$congenitalanomaliesind,pop$group,chisq=TRUE)

pop$infectionsind_n<-ifelse(pop$noinfectionsind == 'Y',0,1)
summary(aov( infectionsind_n ~ group,pop))
CrossTable(pop$infectionsind,pop$group,chisq=TRUE)
CrossTable(pop$infectionsind_n,pop$group,chisq=TRUE)

pop$medicalriskfactorsind_n<-ifelse(pop$nomedicalriskfactorsind == 'Y',0,1)
summary(aov( medicalriskfactorsind_n ~ group,pop))
CrossTable(pop$medicalriskfactorsind,pop$group,chisq=TRUE)

pop$obstetricprocedureind_n<-ifelse(pop$noobstetricprocedureind == 'Y',0,1)
summary(aov( obstetricprocedureind_n ~ group,pop))
summary( obstetricprocedureind_n ~ group,pop)
CrossTable(pop$obstetricprocedureind,pop$group,chisq=TRUE)

pop$obstetricprocedureind_n<-ifelse(pop$noobstetricprocedureind == 'Y',0,1)
summary(aov( obstetricprocedureind_n ~ group,pop))
summary( obstetricprocedureind_n ~ group,pop)
CrossTable(pop$obstetricprocedureind,pop$group,chisq=TRUE)

pop$abnormalconditionsind_n<-ifelse(pop$noabnormalconditionsind == 'Y',0,1)
summary(aov(   abnormalconditionsind_n ~ group,pop))
summary(   abnormalconditionsind_n ~ group,pop)
CrossTable(pop$abnormalconditionsind_n,pop$group,chisq=TRUE)

pop$deathind_n<-ifelse(pop$deathind == 'Y',1,0)
summary(aov(   deathind_n ~ group,pop))
summary(   deathind_n ~ group,pop)
CrossTable(pop$deathind_n,pop$group,chisq=TRUE)

contents(pop)
describe(pop)

summary(aov( birthwtgrams_n ~ group,pop))
hist(pop$gestationweeksestimated_n)
plot(density(pop$gestationweeksestimated_n,na.rm=TRUE))
plot(sort(pop$gestationweeksestimated_n),pch='.')
hist(pop$birthwtgrams_n)
plot(density(pop$birthwtgrams_n,na.rm=TRUE))
plot(sort(pop$birthwtgrams_n),pch='.')

gm<-pop[pop$group=='mihow',]
gc<-pop[pop$group=='control',]
#
# anova
#
#summary(pop)

summary(group~birthwtgrams_n,data=pop,test=TRUE,method='reverse')

f1<-aov(birthwtgrams_n ~ group,data=pop)
summary(f1)
f1
f2<-aov(gestationweeksestimated_n ~ group,data=pop)
summary(f2)
f2
summary(group~birthwtgrams_n,data=pop,test=TRUE,method='reverse')
summary(group~gestationweeksestimated_n,data=pop,test=TRUE,method='reverse')
summary(group~gestationweeksestimated_n,data=pop,test=TRUE,method='reverse')
summary(gestationweeksestimated_n~group,data=pop)

pop$group_n<-0
pop$group_n<-ifelse(pop$group=='mihow',1,pop$group_n)

plot(group_n ~ gestationweeksestimated_n,pop)
plot(gestationweeksestimated_n ~ group,pop)
plot(birthwtgrams_n ~ group,pop)

g<-lm(birthwtgrams_n ~ group,pop)
g<-lm(gestationweeksestimated_n ~ group,pop)
gg<-lm(gestationweeksestimated_n ~ group * mommarried,pop)
gg
summary(gg)
do.call(rbind,lapply(split(pop[,'gestationweeksestimated_n'], pop[,'group']), mean,na.rm=TRUE))
do.call(rbind,lapply(split(pop[,'livebirthstotal'], pop[,'group']), mean,na.rm=TRUE))

d <- datadist(pop )           # add a new variable to an existing datadist
options(datadist="d") 
g<-lrm(group ~ deathind_n+apgar5minute+gestationweeksestimated+birthwtgrams_n+infectionsind_n+abnormalconditionsind_n+medicalriskfactorsind_n+nicu_n,pop)
g
summary(g)
 with(pop,(CrossTable(medicalriskfactorsind_n,group)))
sink()
# agar5minute birthwgt livebairthstotal prenatalcarebeganpregnancy 
# noabnormalconditionsind nomedicalriskfactorsind nicu 
