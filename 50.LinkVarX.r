# 50.LinkVar.r
# Birth forms were revised in 2004.  Some variables were added some deleted or renames
# source("4.LinkVar.r",echo=TRUE)
#
library(knitr)
library(gdata)
library(rms)
library(gmodels)
library(RMySQL)
library(Hmisc)

load("../Data/mihow.RData")
load('../Data/birth.RData')
mihow$momnamemaiden<-tolower(mihow$momnamemaiden)
options('digits'=10)
options('scipen'=10)
options('stringAsFactors'=FALSE)

k9 <- merge(mihow, birth, by.x = "key9", by.y = "key9")
mihow.9<-subset(mihow,subset=!(mihow$clientid %in% k9$clientid)) 
birth.9<-subset(birth,subset=!(birth$id %in% k9$id))
table(k9$clientid)  #look for dups

k9a <- merge(mihow.9, birth.9, by.x = "key9a", by.y = "key9a")
mihow.9a<-subset(mihow.9,subset=!(mihow.9$clientid %in% k9a$clientid))
birth.9a<-subset(birth.9,subset=!(birth.9$id %in% k9a$id))
table(k9a$clientid)  #look for dups
dim(mihow.9a)

k8 <- merge(mihow.9a, birth.9a, by.x = "key8", by.y = "key8")
mihow.8<-subset(mihow.9a,subset=!(mihow.9a$clientid %in% k8$clientid)) 
birth.8<-subset(birth.9a,subset=!(birth.9a$id %in% k8$id))
table(k8$clientid)  #look for dups
dim(mihow.8)

k8a <- merge(mihow.8, birth.8, by.x = "key8a", by.y = "key8a")
mihow.8a<-subset(mihow.8,subset=!(mihow.8$clientid %in% k8a$clientid))
birth.8a<-subset(birth.8,subset=!(birth.8$id %in% k8a$id))
table(k8a$clientid)  #look for dups
dim(mihow.8a)

k7 <- merge(mihow.8a, birth.8a, by.x = "key7", by.y = "key7")
mihow.7<-subset(mihow.8,subset=!(mihow.8$clientid %in% k7$clientid))
birth.7<-subset(birth.8,subset=!(birth.8$id       %in% k7$id))
table(k7$clientid)  #look for dups
dim(mihow.7 )

k7a <- merge(mihow.7, birth.7, by.x = "key7a", by.y = "key7a")
mihow.7a<-subset(mihow.7,subset=!(mihow.7$clientid %in% k7a$clientid))
birth.7a<-subset(birth.7,subset=!(birth.7$id %in% k7a$id))
table(k7a$clientid)  #look for dups
dim(mihow.7a)

k6 <- merge(mihow.7a, birth.7a, by.x = "key6", by.y = "key6")
mihow.6<-subset(mihow.7,subset=!(mihow.7$clientid %in% k6$clientid))
birth.6<-subset(birth.7,subset=!(birth.7$id %in% k6$id))
table(k6$clientid)  #look for dups
dim(mihow.7a)

k6a <- merge(mihow.6, birth.6, by.x = "key6a", by.y = "key6a")
mihow.6a<-subset(mihow.6,subset=!(mihow.6$clientid %in% k6a$clientid))
birth.6a<-subset(birth.6,subset=!(birth.6$id %in% k6a$id))
table(k6a$clientid)  #look for dups
dim(mihow.6a)

#
# two matches for "232-09"  -> twins
#

k5 <- merge(mihow.6a, birth.6a, by.x = "key5", by.y = "key5")
mihow.5<-subset(mihow.6,subset=!(mihow.6$clientid %in% k5$clientid))
birth.5<-subset(birth.6,subset=!(birth.6$id %in% k5$id))
table(k5$clientid)  #look for dups
dim(mihow.5)

k5a <- merge(mihow.5, birth.5, by.x = "key5a", by.y = "key5a")
mihow.5a<-subset(mihow.5,subset=!(mihow.5$clientid %in% k5a$clientid))
birth.5a<-subset(birth.5,subset=!(birth.5$id %in% k5a$id))
table(k5a$clientid)  #look for dups
dim(mihow.5a)

#
# two matches for "001-09"  -> twins
#

k4 <- merge(mihow.5a, birth.5a, by.x = "key4", by.y = "key4")
mihow.4<-subset(mihow.5,subset=!(mihow.5$clientid %in% k4$clientid))
birth.4<-subset(birth.5,subset=!(birth.5$id %in% k4$id))
#k4[k4$clientid== "520-09",]
table(k4$clientid)  #look for dups
dim(mihow.4a)
#
# two matches for "001-09"  -> twins
#
k3 <- merge(mihow.4, birth.4, by.x = "key3", by.y = "key3")  #155  76
#
# Calculate days between due_date or childdob.x and childdob.y
#
k3$days<-abs(difftime(as.Date(k3$est_childdob),as.Date(k3$childdob.y),units="days"))
#
# remove records where est_childdob and childdob.y are not within 40 days
#
k3$near<-ifelse(k3$days > 40,0,1)
k3<-subset(k3,k3$near == 1)

mihow.3<-subset(mihow.4,subset=!(mihow.4$clientid %in% k3$clientid))
birth.3<-subset(birth.4,subset=!(birth.4$id %in% k3$id))
table(k3$clientid)  #look for dups

#paste(trim(birth$momnamelast),trim(birth$momnamefirst),birth$momdob
look<-function(df,who){df[df$clientid== who,c("near","days","childidnum","due_date","childdob.x","childdob.y","livebirthstotal","momage.x","momage.y","momdob.x","momdob.y","momwhite.x","momwhite.y")]}
#
# look will return 1 record for close dates, 2 for twins, else none
#
# twins 616-09  & 151-09
# VISUALLY INSPECT DUPLICATE
look(k3,"151-09") # "151-09"  twins                             ##childdob 1 month##
look(k3,"616-09") # "616-09" 0180000023480 2010-01-21 <NA> 2010-01-11 1 21 21  #twins ##due_date & childob.y close##
# "616-09" 0180000023481 2010-01-21 <NA> 2010-01-11 2 21 21  #twins ##due_date & childob.y close##
look(k3,"617-09") # "617-09" #0244000031637 2010-06-30 <NA> 2010-05-23 3 24 25       ##due_date & childob.y close##
look(k3,"622-09") # "622-09"  #0244000029978 2010-01-03 <NA> 2010-01-05 7 29 30      ##due_date & childob.y close##
look(k3,"650-09") # "650-09" #0224000006901 2009-10-15 <NA> 2009-10-15 4 26 27       ##due_date & childob.y equal##
look(k3,"665-09") # "665-09" # 0244000030395 2010-02-10 <NA> 2010-02-04 4 32 32      ##due_date & childob.y close##


k2 <- merge(mihow.3, birth.3, by.x = "key2", by.y = "key2")
k2$days<-abs(difftime(as.Date(k2$est_childdob),as.Date(k2$childdob.y),units="days"))
k2$near<-ifelse(k2$days > 40,0,1)
k2<-subset(k2,k2$near == 1)
table(k2$clientid)  #look for dups
#
# find multiple matches and VISUALLY INSPECT
#
look(k2, "661-09")   ##twins##
look(k2, "654-07")   ##delete childidnum = '0181000020974' ##
look(k2, "499-09")   ##delete childidnum = '0181000029228' ##
look(k2, "388-07")   ##delete childidnum = '0180000014996' ##
look(k2, "224-08")   ##delete childidnum = '0181000022054' ##
look(k2, "119-07")   ##delete childidnum = '0181000016614' ##
#
# keep closest childdob
#
k2<-subset(k2,subset=childidnum %nin% c('0181000020974','0181000029228','0180000014996','0181000022054','0181000016614'))
mihow.2<-subset(mihow.3,subset=!(mihow.3$clientid %in% k2$clientid))
birth.2<-subset(birth.3,subset=!(birth.3$id %in% k2$id))

#childdob.x close to childdob.y

k1 <- merge(mihow.2, birth.2, by.x = "key1", by.y = "key1")
dim(k1)
k1$days<-abs(difftime(as.Date(k1$est_childdob),as.Date(k1$childdob.y),units="days"))
k1$near<-ifelse(k1$days > 10,0,1)
k1<-subset(k1,k1$near == 1)
dim(k1)
# year(childdob.y) >= year(momdob.y) + momage.x 
k1$est_momage <-abs(as.numeric(substr(k1$childdob.y,1,4))-as.numeric(substr(k1$momdob.y,1,4))- k1$momage.x )
k1<-subset(k1,k1$est_momage < 2)
table(k1$clientid)  #look for dups
#VISUALLY INSPECT RECORDS BASED ON CLIENTID
look1<-function(df,who){df[df$clientid== who,c("est_momage","days","childidnum","due_date","childdob.x","childdob.y","livebirthstotal","momage.x","momage.y","momdob.x","momdob.y","momwhite.x","momwhite.y")]}
look1(k1,"025-07")  
look1(k1,"185-07")  
look1(k1,"198-09")  
look1(k1,"200-07")  
look1(k1,"207-07")  

look1(k1,"244-09")  
look1(k1,"045-09")  
look1(k1,"315-09")  
look1(k1,"319-07")  
look1(k1,"351-09")

look1(k1,"385-07")
look1(k1,"411-09")
look1(k1,"422-07")
look1(k1,"436-07")
look1(k1,"438-07")

look1(k1,"440-07")
look1(k1,"485-07")
look1(k1,"568-07")
look1(k1,"604-07")
look1(k1,"696-07")
#DELETE SUSPECT MATCHES
k1<-subset(k1,subset=childidnum %nin% c("0181000026315","0224000006497","0504000003200",
"0244000027426","0181000018431","0181000018584","0180000015265","0181000018653","0181000019061",
"0181000019389","0224000004760","0181000019242","0187000002638","0180000016945"))

mihow.1<-subset(mihow.2,subset=!(mihow.2$clientid %in% k1$clientid))
birth.1<-subset(birth.2,subset=!(birth.2$id %in% k1$id))
dim(mihow.1);dim(mihow.2);dim(mihow.1)

gg<-rbind( as.matrix(k9$clientid), 
           as.matrix(k8$clientid), 
           as.matrix(k7$clientid),
           as.matrix(k6$clientid),
           as.matrix(k5$clientid),
           as.matrix(k4$clientid),
           as.matrix(k3$clientid),
           as.matrix(k2$clientid),
           as.matrix(k1$clientid))
#CLEAN UP VARIABLES
k9.a<-remove.vars(k9,c("key9", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
"key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
"key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
"key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
k9.a$key<-'9'
k9.aa<-remove.vars(k9a,c("key9a", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
"key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
"key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
"key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
k9.aa$key<-'9a'

k8.a<-remove.vars(k8,c("key8", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
"key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
"key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
"key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
k8.a$key<-'8'
#k8.aa<-remove.vars(k8a,c("key8a", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
#"key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
#"key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
#"key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
#k8.aa$key<-'8a'

k7.a<-remove.vars(k7,c("key7", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
"key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
"key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
"key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
k7.a$key<-'7'
k7.aa<-remove.vars(k7a,c("key7a", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
"key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
"key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
"key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
k7.aa$key<-'7a'
dim(k7.a);dim(k7.aa)

k6.a<-remove.vars(k6,c("key6", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
"key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
"key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
"key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
k6.a$key<-'6'
k6.aa<-remove.vars(k6a,c("key6a", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
"key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
"key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
"key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
k6.aa$key<-'6a'
dim(k6.a);dim(k6.aa)

k5.a<-remove.vars(k5,c("key5", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
"key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
"key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
"key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
k5.a$key<-'5'
k5.aa<-remove.vars(k5a,c("key5a", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
"key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
"key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
"key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
k5.aa$key<-'5a'
dim(k5.a);dim(k5.aa)

k4.a<-remove.vars(k4,c("key4", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
"key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
"key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
"key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
k4.a$key<-'4'
dim(k4.a)

k3.a<-remove.vars(k3,c("key3", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
"key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
"key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
"key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y","near",'days'))
k3.a$key<-'3'
dim(k3.a)

k2.a<-remove.vars(k2,c("key2", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
"key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
"key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
"key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y","near",'days'))
k2.a$key<-'2'
dim(k2.a)

k1.a<-remove.vars(k1,c("key1", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
"key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
"key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
"key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y","near",'days','est_momage'))
k1.a$key<-'1'
dim(k1.a)
#
# concatenate rows from k9 to k1 merges

gg<-rbind(k9.a,k9.aa,k8.a,k7.a,k7.aa,k6.a,k6.aa,k5.a,k5.aa,k4.a,k3.a,k2.a,k1.a)
#
# calculate total number of rows of matched records
# 
nrow(k9.a) + nrow(k9.aa) +nrow(k8.a) +nrow(k7.a) +nrow(k7.aa) +nrow(k6.a) + nrow(k6.aa) + nrow(k5.a) + nrow(k5.aa) + 
nrow(k4.a) + nrow(k3.a) + nrow(k2.a) + nrow(k1.a)

# select rows saved in gg
#
mihow_birth<-subset(gg,select=sort(names(gg)))

save(mihow_birth,file='../Data/mihow_birth.RData')

