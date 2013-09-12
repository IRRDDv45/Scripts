# "100.zips.r"
# source("100.zips.r",echo=TRUE)
#
library(knitr)
library(gdata)
library(gtools)
library(rms)
library(gdata)
library(gmodels)
library(RMySQL)
library(Hmisc)

file<-"../Data/ShelbyzipcodesPairs.csv"
zips<-read.delim(file,sep=',',stringsAsFactors=FALSE)
names(zips)<-tolower(names(zips))

#a <- data.frame of zips
#x <- vector of MIHOW zips
#y <- vector birth zips
a<-zips
x<-zips$mihow
y<-zips$birth
zips_near<-apply(a, MARGIN=1, paste, collapse='')
#paste(x, y, sep="",collapse='') %in% apply(a, MARGIN=1, paste, collapse='')
#paste(x, y, sep="",collapse='') %in% apply(a, MARGIN=1, paste, collapse='')
save(zips,zips_near,file='../Data/zips.RData')

load('../Data/mihow_birth.RData'); 
load('../Data/zips.RData'); 
mihow_birth$zipkey<-paste(mihow_birth$mommailingzip.x, mihow_birth$mommailingzip.y, sep="")

zips$zipkey<-paste(zips$mihow, zips$birth, sep="")
gg<-merge(mihow_birth,zips,by="zipkey")
gg2<-as.dataframe(table(gg$clientid))
gg2<-rename.vars(gg2,"Var1","clientid")
dups<-subset(gg2,gg2$Freq > 1,select="clientid")
look<-function(df,who){df[df$clientid== who,c("clientid","childidnum","duedate","childdob.y","livebirthstotal","momwhite.y","momnamefirst.y","momnamelast.y")]}


#k3 dups

vars<-c("clientid","childidnum","momage.x","momage.y","childdobyear.x","childdobyear.y","programyear","momnamelast.y","dup")
look<-function(df,who){df[df$clientid== who,intersect(vars,df)]}

dups<-as.data.frame(table(k3$clientid))[dups$Freq > 1,]
names(dups)<-c("clientid","Freq")
k3$dup<-0
k3$dup[k3$clientid %in% dups$clientid]<-1
table( k3$dup == 1)
k3$dup<-ifelse(k3$dup == 1 & k3$momage.x == k3$momage.y & !is.na(k3$momage.y), 0,k3$dup)
table( k3$dup == 1)
ttt<-function(indx,dups){print(look(k3,dups[indx]))}
gg<-k3[k3$dup == 1,"clientid"]
for( i in 1:length(gg)) {ttt(i,gg);readline(">>")}

k3$dup<-ifelse(k3$dup == 1 & k3$childdobyear.y >= k3$programyear & !is.na(k3$childdobyear.y), 0,k3$dup)
table( k3$dup == 1)
gg<-k3[k3$dup == 1,"clientid"]
for( i in 1:length(gg)) {ttt(i);readline(">>")}
k3<-k3[k3$dup == 0,]

dups<-as.data.frame(table(k2$clientid))
dups<-subset(dups, dups$Freq > 1)
names(dups)<-c("clientid","Freq")

k2$dup<-0
k2$dup[k2$clientid %in% dups$clientid]<-1
k2$dup<-ifelse(k2$clientid %in% dups$clientid,1,0)

table( k2$dup == 1)
k2$dup<-ifelse(k2$dup == 1 & k2$momage.x == k2$momage.y & !is.na(k2$momage.y), 0,k2$dup)
table( k2$dup == 1)
ttt<-function(indx){print(look(k2,gg[indx]))}
gg<-k2[k2$dup == 1 & !is.na(k2$clientid),"clientid"]
gg<-gg[!is.na(gg)]
for( i in 1:length(gg)) {ttt(i);readline(">>")}

k2$dup<-ifelse(k2$dup == 1 & k2$childdobyear.y >= k2$programyear & !is.na(k2$childdobyear.y), 0,k2$dup)
table( k2$dup == 1)
gg<-k2[k2$dup == 1,"clientid"]
gg<-gg[!is.na(gg)]
for( i in 1:length(gg)) {ttt(i);readline(">>")}
