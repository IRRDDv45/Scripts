   #20.rl_run.r
load('../Data/birth.RData')
load('../Data/mihow.RData')
load('../Data/mihow_birth.RData')

common <- intersect(names(mihow), names(birth))[1:11]
b <- birth[,common]
m <- mihow[,common]

m[is.na(mihow$childdob), 'childdob'] <- mihow[is.na(mihow$childdob), 'duedate']
 
 birth.xn <- table(mihow_birth$uid.y)
 sing.match <- names(birth.xn[birth.xn == 1])
 m.ids <- mihow_birth[match(sing.match, mihow_birth$uid.y), c('clientid', 'uid.y')]
 m.ids$mid <- seq(nrow(m.ids))
 b$m.link <- NA
 m$b.link <- NA
 m$b.link[match(m.ids$clientid, mihow$clientid)] <- m.ids$mid
 b$m.link[match(m.ids$uid.y, birth$uid)] <- m.ids$mid
 
 b$initial <- paste(substr(b$momnamefirst, 1, 1), substr(b$momnamelast, 1, 1), sep='')
 m$initial <- paste(substr(m$momnamefirst, 1, 1), substr(m$momnamelast, 1, 1), sep='')
 
 rpairs <- compare.linkage(b, m, blockfld=c(13), strcmp=c(1,2), strcmpfun=jarowinkler, identity1=b$m.link, identity2=m$b.link, exclude=c(7,9,10,11,12,13))
 nrow(rpairs$pairs)
 
 rpairs1 <- emWeights(rpairs)
 
 summary(rpairs1)
 edit(getPairs(rpairs1, 15, 14))
 
 rpairs2 <- emClassify(rpairs1, threshold.lower=5, threshold.upper=14)
 summary(rpairs2)
 
 unknown <- which(rpairs2$Wdata < 14 & rpairs2$Wdata >= 5)
 for(i in unknown) {
   p1 <- b[rpairs2$pairs$id1[i],]
   p2 <- m[rpairs2$pairs$id2[i],]
   da <- abs(as.integer(p1$momage) - as.integer(p2$momage))
   dd <- abs(as.numeric(difftime(as.Date(p1$childdob), as.Date(p2$childdob), units='days')))
   if(!is.na(da) && da == 1) rpairs2$Wdata[i] <- rpairs2$Wdata[i] + 5
   if(!is.na(dd) && dd > 0 && dd < 15) rpairs2$Wdata[i] <- rpairs2$Wdata[i] + 5
 }
 
 known <- which(rpairs2$Wdata >= 14)
 for(i in known) {
   p1 <- b[rpairs2$pairs$id1[i],]
   p2 <- m[rpairs2$pairs$id2[i],]
   dd <- as.numeric(difftime(as.Date(p1$childdob), as.Date(p2$childdob), units='days'))
   if(!is.na(dd) && (dd > 60 || dd < -120)) rpairs2$Wdata[i] <- 0
 }
 
 matched <- rpairs2$pairs[rpairs2$Wdata > 14,]
 for(i in as.numeric(names(Filter(isTRUE, table(matched$id2) > 1)))) {
   lab <- matched[matched$id2 == i,]
   rec <- birth[lab$id1,]
   if((!is.na(rec[1,'momssn']) && rec[1,'momssn'] == rec[2,'momssn']) || (rec[1,'momdob'] == rec[2,'momdob'] && rec[1,'childdob'] == rec[2,'childdob'])) {
     choice <- which.min(rec$livebirthstotal)
   } else {
     choice <- which.min(abs(as.Date(rec$childdob) - as.Date(mihow[i,'childdob'])))
   }
   rem.ix <- as.numeric(rownames(lab)[setdiff(seq(nrow(lab)), choice)])
   rpairs2$Wdata[rem.ix] <- 0
 }
 
 rpairs3 <- emClassify(rpairs2, threshold.lower=5, threshold.upper=14)
 summary(rpairs3)
 matched <- rpairs3$pairs[rpairs3$Wdata > 14,]
 merge <- data.frame(mihow.uid=mihow$clientid[matched$id2], mihow.row=matched$id2, birth.uid=birth$uid[matched$id1], birth.row=matched$id1)
 write.csv(merge, 'prob_match_res.csv', row.names=FALSE)
