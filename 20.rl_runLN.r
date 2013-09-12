 20.rl_run.r
1 load('../Data/birth.RData')
2 load('../Data/mihow.RData')
3 load('../Data/mihow_birth.RData')
4 
5 common <- intersect(names(mihow), names(birth))[1:11]
6 b <- birth[,common]
7 m <- mihow[,common]
8 
9 m[is.na(mihow$childdob), 'childdob'] <- mihow[is.na(mihow$childdob), 'duedate']
10 
11 birth.xn <- table(mihow_birth$uid.y)
12 sing.match <- names(birth.xn[birth.xn == 1])
13 m.ids <- mihow_birth[match(sing.match, mihow_birth$uid.y), c('clientid', 'uid.y')]
14 m.ids$mid <- seq(nrow(m.ids))
15 b$m.link <- NA
16 m$b.link <- NA
17 m$b.link[match(m.ids$clientid, mihow$clientid)] <- m.ids$mid
18 b$m.link[match(m.ids$uid.y, birth$uid)] <- m.ids$mid
19 
20 b$initial <- paste(substr(b$momnamefirst, 1, 1), substr(b$momnamelast, 1, 1), sep='')
21 m$initial <- paste(substr(m$momnamefirst, 1, 1), substr(m$momnamelast, 1, 1), sep='')
22 
23 rpairs <- compare.linkage(b, m, blockfld=c(13), strcmp=c(1,2), strcmpfun=jarowinkler, identity1=b$m.link, identity2=m$b.link, exclude=c(7,9,10,11,12,13))
24 nrow(rpairs$pairs)
25 
26 rpairs1 <- emWeights(rpairs)
27 
28 summary(rpairs1)
29 edit(getPairs(rpairs1, 15, 14))
30 
31 rpairs2 <- emClassify(rpairs1, threshold.lower=5, threshold.upper=14)
32 summary(rpairs2)
33 
34 unknown <- which(rpairs2$Wdata < 14 & rpairs2$Wdata >= 5)
35 for(i in unknown) {
36   p1 <- b[rpairs2$pairs$id1[i],]
37   p2 <- m[rpairs2$pairs$id2[i],]
38   da <- abs(as.integer(p1$momage) - as.integer(p2$momage))
39   dd <- abs(as.numeric(difftime(as.Date(p1$childdob), as.Date(p2$childdob), units='days')))
40   if(!is.na(da) && da == 1) rpairs2$Wdata[i] <- rpairs2$Wdata[i] + 5
41   if(!is.na(dd) && dd > 0 && dd < 15) rpairs2$Wdata[i] <- rpairs2$Wdata[i] + 5
42 }
43 
44 known <- which(rpairs2$Wdata >= 14)
45 for(i in known) {
46   p1 <- b[rpairs2$pairs$id1[i],]
47   p2 <- m[rpairs2$pairs$id2[i],]
48   dd <- as.numeric(difftime(as.Date(p1$childdob), as.Date(p2$childdob), units='days'))
49   if(!is.na(dd) && (dd > 60 || dd < -120)) rpairs2$Wdata[i] <- 0
50 }
51 
52 matched <- rpairs2$pairs[rpairs2$Wdata > 14,]
53 for(i in as.numeric(names(Filter(isTRUE, table(matched$id2) > 1)))) {
54   lab <- matched[matched$id2 == i,]
55   rec <- birth[lab$id1,]
56   if((!is.na(rec[1,'momssn']) && rec[1,'momssn'] == rec[2,'momssn']) || (rec[1,'momdob'] == rec[2,'momdob'] && rec[1,'childdob'] == rec[2,'childdob'])) {
57     choice <- which.min(rec$livebirthstotal)
58   } else {
59     choice <- which.min(abs(as.Date(rec$childdob) - as.Date(mihow[i,'childdob'])))
60   }
61   rem.ix <- as.numeric(rownames(lab)[setdiff(seq(nrow(lab)), choice)])
62   rpairs2$Wdata[rem.ix] <- 0
63 }
64 
65 rpairs3 <- emClassify(rpairs2, threshold.lower=5, threshold.upper=14)
66 summary(rpairs3)
67 matched <- rpairs3$pairs[rpairs3$Wdata > 14,]
68 merge <- data.frame(mihow.uid=mihow$clientid[matched$id2], mihow.row=matched$id2, birth.uid=birth$uid[matched$id1], birth.row=matched$id1)
69 write.csv(merge, 'prob_match_res.csv', row.names=FALSE)
