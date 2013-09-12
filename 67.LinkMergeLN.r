     1  # "67.LinkMerge.r"
     2  # source("67.LinkMerge.r",echo=TRUE)
     3  
     4  library(gdata)
     5  load('../Data/mihow_birth.RData')
     6  merge <- read.csv('../Data/prob_match_res.csv')
     7  
     8  mihow_birth<-rename.vars(mihow_birth,"uid.y","birth.uid")
     9  birth.xn <- table(mihow_birth$birth.uid)
    10  sing.match <- names(birth.xn[birth.xn == 1])
    11  sing.match<-names(birth.xn)
    12  mb.ids <- mihow_birth[match(sing.match, mihow_birth$birth.uid), c('clientid', 'birth.uid')]
    13  mb.ids$mid <- seq(nrow(mb.ids))
    14  # mb_dp mihowBrith_deterministicProbabalistic
    15  mb_dp <- merge(merge, mb.ids, by.x="mihow.uid", by.y="clientid", all=TRUE)
    16  mb_dp<-rename.vars(bah,c("birth.uid.x","birth.uid.y"),c("birth.uid.merge","birth.uid.mb"))
    17  table(is.na(mb_dp$birth.row), is.na(mb_dp$birth.uid.merge))
    18  
    19  # only in probabilistic
    20  just.prob <- mb_dp[!is.na(mb_dp$birth.row) & is.na(mb_dp$birth.uid.mb),]
    21  # only in deterministic
    22  just.detr <- mb_dp[is.na(mb_dp$birth.row) & !is.na(mb_dp$birth.uid.mb),]
    23  # MIHOW found, agree
    24  both.agree <- mb_dp[!is.na(mb_dp$birth.row) & !is.na(mb_dp$birth.uid.merge) & mb_dp[,'birth.row'] == mb_dp[,'birth.uid.mb'],]
    25  both.agree<-both.agree[!is.na(both.agree$mihow.uid),]
    26  dim(both.agree)
    27  # MIHOW found, records disagree
    28  #both.dis <- mb_dp[!(is.na(mb_dp$birth.row) | is.na(mb_dp$mihow.uid)) & mb_dp[,'birth.row'] != mb_dp[,'birth.uid.mb'],]
    29  both.dis <- mb_dp[mb_dp[,'birth.row'] != mb_dp[,'birth.uid.mb'],]
    30  both.dis<-both.dis[!is.na(both.dis$mihow.uid),]
    31  dim(both.dis)
