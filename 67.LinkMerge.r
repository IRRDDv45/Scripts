# "67.LinkMerge.r"
# source("67.LinkMerge.r",echo=TRUE)

library(gdata)
load('../Data/mihow_birth.RData')
merge <- read.csv('../Data/prob_match_res.csv')

mihow_birth<-rename.vars(mihow_birth,"uid.y","birth.uid")
birth.xn <- table(mihow_birth$birth.uid)
sing.match <- names(birth.xn[birth.xn == 1])
sing.match<-names(birth.xn)
mb.ids <- mihow_birth[match(sing.match, mihow_birth$birth.uid), c('clientid', 'birth.uid')]
mb.ids$mid <- seq(nrow(mb.ids))
# mb_dp mihowBrith_deterministicProbabalistic
mb_dp <- merge(merge, mb.ids, by.x="mihow.uid", by.y="clientid", all=TRUE)
mb_dp<-rename.vars(bah,c("birth.uid.x","birth.uid.y"),c("birth.uid.merge","birth.uid.mb"))
table(is.na(mb_dp$birth.row), is.na(mb_dp$birth.uid.merge))

# only in probabilistic
just.prob <- mb_dp[!is.na(mb_dp$birth.row) & is.na(mb_dp$birth.uid.mb),]
# only in deterministic
just.detr <- mb_dp[is.na(mb_dp$birth.row) & !is.na(mb_dp$birth.uid.mb),]
# MIHOW found, agree
both.agree <- mb_dp[!is.na(mb_dp$birth.row) & !is.na(mb_dp$birth.uid.merge) & mb_dp[,'birth.row'] == mb_dp[,'birth.uid.mb'],]
both.agree<-both.agree[!is.na(both.agree$mihow.uid),]
dim(both.agree)
# MIHOW found, records disagree
#both.dis <- mb_dp[!(is.na(mb_dp$birth.row) | is.na(mb_dp$mihow.uid)) & mb_dp[,'birth.row'] != mb_dp[,'birth.uid.mb'],]
both.dis <- mb_dp[mb_dp[,'birth.row'] != mb_dp[,'birth.uid.mb'],]
both.dis<-both.dis[!is.na(both.dis$mihow.uid),]
dim(both.dis)
