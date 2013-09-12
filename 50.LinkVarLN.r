     1  # 50.LinkVar.r
     2  # Birth forms were revised in 2004.  Some variables were added some deleted or renames
     3  # source("4.LinkVar.r",echo=TRUE)
     4  #
     5  library(knitr)
     6  library(gdata)
     7  library(rms)
     8  library(gmodels)
     9  library(RMySQL)
    10  library(Hmisc)
    11  
    12  load("../Data/mihow.RData")
    13  load('../Data/birth.RData')
    14  mihow$momnamemaiden<-tolower(mihow$momnamemaiden)
    15  options('digits'=10)
    16  options('scipen'=10)
    17  options('stringAsFactors'=FALSE)
    18  
    19  k9 <- merge(mihow, birth, by.x = "key9", by.y = "key9")
    20  mihow.9<-subset(mihow,subset=!(mihow$clientid %in% k9$clientid)) 
    21  birth.9<-subset(birth,subset=!(birth$id %in% k9$id))
    22  table(k9$clientid)  #look for dups
    23  
    24  k9a <- merge(mihow.9, birth.9, by.x = "key9a", by.y = "key9a")
    25  mihow.9a<-subset(mihow.9,subset=!(mihow.9$clientid %in% k9a$clientid))
    26  birth.9a<-subset(birth.9,subset=!(birth.9$id %in% k9a$id))
    27  table(k9a$clientid)  #look for dups
    28  dim(mihow.9a)
    29  
    30  k8 <- merge(mihow.9a, birth.9a, by.x = "key8", by.y = "key8")
    31  mihow.8<-subset(mihow.9a,subset=!(mihow.9a$clientid %in% k8$clientid)) 
    32  birth.8<-subset(birth.9a,subset=!(birth.9a$id %in% k8$id))
    33  table(k8$clientid)  #look for dups
    34  dim(mihow.8)
    35  
    36  k8a <- merge(mihow.8, birth.8, by.x = "key8a", by.y = "key8a")
    37  mihow.8a<-subset(mihow.8,subset=!(mihow.8$clientid %in% k8a$clientid))
    38  birth.8a<-subset(birth.8,subset=!(birth.8$id %in% k8a$id))
    39  table(k8a$clientid)  #look for dups
    40  dim(mihow.8a)
    41  
    42  k7 <- merge(mihow.8a, birth.8a, by.x = "key7", by.y = "key7")
    43  mihow.7<-subset(mihow.8,subset=!(mihow.8$clientid %in% k7$clientid))
    44  birth.7<-subset(birth.8,subset=!(birth.8$id       %in% k7$id))
    45  table(k7$clientid)  #look for dups
    46  dim(mihow.7 )
    47  
    48  k7a <- merge(mihow.7, birth.7, by.x = "key7a", by.y = "key7a")
    49  mihow.7a<-subset(mihow.7,subset=!(mihow.7$clientid %in% k7a$clientid))
    50  birth.7a<-subset(birth.7,subset=!(birth.7$id %in% k7a$id))
    51  table(k7a$clientid)  #look for dups
    52  dim(mihow.7a)
    53  
    54  k6 <- merge(mihow.7a, birth.7a, by.x = "key6", by.y = "key6")
    55  mihow.6<-subset(mihow.7,subset=!(mihow.7$clientid %in% k6$clientid))
    56  birth.6<-subset(birth.7,subset=!(birth.7$id %in% k6$id))
    57  table(k6$clientid)  #look for dups
    58  dim(mihow.7a)
    59  
    60  k6a <- merge(mihow.6, birth.6, by.x = "key6a", by.y = "key6a")
    61  mihow.6a<-subset(mihow.6,subset=!(mihow.6$clientid %in% k6a$clientid))
    62  birth.6a<-subset(birth.6,subset=!(birth.6$id %in% k6a$id))
    63  table(k6a$clientid)  #look for dups
    64  dim(mihow.6a)
    65  
    66  #
    67  # two matches for "232-09"  -> twins
    68  #
    69  
    70  k5 <- merge(mihow.6a, birth.6a, by.x = "key5", by.y = "key5")
    71  mihow.5<-subset(mihow.6,subset=!(mihow.6$clientid %in% k5$clientid))
    72  birth.5<-subset(birth.6,subset=!(birth.6$id %in% k5$id))
    73  table(k5$clientid)  #look for dups
    74  dim(mihow.5)
    75  
    76  k5a <- merge(mihow.5, birth.5, by.x = "key5a", by.y = "key5a")
    77  mihow.5a<-subset(mihow.5,subset=!(mihow.5$clientid %in% k5a$clientid))
    78  birth.5a<-subset(birth.5,subset=!(birth.5$id %in% k5a$id))
    79  table(k5a$clientid)  #look for dups
    80  dim(mihow.5a)
    81  
    82  #
    83  # two matches for "001-09"  -> twins
    84  #
    85  
    86  k4 <- merge(mihow.5a, birth.5a, by.x = "key4", by.y = "key4")
    87  mihow.4<-subset(mihow.5,subset=!(mihow.5$clientid %in% k4$clientid))
    88  birth.4<-subset(birth.5,subset=!(birth.5$id %in% k4$id))
    89  #k4[k4$clientid== "520-09",]
    90  table(k4$clientid)  #look for dups
    91  dim(mihow.4a)
    92  #
    93  # two matches for "001-09"  -> twins
    94  #
    95  k3 <- merge(mihow.4, birth.4, by.x = "key3", by.y = "key3")  #155  76
    96  #
    97  # Calculate days between due_date or childdob.x and childdob.y
    98  #
    99  k3$days<-abs(difftime(as.Date(k3$est_childdob),as.Date(k3$childdob.y),units="days"))
   100  #
   101  # remove records where est_childdob and childdob.y are not within 40 days
   102  #
   103  k3$near<-ifelse(k3$days > 40,0,1)
   104  k3<-subset(k3,k3$near == 1)
   105  
   106  mihow.3<-subset(mihow.4,subset=!(mihow.4$clientid %in% k3$clientid))
   107  birth.3<-subset(birth.4,subset=!(birth.4$id %in% k3$id))
   108  table(k3$clientid)  #look for dups
   109  
   110  #paste(trim(birth$momnamelast),trim(birth$momnamefirst),birth$momdob
   111  look<-function(df,who){df[df$clientid== who,c("near","days","childidnum","due_date","childdob.x","childdob.y","livebirthstotal","momage.x","momage.y","momdob.x","momdob.y","momwhite.x","momwhite.y")]}
   112  #
   113  # look will return 1 record for close dates, 2 for twins, else none
   114  #
   115  # twins 616-09  & 151-09
   116  # VISUALLY INSPECT DUPLICATE
   117  look(k3,"151-09") # "151-09"  twins                             ##childdob 1 month##
   118  look(k3,"616-09") # "616-09" 0180000023480 2010-01-21 <NA> 2010-01-11 1 21 21  #twins ##due_date & childob.y close##
   119  # "616-09" 0180000023481 2010-01-21 <NA> 2010-01-11 2 21 21  #twins ##due_date & childob.y close##
   120  look(k3,"617-09") # "617-09" #0244000031637 2010-06-30 <NA> 2010-05-23 3 24 25       ##due_date & childob.y close##
   121  look(k3,"622-09") # "622-09"  #0244000029978 2010-01-03 <NA> 2010-01-05 7 29 30      ##due_date & childob.y close##
   122  look(k3,"650-09") # "650-09" #0224000006901 2009-10-15 <NA> 2009-10-15 4 26 27       ##due_date & childob.y equal##
   123  look(k3,"665-09") # "665-09" # 0244000030395 2010-02-10 <NA> 2010-02-04 4 32 32      ##due_date & childob.y close##
   124  
   125  
   126  k2 <- merge(mihow.3, birth.3, by.x = "key2", by.y = "key2")
   127  k2$days<-abs(difftime(as.Date(k2$est_childdob),as.Date(k2$childdob.y),units="days"))
   128  k2$near<-ifelse(k2$days > 40,0,1)
   129  k2<-subset(k2,k2$near == 1)
   130  table(k2$clientid)  #look for dups
   131  #
   132  # find multiple matches and VISUALLY INSPECT
   133  #
   134  look(k2, "661-09")   ##twins##
   135  look(k2, "654-07")   ##delete childidnum = '0181000020974' ##
   136  look(k2, "499-09")   ##delete childidnum = '0181000029228' ##
   137  look(k2, "388-07")   ##delete childidnum = '0180000014996' ##
   138  look(k2, "224-08")   ##delete childidnum = '0181000022054' ##
   139  look(k2, "119-07")   ##delete childidnum = '0181000016614' ##
   140  #
   141  # keep closest childdob
   142  #
   143  k2<-subset(k2,subset=childidnum %nin% c('0181000020974','0181000029228','0180000014996','0181000022054','0181000016614'))
   144  mihow.2<-subset(mihow.3,subset=!(mihow.3$clientid %in% k2$clientid))
   145  birth.2<-subset(birth.3,subset=!(birth.3$id %in% k2$id))
   146  
   147  #childdob.x close to childdob.y
   148  
   149  k1 <- merge(mihow.2, birth.2, by.x = "key1", by.y = "key1")
   150  dim(k1)
   151  k1$days<-abs(difftime(as.Date(k1$est_childdob),as.Date(k1$childdob.y),units="days"))
   152  k1$near<-ifelse(k1$days > 10,0,1)
   153  k1<-subset(k1,k1$near == 1)
   154  dim(k1)
   155  # year(childdob.y) >= year(momdob.y) + momage.x 
   156  k1$est_momage <-abs(as.numeric(substr(k1$childdob.y,1,4))-as.numeric(substr(k1$momdob.y,1,4))- k1$momage.x )
   157  k1<-subset(k1,k1$est_momage < 2)
   158  table(k1$clientid)  #look for dups
   159  #VISUALLY INSPECT RECORDS BASED ON CLIENTID
   160  look1<-function(df,who){df[df$clientid== who,c("est_momage","days","childidnum","due_date","childdob.x","childdob.y","livebirthstotal","momage.x","momage.y","momdob.x","momdob.y","momwhite.x","momwhite.y")]}
   161  look1(k1,"025-07")  
   162  look1(k1,"185-07")  
   163  look1(k1,"198-09")  
   164  look1(k1,"200-07")  
   165  look1(k1,"207-07")  
   166  
   167  look1(k1,"244-09")  
   168  look1(k1,"045-09")  
   169  look1(k1,"315-09")  
   170  look1(k1,"319-07")  
   171  look1(k1,"351-09")
   172  
   173  look1(k1,"385-07")
   174  look1(k1,"411-09")
   175  look1(k1,"422-07")
   176  look1(k1,"436-07")
   177  look1(k1,"438-07")
   178  
   179  look1(k1,"440-07")
   180  look1(k1,"485-07")
   181  look1(k1,"568-07")
   182  look1(k1,"604-07")
   183  look1(k1,"696-07")
   184  #DELETE SUSPECT MATCHES
   185  k1<-subset(k1,subset=childidnum %nin% c("0181000026315","0224000006497","0504000003200",
   186  "0244000027426","0181000018431","0181000018584","0180000015265","0181000018653","0181000019061",
   187  "0181000019389","0224000004760","0181000019242","0187000002638","0180000016945"))
   188  
   189  mihow.1<-subset(mihow.2,subset=!(mihow.2$clientid %in% k1$clientid))
   190  birth.1<-subset(birth.2,subset=!(birth.2$id %in% k1$id))
   191  dim(mihow.1);dim(mihow.2);dim(mihow.1)
   192  
   193  gg<-rbind( as.matrix(k9$clientid), 
   194             as.matrix(k8$clientid), 
   195             as.matrix(k7$clientid),
   196             as.matrix(k6$clientid),
   197             as.matrix(k5$clientid),
   198             as.matrix(k4$clientid),
   199             as.matrix(k3$clientid),
   200             as.matrix(k2$clientid),
   201             as.matrix(k1$clientid))
   202  #CLEAN UP VARIABLES
   203  k9.a<-remove.vars(k9,c("key9", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
   204  "key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
   205  "key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
   206  "key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
   207  k9.a$key<-'9'
   208  k9.aa<-remove.vars(k9a,c("key9a", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
   209  "key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
   210  "key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
   211  "key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
   212  k9.aa$key<-'9a'
   213  
   214  k8.a<-remove.vars(k8,c("key8", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
   215  "key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
   216  "key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
   217  "key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
   218  k8.a$key<-'8'
   219  #k8.aa<-remove.vars(k8a,c("key8a", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
   220  #"key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
   221  #"key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
   222  #"key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
   223  #k8.aa$key<-'8a'
   224  
   225  k7.a<-remove.vars(k7,c("key7", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
   226  "key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
   227  "key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
   228  "key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
   229  k7.a$key<-'7'
   230  k7.aa<-remove.vars(k7a,c("key7a", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
   231  "key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
   232  "key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
   233  "key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
   234  k7.aa$key<-'7a'
   235  dim(k7.a);dim(k7.aa)
   236  
   237  k6.a<-remove.vars(k6,c("key6", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
   238  "key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
   239  "key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
   240  "key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
   241  k6.a$key<-'6'
   242  k6.aa<-remove.vars(k6a,c("key6a", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
   243  "key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
   244  "key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
   245  "key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
   246  k6.aa$key<-'6a'
   247  dim(k6.a);dim(k6.aa)
   248  
   249  k5.a<-remove.vars(k5,c("key5", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
   250  "key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
   251  "key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
   252  "key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
   253  k5.a$key<-'5'
   254  k5.aa<-remove.vars(k5a,c("key5a", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
   255  "key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
   256  "key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
   257  "key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
   258  k5.aa$key<-'5a'
   259  dim(k5.a);dim(k5.aa)
   260  
   261  k4.a<-remove.vars(k4,c("key4", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
   262  "key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
   263  "key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
   264  "key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y"))
   265  k4.a$key<-'4'
   266  dim(k4.a)
   267  
   268  k3.a<-remove.vars(k3,c("key3", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
   269  "key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
   270  "key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
   271  "key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y","near",'days'))
   272  k3.a$key<-'3'
   273  dim(k3.a)
   274  
   275  k2.a<-remove.vars(k2,c("key2", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
   276  "key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
   277  "key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
   278  "key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y","near",'days'))
   279  k2.a$key<-'2'
   280  dim(k2.a)
   281  
   282  k1.a<-remove.vars(k1,c("key1", "key1.x", "key2.x", "key3.x", "key4.x", "key5.x", "key6.x",
   283  "key7.x", "key8.x", "key9.x", "key1.y", "key2.y", "key3.y", "key4.y", "key5.y", "key6.y", "key7.y",
   284  "key8.y", "key9.y","key4a.x","key4a.y","key5a.x","key5a.y" ,
   285  "key6a.x","key6a.y","key7a.x","key7a.y","key8a.x","key8a.y","key9a.x","key9a.y","near",'days','est_momage'))
   286  k1.a$key<-'1'
   287  dim(k1.a)
   288  #
   289  # concatenate rows from k9 to k1 merges
   290  
   291  gg<-rbind(k9.a,k9.aa,k8.a,k7.a,k7.aa,k6.a,k6.aa,k5.a,k5.aa,k4.a,k3.a,k2.a,k1.a)
   292  #
   293  # calculate total number of rows of matched records
   294  # 
   295  nrow(k9.a) + nrow(k9.aa) +nrow(k8.a) +nrow(k7.a) +nrow(k7.aa) +nrow(k6.a) + nrow(k6.aa) + nrow(k5.a) + nrow(k5.aa) + 
   296  nrow(k4.a) + nrow(k3.a) + nrow(k2.a) + nrow(k1.a)
   297  
   298  # select rows saved in gg
   299  #
   300  mihow_birth<-subset(gg,select=sort(names(gg)))
   301  
   302  save(mihow_birth,file='../Data/mihow_birth.RData')
   303  
