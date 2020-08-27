'Q212' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2012.Q2",header=TRUE)
'Q312' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2012.Q3",header=TRUE)
'Q412' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2012.Q4",header=TRUE)
'Q113' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2013.Q1",header=TRUE)
'Q213' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2013.Q2",header=TRUE)
'Q313' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2013.Q3",header=TRUE)
'Q413' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2013.Q4",header=TRUE)
'Q114' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2014.Q1",header=TRUE)
'Q214' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2014.Q2",header=TRUE)
'Q314' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2014.Q3",header=TRUE)
'Q414' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2014.Q4",header=TRUE)

'Q115' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2015.Q1",header=TRUE)
'Q215' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2015.Q2",header=TRUE)
'Q315' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2015.Q3",header=TRUE)
'Q415' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2015.Q4",header=TRUE)
'Q116' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2016.Q1",header=TRUE)
'Q216' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2016.Q2",header=TRUE)
'Q316' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2016.Q3",header=TRUE)
'Q416' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2016.Q4",header=TRUE)
'Q117' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2017.Q1",header=TRUE)
'Q217' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2017.Q2",header=TRUE)
'Q317' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2017.Q3",header=TRUE)
'Q417' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2017.Q4",header=TRUE)
'Q118' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2018.Q1",header=TRUE)
'Q218' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2018.Q2",header=TRUE)
'Q318' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/Year.Quarter/2018.Q3",header=TRUE)


times = c("Q212","Q312","Q412","Q113","Q213","Q313","Q413","Q114","Q214","Q314","Q414","Q115","Q215","Q315","Q415","Q116","Q216","Q316","Q416","Q117","Q217","Q317","Q417","Q118","Q218","Q318")


# Getting all combinations of possible routes flown in the entire year
list_of_routes=data.frame(mkts)
list_of_routes$orig=""
list_of_routes$des=""
for (i in 1:length(list_of_routes$list_of_routes)) {
  list_of_routes$orig[i]=substr(list_of_routes$list_of_routes[i],1,3)
  list_of_routes$des[i]=substr(list_of_routes$list_of_routes[i],5,7)
}
list_of_routes=list_of_routes[,-c(1)]

list_of_routes_orig=as.vector(list_of_routes$orig)
list_of_routes_des=as.vector(list_of_routes$des)
list_of_routes=c()
list_of_routes=append(list_of_routes,list_of_routes_orig)
list_of_routes=append(list_of_routes,list_of_routes_des)

pairs_matrix=t(combn(list_of_routes, 2))
pairs_df=data.frame(pairs_matrix)
rm(pairs_matrix)
pairs_df=subset(pairs_df, !duplicated(subset(pairs_df, select=c(X1, X2))))
pairs_df=pairs_df[!(pairs_df$X1==pairs_df$X2),]


v=151527
for(i in 1:length(times)){
  file = times[i]
  abc = get(file)
  mkts = unique(Q113$AIRPORT_GROUP)
  for(n in 1:length(mkts)){
    origin = as.character(substr(mkts[n],1,3))
    dest = as.character(substr(mkts[n],5,7))
    if(dest<origin){
      c = origin
      origin = dest
      dest = c
      
      ORIGIN=origin
      DEST=dest
    }
    y = filter(Q113,ORIGIN==origin & DEST==dest | ORIGIN==dest&DEST==origin)
    carriers = unique(y$TK_CARRIER_GROUP)
    for(j in 1:length(carriers)){
      z = filter(y, TK_CARRIER_GROUP==carriers[j])
      PAX = sum(z$PASSENGERS)
      fare = sum(z$MARKET_FARE*z$PASSENGERS)/PAX
      if(PAX<200) next
      v=v+1
      farematrix2[v, 1]=fare
      farematrix2[v, 2]=origin
      farematrix2[v, 3]=dest
      farematrix2[v, 4]=file
      farematrix2[v,5]=mean(y$MARKET_MILES_FLOWN)
      farematrix2[v,6]=as.character(carriers[j])
      farematrix2[v,7]=PAX
    }
  }
}

##############################################################################################################################################
ORIGIN=c("SEA")
DEST=c("HNL")
Q113$ORIGIN="HNL"
Q113$DEST="BLI"
Q113TEST=data.frame(Q113)

Q113$ORIGIN=substr(Q113$AIRPORT_GROUP,1,3)
Q113$DEST=substr(Q113$AIRPORT_GROUP,5,7)

library(dplyr)

v=152628

mkts = unique(Q113$AIRPORT_GROUP)
for(n in 1:length(mkts)){
  origin = as.character(substr(mkts[n],1,3))
  dest = as.character(substr(mkts[n],5,7))
  if(dest<origin){
    c = origin
    origin = dest
    dest = c
    
    ORIGIN=origin
    DEST=dest
  }
  y = filter(Q113,ORIGIN==origin & DEST==dest | ORIGIN==dest&DEST==origin)
  carriers = unique(y$TK_CARRIER_GROUP)
  for(j in 1:length(carriers)){
    z = filter(y, TK_CARRIER_GROUP==carriers[j])
    PAX = sum(z$PASSENGERS)
    fare = sum(z$MARKET_FARE*z$PASSENGERS)/PAX
    if(PAX<200) next
    v=v+1
    farematrix2[v, 1]=fare
    farematrix2[v, 2]=origin
    farematrix2[v, 3]=dest
    farematrix2[v, 4]=file
    farematrix2[v,5]=mean(y$MARKET_MILES_FLOWN)
    farematrix2[v,6]=as.character(carriers[j])
    farematrix2[v,7]=PAX
  }
}



## Below this is Greg's raw stuff
##############################################################################################################################################

CPN <- read.csv("C:/Users/Greg/OneDrive/Stanford/2018/Thesis/CouponsQ2.csv")
TKT = read.csv("C:/Users/Greg/OneDrive/Stanford/2018/Thesis/TicketQ2.csv")
MKT = read.csv("C:/Users/Greg/OneDrive/Stanford/2018/Thesis/MarketQ2.csv")
carriers = c("AA","DL","UA","US","AS","VX","WN","B6","F9","NK")

Q212 = read.csv("E:/New Datasets/2012Q2.csv")
Q212 = filter(Q212,MARKET_COUPONS==1 & MARKET_FARE>5)
abcd = setDT(Q212)
routelist = unique(Q212$AIRPORT_GROUP)

new = matrix()

Q115 = read.csv("E:/New Datasets/2015Q1.csv")
Q115 = filter(Q115,MARKET_COUPONS==1 & MARKET_FARE>5)
Q115routes = unique(Q115$AIRPORT_GROUP)

Q212 = read.csv("E:/Thesis/Dataset V2/2012Q2.csv")
Q212 = filter(Q212,MARKET_COUPONS==1 & MARKET_FARE>5)
Q312 = read.csv("E:/Thesis/Dataset V2/2012Q3.csv")
Q312 = filter(Q312,MARKET_COUPONS==1 & MARKET_FARE>5)
Q412 = read.csv("E:/Thesis/Dataset V2/2012Q4.csv")
Q412 = filter(Q412,MARKET_COUPONS==1 & MARKET_FARE>5)
Q113 = read.csv("E:/Thesis/Dataset V2/2013Q1.csv")
Q113 = filter(Q113,MARKET_COUPONS==1 & MARKET_FARE>5)
Q213 = read.csv("E:/Thesis/Dataset V2/2013Q2.csv")
Q213 = filter(Q213,MARKET_COUPONS==1 & MARKET_FARE>5)
Q313 = read.csv("E:/Thesis/Dataset V2/2013Q3.csv")
Q313 = filter(Q313,MARKET_COUPONS==1 & MARKET_FARE>5)
Q413 = read.csv("E:/Thesis/Dataset V2/2013Q4.csv")
Q413 = filter(Q413,MARKET_COUPONS==1 & MARKET_FARE>5)

Q312$MARKET_FARE=log(Q312$MARKET_FARE)
Q412$MARKET_FARE=log(Q412$MARKET_FARE)
Q113$MARKET_FARE=log(Q113$MARKET_FARE)
Q213$MARKET_FARE=log(Q213$MARKET_FARE)
Q313$MARKET_FARE=log(Q313$MARKET_FARE)
Q413$MARKET_FARE=log(Q413$MARKET_FARE)


Q114 = read.csv("E:/Thesis/Dataset V2/2014Q1.csv")
Q114 = filter(Q114,MARKET_COUPONS==1 & MARKET_FARE>5)
Q214 = read.csv("E:/Thesis/Dataset V2/2014Q2.csv")
Q214 = filter(Q214,MARKET_COUPONS==1 & MARKET_FARE>5)
Q314 = read.csv("E:/Thesis/Dataset V2/2014Q3.csv")
Q314 = filter(Q314,MARKET_COUPONS==1 & MARKET_FARE>5)
Q414 = read.csv("E:/Thesis/Dataset V2/2014Q4.csv")
Q414 = filter(Q414,MARKET_COUPONS==1 & MARKET_FARE>5)
Q115 = read.csv("E:/Thesis/Dataset V2/2015Q1.csv")
Q115 = filter(Q115,MARKET_COUPONS==1 & MARKET_FARE>5)
Q215 = read.csv("E:/Thesis/Dataset V2/2015Q2.csv")
Q215 = filter(Q215,MARKET_COUPONS==1 & MARKET_FARE>5)
Q315 = read.csv("E:/Thesis/Dataset V2/2015Q3.csv")
Q315 = filter(Q315,MARKET_COUPONS==1 & MARKET_FARE>5)
Q415 = read.csv("E:/Thesis/Dataset V2/2015Q4.csv")
Q415 = filter(Q415,MARKET_COUPONS==1 & MARKET_FARE>5)

rm(Q116)
rm(Q216)
rm(Q316)
rm(Q416)
rm(Q117)
rm(Q217)
rm(Q317)
rm(Q417)

Q118 = read.csv("E:/Thesis/Dataset V2/2018Q1.csv")
Q118 = filter(Q118,MARKET_COUPONS==1 & MARKET_FARE>5)
Q218 = read.csv("E:/Thesis/Dataset V2/2018Q2.csv")
Q218 = filter(Q218,MARKET_COUPONS==1 & MARKET_FARE>5)
Q318 = read.csv("E:/Thesis/Dataset V2/2018Q3.csv")
Q318 = filter(Q218,MARKET_COUPONS==1 & MARKET_FARE>5)


Q118$MARKET_FARE=log(Q118$MARKET_FARE)
Q218$MARKET_FARE=log(Q218$MARKET_FARE)
Q318$MARKET_FARE=log(Q318$MARKET_FARE)

Q316$MARKET_FARE=log(Q316$MARKET_FARE)
Q416$MARKET_FARE=log(Q416$MARKET_FARE)
Q117$MARKET_FARE=log(Q117$MARKET_FARE)
Q217$MARKET_FARE=log(Q217$MARKET_FARE)
Q317$MARKET_FARE=log(Q317$MARKET_FARE)
Q417$MARKET_FARE=log(Q417$MARKET_FARE)



farematrix2=matrix(,ncol=7,nrow=300000)


Q212 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2012.Q2",header=TRUE)
Q312 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2012.Q3",header=TRUE)
Q412 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2012.Q4",header=TRUE)
Q113 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2013.Q1",header=TRUE)
Q213 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2013.Q2",header=TRUE)
Q313 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2013.Q3",header=TRUE)
Q413 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2013.Q4",header=TRUE)
Q114 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2014.Q1",header=TRUE)
Q214 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2014.Q2",header=TRUE)
Q314 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2014.Q3",header=TRUE)
Q414<- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2014.Q4",header=TRUE)
Q115 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2015.Q1",header=TRUE)
Q215 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2015.Q2",header=TRUE)
Q315 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2015.Q3",header=TRUE)
Q415 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2015.Q4",header=TRUE)
Q116 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2016.Q1",header=TRUE)
Q216 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2016.Q2",header=TRUE)
Q316 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2016.Q3",header=TRUE)
Q416 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2016.Q4",header=TRUE)
Q117 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2017.Q1",header=TRUE)
Q217 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2017.Q2",header=TRUE)
Q317<- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2017.Q3",header=TRUE)
Q417 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2017.Q4",header=TRUE)
Q118 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2018.Q1",header=TRUE)
Q218<- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2018.Q2",header=TRUE)
Q318 <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2018.Q3",header=TRUE)

v=151486
for(i in 26:26){
  file = times[i]
  abc = get(file)
  mkts = unique(abc$AIRPORT_GROUP)
  for(n in 1:length(mkts)){
    origin = as.character(substr(mkts[n],1,3))
    dest = as.character(substr(mkts[n],5,7))
    if(dest<origin){
      c = origin
      origin = dest
      dest = c
    }
    y = filter(abc,ORIGIN==origin & DEST==dest | ORIGIN==dest&DEST==origin)
    carriers = unique(y$TICKET_CARRIER)
    for(j in 1:length(carriers)){
      z = filter(y, TICKET_CARRIER==carriers[j])
      PAX = sum(z$PASSENGERS)
      fare = sum(z$MARKET_FARE*z$PASSENGERS)/PAX
      if(PAX<200) next
      v=v+1
      farematrix2[v, 1]=fare
      farematrix2[v, 2]=origin
      farematrix2[v, 3]=dest
      farematrix2[v, 4]=file
      farematrix2[v,5]=mean(y$MARKET_MILES_FLOWN)
      farematrix2[v,6]=as.character(carriers[j])
      farematrix2[v,7]=PAX
    }
  }
}



times = c("Q212","Q312","Q412","Q113","Q213","Q313","Q413","Q114","Q214","Q314","Q414","Q115","Q215","Q315","Q415","Q116","Q216","Q316","Q416","Q117","Q217","Q317","Q417","Q118","Q218","Q318")

## THIS CREATES LIST OF ALL PAIRINGS
for(i in 16:24){
  file = times[i]
  abc = get(file)
  abc = unique(abc$AIRPORT_GROUP)
  if(i==1) ctrlrts = abc
  else
    ctrlrts = union(ctrlrts,abc)
}
15812



## THIS CREATES LIST OF AIRPORTS THAT HAVE SOME ULCC SERVICE Q212-Q218
for(i in 1:25){
  file = times[i]
  abc = filter(get(file),TICKET_CARRIER=="F9"|TICKET_CARRIER=="NK"|TICKET_CARRIER=="G4")
  abc = unique(abc$AIRPORT_GROUP)
  if(i==1) airports = abc
  else
    airports = union(airports,abc)
}

mktslist1 = list()
for(k in 2:16){
  q = k%%4
  if(q==0) q=4
  yr = 11 + ceiling(k/4)
  title = paste("Q",q,yr,sep="")
  NSMKT = get(title)
  i = k-2
  mktslist1[[10*i+1]] = filter(NSMKT,TICKET_CARRIER=="AA")
  mktslist1[[10*i+2]] = filter(NSMKT,TICKET_CARRIER=="DL")
  mktslist1[[10*i+3]] = filter(NSMKT,TICKET_CARRIER=="UA")
  mktslist1[[10*i+4]] = filter(NSMKT,TICKET_CARRIER=="US")
  mktslist1[[10*i+5]] = filter(NSMKT,TICKET_CARRIER=="AS")
  mktslist1[[10*i+6]] = filter(NSMKT,TICKET_CARRIER=="VX")
  mktslist1[[10*i+7]] = filter(NSMKT,TICKET_CARRIER=="WN")
  mktslist1[[10*i+8]] = filter(NSMKT,TICKET_CARRIER=="B6")
  mktslist1[[10*i+9]] = filter(NSMKT,TICKET_CARRIER=="F9")
  mktslist1[[10*i+10]] = filter(NSMKT,TICKET_CARRIER=="NK")
}

rm(Q212)
rm(Q312)
rm(Q412)
rm(Q113)
rm(Q213)
rm(Q313)
rm(Q413)
rm(Q114)
rm(Q214)
rm(Q314)
rm(Q414)
rm(Q115)
rm(Q215)



Q116 = read.csv("E:/Thesis/Dataset V2/2016Q1.csv")
Q116 = filter(Q116,MARKET_COUPONS==1 & MARKET_FARE>5)

Q216 = read.csv("E:/Thesis/Dataset V2/2016Q2.csv")
Q216 = filter(Q216,MARKET_COUPONS==1 & MARKET_FARE>5)

Q316 = read.csv("E:/Thesis/Dataset V2/2016Q3.csv")
Q316 = filter(Q316,MARKET_COUPONS==1 & MARKET_FARE>5)

Q416 = read.csv("E:/Thesis/Dataset V2/2016Q4.csv")
Q416 = filter(Q416,MARKET_COUPONS==1 & MARKET_FARE>5)

Q117 = read.csv("E:/Thesis/Dataset V2/2017Q1.csv")
Q117 = filter(Q117,MARKET_COUPONS==1 & MARKET_FARE>5)

Q217 = read.csv("E:/Thesis/Dataset V2/2017Q2.csv")
Q217 = filter(Q217,MARKET_COUPONS==1 & MARKET_FARE>5)

Q317 = read.csv("E:/Thesis/Dataset V2/2017Q3.csv")
Q317 = filter(Q317,MARKET_COUPONS==1 & MARKET_FARE>5)

Q417 = read.csv("E:/Thesis/Dataset V2/2017Q4.csv")
Q417 = filter(Q417,MARKET_COUPONS==1 & MARKET_FARE>5)

Q118 = read.csv("E:/Thesis/Dataset V2/2018Q1.csv")
Q118 = filter(Q118,MARKET_COUPONS==1 & MARKET_FARE>5)

Q218 = read.csv("E:/Thesis/Dataset V2/2018Q2.csv")
Q218 = filter(Q218,MARKET_COUPONS==1 & MARKET_FARE>5)

mktslist = list()

for(k in 1:10){
  q = k%%4
  if(q==0) q=4
  yr = 15 + ceiling(k/4)
  title = paste("Q",q,yr,sep="")
  i = k-1
  mktslist[[10*i+1]] = filter(get(title),TICKET_CARRIER=="AA")
  mktslist[[10*i+2]] = filter(get(title),TICKET_CARRIER=="DL")
  mktslist[[10*i+3]] = filter(get(title),TICKET_CARRIER=="UA")
  mktslist[[10*i+4]] = filter(get(title),TICKET_CARRIER=="US")
  mktslist[[10*i+5]] = filter(get(title),TICKET_CARRIER=="AS")
  mktslist[[10*i+6]] = filter(get(title),TICKET_CARRIER=="VX")
  mktslist[[10*i+7]] = filter(get(title),TICKET_CARRIER=="WN")
  mktslist[[10*i+8]] = filter(get(title),TICKET_CARRIER=="B6")
  mktslist[[10*i+9]] = filter(get(title),TICKET_CARRIER=="F9")
  mktslist[[10*i+10]] = filter(get(title),TICKET_CARRIER=="NK")
}

rm(Q116)
rm(Q216)
rm(Q316)
rm(Q416)
rm(Q117)
rm(Q217)
rm(Q317)
rm(Q417)
rm(Q118)
rm(Q218)


allmkts = append(mktslist1,mktslist)
rm(mktslist1)
rm(mktslist)


route = read.csv("E:/F9 Routes Only.csv") #this is NK!
route_1 = route[1:90,]
results = list() #list of all the matrices of all routes

baseline = dim(route_1)[1]*4

regression = matrix(1:baseline,nrow=baseline/4)
z=0
for (i in 1:dim(route_1)[1]){
  #  market = route_1[i,1]
  #for (i in 1:20){
  origin = as.character(route_1[i,2])
  dest = as.character(route_1[i,3])
  qtr = as.numeric(as.character((route_1[i,4])))
  yr = as.numeric(as.character((route_1[i,5])))
  c1 = route_1[i,6]
  c2 = route_1[i,7]
  c3 = route_1[i,8]
  c4 = route_1[i,9]
  cmat = matrix(1:70,nrow=10)
  rownames(cmat) = carriers
  for(j in -2:4){
    q = (qtr+j)%%4
    if (q==0) q=4
    
    if(qtr+j>4) {
      year = yr+1
    } else if (qtr+j<=0) {
      year = yr-1
    } else year = yr
    year = year-12 #change to 13 later?
    index = year*4+q
    
    for (k in 1:10){
      NSMKT  = allmkts[[10*(index-1)+k]]
      y = filter(NSMKT,ORIGIN==origin & DEST==dest | ORIGIN==dest&DEST==origin)
      fare = round(sum(y$MARKET_FARE*y$PASSENGERS)/sum(y$PASSENGERS),0)
      cmat[k,j+3] = fare
      if(is.nan(fare)==FALSE){
        z=z+1
        regression[z,1] = fare
        if(j<0)regression[z,3] = 0
        else regression[z,3] = 1
      }
    }
  }
  results[[i]] = cmat
}

summary(results[[1]])


## set random routes to lookat
set.seed(100)
ctrlgrp = sample(Q115_nonULCC,90)













origin = "ORD"
dest = "SEA"
for(j in 2:11){
  MKT = get(noquote(cmat[j,1]))
  MKTTOTAL = filter(MKT,ORIGIN==origin & DEST==dest | ORIGIN==dest&DEST==origin)
  NSMKT = filter(MKTTOTAL, MARKET_COUPONS==1)
  NSMKT_UA = filter(NSMKT,TICKET_CARRIER=="UA")
  NSMKT_F9 = filter(NSMKT,TICKET_CARRIER=="F9")
  NSMKT_WN = filter(NSMKT,TICKET_CARRIER=="WN")
  NSMKT_DL = filter(NSMKT,TICKET_CARRIER=="DL")
  NSMKT_NK = filter(NSMKT,TICKET_CARRIER=="NK")
  NSMKT_AA = filter(NSMKT,TICKET_CARRIER=="AA")
  y = NSMKT_AA
  cmat[j,2] = round(sum(y$MARKET_FARE*y$PASSENGERS)/sum(y$PASSENGERS),0)
  y = NSMKT_UA
  cmat[j,3] = round(sum(y$MARKET_FARE*y$PASSENGERS)/sum(y$PASSENGERS),0)
  y = NSMKT_DL
  cmat[j,4]= round(sum(y$MARKET_FARE*y$PASSENGERS)/sum(y$PASSENGERS),0)
  y = NSMKT_WN
  cmat[j,5] = round(sum(y$MARKET_FARE*y$PASSENGERS)/sum(y$PASSENGERS),0)
  y = NSMKT_NK
  cmat[j,6] = round(sum(y$MARKET_FARE*y$PASSENGERS)/sum(y$PASSENGERS),0)
  y = NSMKT_F9
  cmat[j,7] = round(sum(y$MARKET_FARE*y$PASSENGERS)/sum(y$PASSENGERS),0)
}
cmat


MKT = Q216
MKTTOTAL = filter(MKT,ORIGIN==origin & DEST==dest | ORIGIN==dest&DEST==origin)
NSMKT = filter(MKTTOTAL, MARKET_COUPONS==1)
NSMKT_UA = filter(NSMKT,TICKET_CARRIER=="UA")
NSMKT_F9 = filter(NSMKT,TICKET_CARRIER=="F9")
NSMKT_WN = filter(NSMKT,TICKET_CARRIER=="WN")
NSMKT_DL = filter(NSMKT,TICKET_CARRIER=="DL")
NSMKT_AS = filter(NSMKT,TICKET_CARRIER=="AS")
NSMKT_AA = filter(NSMKT,TICKET_CARRIER=="AA")



y = NSMKT_WN
sum(y$MARKET_FARE*y$PASSENGERS)/sum(y$PASSENGERS)
y = NSMKT_AA
sum(y$MARKET_FARE*y$PASSENGERS)/sum(y$PASSENGERS)
y = NSMKT_DL
sum(y$MARKET_FARE*y$PASSENGERS)/sum(y$PASSENGERS)
y = NSMKT_F9
sum(y$MARKET_FARE*y$PASSENGERS)/sum(y$PASSENGERS)





x=201822864468
a = filter(CPN, ITIN_ID==x)
b = filter(MKT, ITIN_ID==x)
c = filter(TKT, ITIN_ID==x)

NS.






TKTCHSORD = filter(TKT,ORIGIN=="CHS"  | ORIGIN=="ORD")



cmat = matrix(1:70,nrow=11)
cmat[1,1] = 0
cmat[1,2] = "AA"
cmat[1,3] = "UA"
cmat[1,4] = "DL"
cmat[1,5] = "WN"
cmat[1,6] = "AS"
cmat[1,7] = "F9"
cmat[2,1] = "Q116"
cmat[3,1] = "Q216"
cmat[4,1] = "Q316"
cmat[5,1] = "Q416"
cmat[6,1] = "Q117"
cmat[7,1] = "Q217"
cmat[8,1] = "Q317"
cmat[9,1] = "Q417"
cmat[10,1] = "Q118"
cmat[11,1] = "Q218"
