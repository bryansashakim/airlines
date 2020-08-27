library(tidyverse)
library(ggplot2)
library(dplyr)

##### First Import all the Updated Data sets(Now it is "read.table" because I saved them as tables (dataframe I think))
'2012.Q2' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2012.Q2",header=TRUE)
'2012.Q3' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2012.Q3",header=TRUE)
'2012.Q4' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2012.Q4",header=TRUE)
'2013.Q1' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2013.Q1",header=TRUE)
'2013.Q2' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2013.Q2",header=TRUE)
'2013.Q3' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2013.Q3",header=TRUE)
'2013.Q4' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2013.Q4",header=TRUE)
'2014.Q1' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2014.Q1",header=TRUE)
'2014.Q2' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2014.Q2",header=TRUE)
'2014.Q3' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2014.Q3",header=TRUE)
'2014.Q4' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2014.Q4",header=TRUE)
'2015.Q1' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2015.Q1",header=TRUE)
'2015.Q2' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2015.Q2",header=TRUE)
'2015.Q3' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2015.Q3",header=TRUE)
'2015.Q4' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2015.Q4",header=TRUE)
'2016.Q1' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2016.Q1",header=TRUE)
'2016.Q2' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2016.Q2",header=TRUE)
'2016.Q3' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2016.Q3",header=TRUE)
'2016.Q4' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2016.Q4",header=TRUE)
'2017.Q1' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2017.Q1",header=TRUE)
'2017.Q2' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2017.Q2",header=TRUE)
'2017.Q3' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2017.Q3",header=TRUE)
'2017.Q4' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2017.Q4",header=TRUE)
'2018.Q1' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2018.Q1",header=TRUE)
'2018.Q2' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2018.Q2",header=TRUE)
'2018.Q3' <- read.table("/Users/bryankim/Documents/Senior Thesis/Code & Data/Rscripts and results/Cleaned Data/2018.Q3",header=TRUE)

# Create timestamp
`2012.Q2`$timestamp<-paste(`2012.Q2`$YEAR,`2012.Q2`$QUARTER,sep="Q")

# Create lists for the Routes
listOfRoutes2012.Q2<-vector(mode="list")
listOfRoutes2012.Q3<-vector(mode="list")
listOfRoutes2012.Q4<-vector(mode="list")
listOfRoutes2013.Q1<-vector(mode="list")
listOfRoutes2013.Q2<-vector(mode="list")
listOfRoutes2013.Q3<-vector(mode="list")
listOfRoutes2013.Q4<-vector(mode="list")
listOfRoutes2014.Q1<-vector(mode="list")
listOfRoutes2014.Q2<-vector(mode="list")
listOfRoutes2014.Q3<-vector(mode="list")
listOfRoutes2014.Q4<-vector(mode="list")
listOfRoutes2015.Q1<-vector(mode="list")
listOfRoutes2015.Q2<-vector(mode="list")
listOfRoutes2015.Q3<-vector(mode="list")
listOfRoutes2015.Q4<-vector(mode="list")
listOfRoutes2016.Q1<-vector(mode="list")
listOfRoutes2016.Q2<-vector(mode="list")
listOfRoutes2016.Q3<-vector(mode="list")
listOfRoutes2016.Q4<-vector(mode="list")
listOfRoutes2017.Q1<-vector(mode="list")
listOfRoutes2017.Q2<-vector(mode="list")
listOfRoutes2017.Q3<-vector(mode="list")
listOfRoutes2017.Q4<-vector(mode="list")
listOfRoutes2018.Q1<-vector(mode="list")
listOfRoutes2018.Q2<-vector(mode="list")
listOfRoutes2018.Q3<-vector(mode="list")

# Create lists for dates
listOfDates2012.Q2<-vector(mode="list")


# Run For loop to get the Routes run by ULCCs
for (i in seq_along(`2012.Q2`$AIRPORT_GROUP)) 
{if(`2012.Q2`$TK_CARRIER_GROUP[[i]] == "F9" | `2012.Q2`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2012.Q2[i]<-`2012.Q2`[i,6];listOfDates2012.Q2[i]<-`2012.Q2`[i,12]}
   }

for (i in seq_along(`2012.Q3`$AIRPORT_GROUP)) 
{if(`2012.Q3`$TK_CARRIER_GROUP[[i]] == "F9" | `2012.Q3`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2012.Q3[i]<-as.character(`2012.Q3`[i,6])}}

for (i in seq_along(`2012.Q4`$AIRPORT_GROUP)) 
{if(`2012.Q4`$TK_CARRIER_GROUP[[i]] == "F9" | `2012.Q4`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2012.Q4[i]<-as.character(`2012.Q4`[i,6])}}

for (i in seq_along(`2013.Q1`$AIRPORT_GROUP)) 
{if(`2013.Q1`$TK_CARRIER_GROUP[[i]] == "F9" | `2013.Q1`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2013.Q1[i]<-as.character(`2013.Q1`[i,6])}}

for (i in seq_along(`2013.Q2`$AIRPORT_GROUP)) 
{if(`2013.Q2`$TK_CARRIER_GROUP[[i]] == "F9" | `2013.Q2`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2013.Q2[i]<-as.character(`2013.Q2`[i,6])}}

for (i in seq_along(`2013.Q3`$AIRPORT_GROUP)) 
{if(`2013.Q3`$TK_CARRIER_GROUP[[i]] == "F9" | `2013.Q3`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2013.Q3[i]<-as.character(`2013.Q3`[i,6])}}

for (i in seq_along(`2013.Q4`$AIRPORT_GROUP)) 
{if(`2013.Q4`$TK_CARRIER_GROUP[[i]] == "F9" | `2013.Q4`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2013.Q4[i]<-as.character(`2013.Q4`[i,6])}}

for (i in seq_along(`2014.Q1`$AIRPORT_GROUP)) 
{if(`2014.Q1`$TK_CARRIER_GROUP[[i]] == "F9" | `2014.Q1`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2014.Q1[i]<-as.character(`2014.Q1`[i,6])}}

for (i in seq_along(`2014.Q2`$AIRPORT_GROUP)) 
{if(`2014.Q2`$TK_CARRIER_GROUP[[i]] == "F9" | `2014.Q2`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2014.Q2[i]<-as.character(`2014.Q2`[i,6])}}

for (i in seq_along(`2014.Q3`$AIRPORT_GROUP)) 
{if(`2014.Q3`$TK_CARRIER_GROUP[[i]] == "F9" | `2014.Q3`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2014.Q3[i]<-as.character(`2014.Q3`[i,6])}}

for (i in seq_along(`2014.Q4`$AIRPORT_GROUP)) 
{if(`2014.Q4`$TK_CARRIER_GROUP[[i]] == "F9" | `2014.Q4`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2014.Q4[i]<-as.character(`2014.Q4`[i,6])}}

for (i in seq_along(`2015.Q1`$AIRPORT_GROUP)) 
{if(`2015.Q1`$TK_CARRIER_GROUP[[i]] == "F9" | `2015.Q1`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2015.Q1[i]<-as.character(`2015.Q1`[i,6])}}

for (i in seq_along(`2015.Q2`$AIRPORT_GROUP)) 
{if(`2015.Q2`$TK_CARRIER_GROUP[[i]] == "F9" | `2015.Q2`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2015.Q2[i]<-as.character(`2015.Q2`[i,6])}}

for (i in seq_along(`2015.Q3`$AIRPORT_GROUP)) 
{if(`2015.Q3`$TK_CARRIER_GROUP[[i]] == "F9" | `2015.Q3`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2015.Q3[i]<-as.character(`2015.Q3`[i,6])}}

for (i in seq_along(`2015.Q4`$AIRPORT_GROUP)) 
{if(`2015.Q4`$TK_CARRIER_GROUP[[i]] == "F9" | `2015.Q4`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2015.Q4[i]<-as.character(`2015.Q4`[i,6])}}

for (i in seq_along(`2016.Q1`$AIRPORT_GROUP)) 
{if(`2016.Q1`$TK_CARRIER_GROUP[[i]] == "F9" | `2016.Q1`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2016.Q1[i]<-as.character(`2016.Q1`[i,6])}}

for (i in seq_along(`2016.Q2`$AIRPORT_GROUP)) 
{if(`2016.Q2`$TK_CARRIER_GROUP[[i]] == "F9" | `2016.Q2`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2016.Q2[i]<-as.character(`2016.Q2`[i,6])}}

for (i in seq_along(`2016.Q3`$AIRPORT_GROUP)) 
{if(`2016.Q3`$TK_CARRIER_GROUP[[i]] == "F9" | `2016.Q3`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2016.Q3[i]<-as.character(`2016.Q3`[i,6])}}

for (i in seq_along(`2016.Q4`$AIRPORT_GROUP)) 
{if(`2016.Q4`$TK_CARRIER_GROUP[[i]] == "F9" | `2016.Q4`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2016.Q4[i]<-as.character(`2016.Q4`[i,6])}}

for (i in seq_along(`2017.Q1`$AIRPORT_GROUP)) 
{if(`2017.Q1`$TK_CARRIER_GROUP[[i]] == "F9" | `2017.Q1`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2017.Q1[i]<-as.character(`2017.Q1`[i,6])}}

for (i in seq_along(`2017.Q2`$AIRPORT_GROUP)) 
{if(`2017.Q2`$TK_CARRIER_GROUP[[i]] == "F9" | `2017.Q2`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2017.Q2[i]<-as.character(`2017.Q2`[i,6])}}

for (i in seq_along(`2017.Q3`$AIRPORT_GROUP)) 
{if(`2017.Q3`$TK_CARRIER_GROUP[[i]] == "F9" | `2017.Q3`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2017.Q3[i]<-as.character(`2017.Q3`[i,6])}}

for (i in seq_along(`2017.Q4`$AIRPORT_GROUP)) 
{if(`2017.Q4`$TK_CARRIER_GROUP[[i]] == "F9" | `2017.Q4`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2017.Q4[i]<-as.character(`2017.Q4`[i,6])}}

for (i in seq_along(`2018.Q1`$AIRPORT_GROUP)) 
{if(`2018.Q1`$TK_CARRIER_GROUP[[i]] == "F9" | `2018.Q1`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2018.Q1[i]<-as.character(`2018.Q1`[i,6])}}

for (i in seq_along(`2018.Q2`$AIRPORT_GROUP)) 
{if(`2018.Q2`$TK_CARRIER_GROUP[[i]] == "F9" | `2018.Q2`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2018.Q2[i]<-as.character(`2018.Q2`[i,6])}}

for (i in seq_along(`2018.Q3`$AIRPORT_GROUP)) 
{if(`2018.Q3`$TK_CARRIER_GROUP[[i]] == "F9" | `2018.Q3`$TK_CARRIER_GROUP[[i]] == "NK") 
{listOfRoutes2018.Q3[i]<-as.character(`2018.Q3`[i,6])}}


# Dropping all NULL elements
library(plyr)
listOfRoutes2012.Q2<-plyr::compact(listOfRoutes2012.Q2)
listOfDates2012.Q2<-plyr::compact(listOfDates2012.Q2)

# Create dataframe for calender of routes 
ULCCRoutes2012.Q2<-data.frame(I(listOfRoutes2012.Q2))
ULCCRoutes2012.Q2$listOfDates2012.Q2<-listOfDates2012.Q2

### Doing it for all of them
listOfRoutes<-vector(mode="list")
listOfDates<-vector(mode="list")
ReplicationAggregate$timestamp<-paste(ReplicationAggregate$YEAR,ReplicationAggregate$QUARTER,sep="")
for (i in seq_along(ReplicationAggregate$AIRPORT_GROUP)) 
 {if(ReplicationAggregate$TK_CARRIER_GROUP[[i]] == "F9" | ReplicationAggregate$TK_CARRIER_GROUP[[i]] == "NK") 
      {listOfRoutes[i]<-ReplicationAggregate[i,6];listOfDates[i]<-ReplicationAggregate[i,12]}
}

# Dropping all NULL elements
library(plyr)
listOfRoutes<-plyr::compact(listOfRoutes)
listOfDates<-plyr::compact(listOfDates)

# Create dataframe for calender of routes 
ULCCRoutesAggregate<-data.frame(I(listOfRoutes))
ULCCRoutesAggregate$listOfDates<-listOfDates

# unique elements
uniqueULCCRoutesAggregate <- unique(ULCCRoutesAggregate)

# Make it an easy to work with data frame
uniqueULCCRoutesAggregate<-as.data.frame(lapply(uniqueULCCRoutesAggregate,unlist))

# Sort by Market
uniqueULCCRoutesAggregate<-uniqueULCCRoutesAggregate[order(uniqueULCCRoutesAggregate$listOfRoutes),]

# Make it an easy to work with data frame (again)
uniqueULCCRoutesAggregate<-as.data.frame(lapply(uniqueULCCRoutesAggregate,unlist))

# The following gets the FIRST occurence of each unique market, which corresponds to the ENTRY of a carrier in that market 
ULCC_Entry<-uniqueULCCRoutesAggregate[match(unique(uniqueULCCRoutesAggregate$listOfRoutes),uniqueULCCRoutesAggregate$listOfRoutes),]
names(ULCC_Entry)[1]<-"AIRPORT_GROUP"
names(ULCC_Entry)[2]<-"entry"
ULCC_Entry<-ULCC_Entry[with(ULCC_Entry,order(entry)),]
ULCC_Entry<-as.data.frame(lapply(ULCC_Entry,unlist))

# The following gets the LAST occurence of each unique market, which corresponds to the EXIT of a carrier in that market 
library(data.table)
ULCC_Exit<-setDT(uniqueULCCRoutesAggregate)[,list(exit=listOfDates[.N]),by=listOfRoutes]
names(ULCC_Exit)[1]<-"AIRPORT_GROUP"
ULCC_Exit<-ULCC_Exit[with(ULCC_Exit,order(exit)),]


# Saving the uniqueULCCRoutesAggregate, ULCC_Entry, ULCC_Exit data sets
write.table(uniqueULCCRoutesAggregate,file="uniqueULCCRoutesAggregate")
write.table(ULCC_Entry,file="ULCC_Entry")
write.table(ULCC_Exit,file="ULCC_Exit")


# Get the unique Routes of the list
uniqueListOfULCCRoutes2012.Q2 <- unique(listOfRoutes2012.Q2)
uniqueListOfULCCRoutes2012.Q3 <- unique(listOfRoutes2012.Q3)
uniqueListOfULCCRoutes2012.Q4 <- unique(listOfRoutes2012.Q4)
uniqueListOfULCCRoutes2013.Q1 <- unique(listOfRoutes2013.Q1)
uniqueListOfULCCRoutes2013.Q2 <- unique(listOfRoutes2013.Q2)
uniqueListOfULCCRoutes2013.Q3 <- unique(listOfRoutes2013.Q3)
uniqueListOfULCCRoutes2013.Q4 <- unique(listOfRoutes2013.Q4)
uniqueListOfULCCRoutes2014.Q1 <- unique(listOfRoutes2014.Q1)
uniqueListOfULCCRoutes2014.Q2 <- unique(listOfRoutes2014.Q2)
uniqueListOfULCCRoutes2014.Q3 <- unique(listOfRoutes2014.Q3)
uniqueListOfULCCRoutes2014.Q4 <- unique(listOfRoutes2014.Q4)
uniqueListOfULCCRoutes2015.Q1 <- unique(listOfRoutes2015.Q1)
uniqueListOfULCCRoutes2015.Q2 <- unique(listOfRoutes2015.Q2)
uniqueListOfULCCRoutes2015.Q3 <- unique(listOfRoutes2015.Q3)
uniqueListOfULCCRoutes2015.Q4 <- unique(listOfRoutes2015.Q4)
uniqueListOfULCCRoutes2016.Q1 <- unique(listOfRoutes2016.Q1)
uniqueListOfULCCRoutes2016.Q2 <- unique(listOfRoutes2016.Q2)
uniqueListOfULCCRoutes2016.Q3 <- unique(listOfRoutes2016.Q3)
uniqueListOfULCCRoutes2016.Q4 <- unique(listOfRoutes2016.Q4)
uniqueListOfULCCRoutes2017.Q1 <- unique(listOfRoutes2017.Q1)
uniqueListOfULCCRoutes2017.Q2 <- unique(listOfRoutes2017.Q2)
uniqueListOfULCCRoutes2017.Q3 <- unique(listOfRoutes2017.Q3)
uniqueListOfULCCRoutes2017.Q4 <- unique(listOfRoutes2017.Q4)
uniqueListOfULCCRoutes2018.Q1 <- unique(listOfRoutes2018.Q1)
uniqueListOfULCCRoutes2018.Q2 <- unique(listOfRoutes2018.Q2)
uniqueListOfULCCRoutes2018.Q3 <- unique(listOfRoutes2018.Q3)

# Deleting all rows where TICKETING CARRIER GROUP is not one of our airline of interest
`2012.Q2`<-`2012.Q2`[`2012.Q2`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2012.Q2, ]
`2012.Q3`<-`2012.Q3`[`2012.Q3`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2012.Q3, ]
`2012.Q4`<-`2012.Q4`[`2012.Q4`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2012.Q4, ]
`2013.Q1`<-`2013.Q1`[`2013.Q1`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2013.Q1, ]
`2013.Q2`<-`2013.Q2`[`2013.Q2`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2013.Q2, ]
`2013.Q3`<-`2013.Q3`[`2013.Q3`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2013.Q3, ]
`2013.Q4`<-`2013.Q4`[`2013.Q4`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2013.Q4, ]
`2014.Q1`<-`2014.Q1`[`2014.Q1`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2014.Q1, ]
`2014.Q2`<-`2014.Q2`[`2014.Q2`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2014.Q2, ]
`2014.Q3`<-`2014.Q3`[`2014.Q3`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2014.Q3, ]
`2014.Q4`<-`2014.Q4`[`2014.Q4`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2014.Q4, ]
`2015.Q1`<-`2015.Q1`[`2015.Q1`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2015.Q1, ]
`2015.Q2`<-`2015.Q2`[`2015.Q2`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2015.Q2, ]
`2015.Q3`<-`2015.Q3`[`2015.Q3`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2015.Q3, ]
`2015.Q4`<-`2015.Q4`[`2015.Q4`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2015.Q4, ]
`2016.Q1`<-`2016.Q1`[`2016.Q1`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2016.Q1, ]
`2016.Q2`<-`2016.Q2`[`2016.Q2`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2016.Q2, ]
`2016.Q3`<-`2016.Q3`[`2016.Q3`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2016.Q3, ]
`2016.Q4`<-`2016.Q4`[`2016.Q4`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2016.Q4, ]
`2017.Q1`<-`2017.Q1`[`2017.Q1`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2017.Q1, ]
`2017.Q2`<-`2017.Q2`[`2017.Q2`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2017.Q2, ]
`2017.Q3`<-`2017.Q3`[`2017.Q3`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2017.Q3, ]
`2017.Q4`<-`2017.Q4`[`2017.Q4`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2017.Q4, ]
`2018.Q1`<-`2018.Q1`[`2018.Q1`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2018.Q1, ]
`2018.Q2`<-`2018.Q2`[`2018.Q2`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2018.Q2, ]
`2018.Q3`<-`2018.Q3`[`2018.Q3`$AIRPORT_GROUP %in% uniqueListOfULCCRoutes2018.Q3, ]


# Make it a dataframe in order to save it as a table
uniqueListOfULCCRoutes2012.Q2<-data.frame(uniqueListOfULCCRoutes2012.Q2 = unlist(uniqueListOfULCCRoutes2012.Q2))
uniqueListOfULCCRoutes2012.Q3<-data.frame(uniqueListOfULCCRoutes2012.Q3 = unlist(uniqueListOfULCCRoutes2012.Q3))
uniqueListOfULCCRoutes2012.Q4<-data.frame(uniqueListOfULCCRoutes2012.Q4 = unlist(uniqueListOfULCCRoutes2012.Q4))
uniqueListOfULCCRoutes2013.Q1<-data.frame(uniqueListOfULCCRoutes2013.Q1 = unlist(uniqueListOfULCCRoutes2013.Q1))
uniqueListOfULCCRoutes2013.Q2<-data.frame(uniqueListOfULCCRoutes2013.Q2 = unlist(uniqueListOfULCCRoutes2013.Q2))
uniqueListOfULCCRoutes2013.Q3<-data.frame(uniqueListOfULCCRoutes2013.Q3 = unlist(uniqueListOfULCCRoutes2013.Q3))
uniqueListOfULCCRoutes2013.Q4<-data.frame(uniqueListOfULCCRoutes2013.Q4 = unlist(uniqueListOfULCCRoutes2013.Q4))
uniqueListOfULCCRoutes2014.Q1<-data.frame(uniqueListOfULCCRoutes2014.Q1 = unlist(uniqueListOfULCCRoutes2014.Q1))
uniqueListOfULCCRoutes2014.Q2<-data.frame(uniqueListOfULCCRoutes2014.Q2 = unlist(uniqueListOfULCCRoutes2014.Q2))
uniqueListOfULCCRoutes2014.Q3<-data.frame(uniqueListOfULCCRoutes2014.Q3 = unlist(uniqueListOfULCCRoutes2014.Q3))
uniqueListOfULCCRoutes2014.Q4<-data.frame(uniqueListOfULCCRoutes2014.Q4 = unlist(uniqueListOfULCCRoutes2014.Q4))
uniqueListOfULCCRoutes2015.Q1<-data.frame(uniqueListOfULCCRoutes2015.Q1 = unlist(uniqueListOfULCCRoutes2015.Q1))
uniqueListOfULCCRoutes2015.Q2<-data.frame(uniqueListOfULCCRoutes2015.Q2 = unlist(uniqueListOfULCCRoutes2015.Q2))
uniqueListOfULCCRoutes2015.Q3<-data.frame(uniqueListOfULCCRoutes2015.Q3 = unlist(uniqueListOfULCCRoutes2015.Q3))
uniqueListOfULCCRoutes2015.Q4<-data.frame(uniqueListOfULCCRoutes2015.Q4 = unlist(uniqueListOfULCCRoutes2015.Q4))
uniqueListOfULCCRoutes2016.Q1<-data.frame(uniqueListOfULCCRoutes2016.Q1 = unlist(uniqueListOfULCCRoutes2016.Q1))
uniqueListOfULCCRoutes2016.Q2<-data.frame(uniqueListOfULCCRoutes2016.Q2 = unlist(uniqueListOfULCCRoutes2016.Q2))
uniqueListOfULCCRoutes2016.Q3<-data.frame(uniqueListOfULCCRoutes2016.Q3 = unlist(uniqueListOfULCCRoutes2016.Q3))
uniqueListOfULCCRoutes2016.Q4<-data.frame(uniqueListOfULCCRoutes2016.Q4 = unlist(uniqueListOfULCCRoutes2016.Q4))
uniqueListOfULCCRoutes2017.Q1<-data.frame(uniqueListOfULCCRoutes2017.Q1 = unlist(uniqueListOfULCCRoutes2017.Q1))
uniqueListOfULCCRoutes2017.Q2<-data.frame(uniqueListOfULCCRoutes2017.Q2 = unlist(uniqueListOfULCCRoutes2017.Q2))
uniqueListOfULCCRoutes2017.Q3<-data.frame(uniqueListOfULCCRoutes2017.Q3 = unlist(uniqueListOfULCCRoutes2017.Q3))
uniqueListOfULCCRoutes2017.Q4<-data.frame(uniqueListOfULCCRoutes2017.Q4 = unlist(uniqueListOfULCCRoutes2017.Q4))
uniqueListOfULCCRoutes2018.Q1<-data.frame(uniqueListOfULCCRoutes2018.Q1 = unlist(uniqueListOfULCCRoutes2018.Q1))
uniqueListOfULCCRoutes2018.Q2<-data.frame(uniqueListOfULCCRoutes2018.Q2 = unlist(uniqueListOfULCCRoutes2018.Q2))
uniqueListOfULCCRoutes2018.Q3<-data.frame(uniqueListOfULCCRoutes2018.Q3 = unlist(uniqueListOfULCCRoutes2018.Q3))


# save (write) the list onto my local computer
write.table(uniqueListOfULCCRoutes2012.Q2,file="uniqueListOfULCCRoutes2012.Q2")
write.table(uniqueListOfULCCRoutes2012.Q3,file="uniqueListOfULCCRoutes2012.Q3")
write.table(uniqueListOfULCCRoutes2012.Q4,file="uniqueListOfULCCRoutes2012.Q4")
write.table(uniqueListOfULCCRoutes2013.Q1,file="uniqueListOfULCCRoutes2013.Q1")
write.table(uniqueListOfULCCRoutes2013.Q2,file="uniqueListOfULCCRoutes2013.Q2")
write.table(uniqueListOfULCCRoutes2013.Q3,file="uniqueListOfULCCRoutes2013.Q3")
write.table(uniqueListOfULCCRoutes2013.Q4,file="uniqueListOfULCCRoutes2013.Q4")
write.table(uniqueListOfULCCRoutes2014.Q1,file="uniqueListOfULCCRoutes2014.Q1")
write.table(uniqueListOfULCCRoutes2014.Q2,file="uniqueListOfULCCRoutes2014.Q2")
write.table(uniqueListOfULCCRoutes2014.Q3,file="uniqueListOfULCCRoutes2014.Q3")
write.table(uniqueListOfULCCRoutes2014.Q4,file="uniqueListOfULCCRoutes2014.Q4")
write.table(uniqueListOfULCCRoutes2015.Q1,file="uniqueListOfULCCRoutes2015.Q1")
write.table(uniqueListOfULCCRoutes2015.Q2,file="uniqueListOfULCCRoutes2015.Q2")
write.table(uniqueListOfULCCRoutes2015.Q3,file="uniqueListOfULCCRoutes2015.Q3")
write.table(uniqueListOfULCCRoutes2015.Q4,file="uniqueListOfULCCRoutes2015.Q4")
write.table(uniqueListOfULCCRoutes2016.Q1,file="uniqueListOfULCCRoutes2016.Q1")
write.table(uniqueListOfULCCRoutes2016.Q2,file="uniqueListOfULCCRoutes2016.Q2")
write.table(uniqueListOfULCCRoutes2016.Q3,file="uniqueListOfULCCRoutes2016.Q3")
write.table(uniqueListOfULCCRoutes2016.Q4,file="uniqueListOfULCCRoutes2016.Q4")
write.table(uniqueListOfULCCRoutes2017.Q1,file="uniqueListOfULCCRoutes2017.Q1")
write.table(uniqueListOfULCCRoutes2017.Q2,file="uniqueListOfULCCRoutes2017.Q2")
write.table(uniqueListOfULCCRoutes2017.Q3,file="uniqueListOfULCCRoutes2017.Q3")
write.table(uniqueListOfULCCRoutes2017.Q4,file="uniqueListOfULCCRoutes2017.Q4")
write.table(uniqueListOfULCCRoutes2018.Q1,file="uniqueListOfULCCRoutes2018.Q1")
write.table(uniqueListOfULCCRoutes2018.Q2,file="uniqueListOfULCCRoutes2018.Q2")
write.table(uniqueListOfULCCRoutes2018.Q3,file="uniqueListOfULCCRoutes2018.Q3")

# save (write) the new data sets on local computer
write.table(`2012.Q2`,file="2012.Q2",row.names = FALSE)
write.table(`2012.Q3`,file="2012.Q3",row.names = FALSE)
write.table(`2012.Q4`,file="2012.Q4",row.names = FALSE)
write.table(`2013.Q1`,file="2013.Q1",row.names = FALSE)
write.table(`2013.Q2`,file="2013.Q2",row.names = FALSE)
write.table(`2013.Q3`,file="2013.Q3",row.names = FALSE)
write.table(`2013.Q4`,file="2013.Q4",row.names = FALSE)
write.table(`2014.Q1`,file="2014.Q1",row.names = FALSE)
write.table(`2014.Q2`,file="2014.Q2",row.names = FALSE)
write.table(`2014.Q3`,file="2014.Q3",row.names = FALSE)
write.table(`2014.Q4`,file="2014.Q4",row.names = FALSE)
write.table(`2015.Q1`,file="2015.Q1",row.names = FALSE)
write.table(`2015.Q2`,file="2015.Q2",row.names = FALSE)
write.table(`2015.Q3`,file="2015.Q3",row.names = FALSE)
write.table(`2015.Q4`,file="2015.Q4",row.names = FALSE)
write.table(`2016.Q1`,file="2016.Q1",row.names = FALSE)
write.table(`2016.Q2`,file="2016.Q2",row.names = FALSE)
write.table(`2016.Q3`,file="2016.Q3",row.names = FALSE)
write.table(`2016.Q4`,file="2016.Q4",row.names = FALSE)
write.table(`2017.Q1`,file="2017.Q1",row.names = FALSE)
write.table(`2017.Q2`,file="2017.Q2",row.names = FALSE)
write.table(`2017.Q3`,file="2017.Q3",row.names = FALSE)
write.table(`2017.Q4`,file="2017.Q4",row.names = FALSE)
write.table(`2018.Q1`,file="2018.Q1",row.names = FALSE)
write.table(`2018.Q2`,file="2018.Q2",row.names = FALSE)
write.table(`2018.Q3`,file="2018.Q3",row.names = FALSE)

### Combine all the uniqueList into one
# First need to rename column names so they are all the same to use rbind command
install.packages("tidyverse")
library(tidyverse)

names(uniqueListOfULCCRoutes2012.Q2)[1] <- "temp"
names(uniqueListOfULCCRoutes2012.Q3)[1] <- "temp"
names(uniqueListOfULCCRoutes2012.Q4)[1] <- "temp"
names(uniqueListOfULCCRoutes2013.Q1)[1] <- "temp"
names(uniqueListOfULCCRoutes2013.Q2)[1] <- "temp"
names(uniqueListOfULCCRoutes2013.Q3)[1] <- "temp"
names(uniqueListOfULCCRoutes2013.Q4)[1] <- "temp"
names(uniqueListOfULCCRoutes2014.Q1)[1] <- "temp"
names(uniqueListOfULCCRoutes2014.Q2)[1] <- "temp"
names(uniqueListOfULCCRoutes2014.Q3)[1] <- "temp"
names(uniqueListOfULCCRoutes2014.Q4)[1] <- "temp"
names(uniqueListOfULCCRoutes2015.Q1)[1] <- "temp"
names(uniqueListOfULCCRoutes2015.Q2)[1] <- "temp"
names(uniqueListOfULCCRoutes2015.Q3)[1] <- "temp"
names(uniqueListOfULCCRoutes2015.Q4)[1] <- "temp"
names(uniqueListOfULCCRoutes2016.Q1)[1] <- "temp"
names(uniqueListOfULCCRoutes2016.Q2)[1] <- "temp"
names(uniqueListOfULCCRoutes2016.Q3)[1] <- "temp"
names(uniqueListOfULCCRoutes2016.Q4)[1] <- "temp"
names(uniqueListOfULCCRoutes2017.Q1)[1] <- "temp"
names(uniqueListOfULCCRoutes2017.Q2)[1] <- "temp"
names(uniqueListOfULCCRoutes2017.Q3)[1] <- "temp"
names(uniqueListOfULCCRoutes2017.Q4)[1] <- "temp"
names(uniqueListOfULCCRoutes2018.Q1)[1] <- "temp"
names(uniqueListOfULCCRoutes2018.Q2)[1] <- "temp"
names(uniqueListOfULCCRoutes2018.Q3)[1] <- "temp"

AggregateUniqueULCCRoutes <- rbind(uniqueListOfULCCRoutes2012.Q2,uniqueListOfULCCRoutes2012.Q3,uniqueListOfULCCRoutes2012.Q4,
                                   uniqueListOfULCCRoutes2013.Q1,uniqueListOfULCCRoutes2013.Q2,uniqueListOfULCCRoutes2013.Q3,uniqueListOfULCCRoutes2013.Q4,
                                   uniqueListOfULCCRoutes2014.Q1,uniqueListOfULCCRoutes2014.Q2,uniqueListOfULCCRoutes2014.Q3,uniqueListOfULCCRoutes2014.Q4,
                                   uniqueListOfULCCRoutes2015.Q1,uniqueListOfULCCRoutes2015.Q2,uniqueListOfULCCRoutes2015.Q3,uniqueListOfULCCRoutes2015.Q4,
                                   uniqueListOfULCCRoutes2015.Q1,uniqueListOfULCCRoutes2015.Q2,uniqueListOfULCCRoutes2015.Q3,uniqueListOfULCCRoutes2015.Q4,
                                   uniqueListOfULCCRoutes2016.Q1,uniqueListOfULCCRoutes2016.Q2,uniqueListOfULCCRoutes2016.Q3,uniqueListOfULCCRoutes2016.Q4,
                                   uniqueListOfULCCRoutes2017.Q1,uniqueListOfULCCRoutes2017.Q2,uniqueListOfULCCRoutes2017.Q3,uniqueListOfULCCRoutes2017.Q4,
                                   uniqueListOfULCCRoutes2018.Q1,uniqueListOfULCCRoutes2018.Q2,uniqueListOfULCCRoutes2018.Q3)

AggregateUniqueULCCRoutes<-unique(AggregateUniqueULCCRoutes)

names(AggregateUniqueULCCRoutes)[1] <- "uniqueListOfULCCRoutes_year"

write.table(AggregateUniqueULCCRoutes,file="AggregateUniqueULCCRoutes(2012.Q2_2018.Q4")

# Deleting unnecessary data
rm("uniqueListOfULCCRoutes2018.Q2", "uniqueListOfULCCRoutes2018.Q3")
rm("uniqueListOfULCCRoutes2012.Q2", "uniqueListOfULCCRoutes2012.Q3", "uniqueListOfULCCRoutes2012.Q4")
rm("uniqueListOfULCCRoutes2013.Q2", "uniqueListOfULCCRoutes2013.Q3", "uniqueListOfULCCRoutes2013.Q4")
rm("uniqueListOfULCCRoutes2014.Q2" ,"uniqueListOfULCCRoutes2014.Q3", "uniqueListOfULCCRoutes2014.Q4")
rm("uniqueListOfULCCRoutes2015.Q2", "uniqueListOfULCCRoutes2015.Q3", "uniqueListOfULCCRoutes2015.Q4")
rm("uniqueListOfULCCRoutes2016.Q1", "uniqueListOfULCCRoutes2016.Q2", "uniqueListOfULCCRoutes2016.Q3")
rm("uniqueListOfULCCRoutes2016.Q4", "uniqueListOfULCCRoutes2017.Q1" ,"uniqueListOfULCCRoutes2017.Q2")
rm("uniqueListOfULCCRoutes2016.Q4", "uniqueListOfULCCRoutes2017.Q1", "uniqueListOfULCCRoutes2017.Q2")
rm("uniqueListOfULCCRoutes2013.Q1" ,"uniqueListOfULCCRoutes2014.Q1", "uniqueListOfULCCRoutes2015.Q1")
rm("uniqueListOfULCCRoutes2017.Q3", "uniqueListOfULCCRoutes2017.Q4", "uniqueListOfULCCRoutes2018.Q1")

rm(listOfRoutes2012.Q2,
   listOfRoutes2012.Q3,
   listOfRoutes2012.Q3,
   listOfRoutes2012.Q4,
   listOfRoutes2013.Q1,
   listOfRoutes2013.Q2,
   listOfRoutes2013.Q3,
   listOfRoutes2013.Q4,
   listOfRoutes2014.Q1,
   listOfRoutes2014.Q2,
   listOfRoutes2014.Q3,
   listOfRoutes2014.Q4,
   listOfRoutes2015.Q1,
   listOfRoutes2015.Q2,
   listOfRoutes2015.Q3,
   listOfRoutes2015.Q4,
   listOfRoutes2016.Q1,
   listOfRoutes2016.Q2,
   listOfRoutes2016.Q3,
   listOfRoutes2016.Q4,
   listOfRoutes2017.Q1,
   listOfRoutes2017.Q2,
   listOfRoutes2017.Q3,
   listOfRoutes2017.Q4,
   listOfRoutes2018.Q1,
   listOfRoutes2018.Q2,
   listOfRoutes2018.Q3)

rm(`2012.Q2`,
   `2012.Q3`,
   `2012.Q4`,
   `2013.Q1`,
   `2013.Q2`,
   `2013.Q3`,
   `2013.Q4`,
   `2014.Q1`,
   `2014.Q2`,
   `2014.Q3`,
   `2014.Q4`,
   `2015.Q1`,
   `2015.Q2`,
   `2015.Q3`,
   `2015.Q4`,
   `2016.Q1`,
   `2016.Q2`,
   `2016.Q3`,
   `2016.Q4`,
   `2017.Q1`,
   `2017.Q2`,
   `2017.Q3`,
   `2017.Q4`,
   `2018.Q1`,
   `2018.Q2`,
   `2018.Q3`)

# Deleting the MARKET_MILES_FLOWN column
`2015.Q2`$MARKET_MILES_FLOWN <- NULL
`2015.Q3`$MARKET_MILES_FLOWN <- NULL
`2015.Q4`$MARKET_MILES_FLOWN <- NULL
`2016.Q1`$MARKET_MILES_FLOWN <- NULL
`2016.Q2`$MARKET_MILES_FLOWN <- NULL
`2016.Q3`$MARKET_MILES_FLOWN <- NULL
`2016.Q4`$MARKET_MILES_FLOWN <- NULL
`2017.Q1`$MARKET_MILES_FLOWN <- NULL
`2017.Q2`$MARKET_MILES_FLOWN <- NULL
`2017.Q3`$MARKET_MILES_FLOWN <- NULL
`2017.Q4`$MARKET_MILES_FLOWN <- NULL
`2018.Q1`$MARKET_MILES_FLOWN <- NULL
`2018.Q2`$MARKET_MILES_FLOWN <- NULL
`2018.Q3`$MARKET_MILES_FLOWN <- NULL

# Deleting the DISTANCE_GROUP column
`2015.Q3`$DISTANCE_GROUP <- NULL

# Deleting the MARKET_DISTANCE column
`2015.Q3`$MARKET_DISTANCE <- NULL

# Deleting the X column
`2012.Q2`$X <- NULL
`2012.Q3`$X <- NULL
`2012.Q4`$X <- NULL
`2013.Q1`$X <- NULL
`2013.Q2`$X <- NULL
`2013.Q3`$X <- NULL
`2013.Q4`$X <- NULL
`2014.Q1`$X <- NULL
`2014.Q2`$X <- NULL
`2014.Q3`$X <- NULL
`2014.Q4`$X <- NULL
`2015.Q1`$X <- NULL
`2015.Q2`$X <- NULL
`2015.Q3`$X <- NULL
`2015.Q4`$X <- NULL
`2016.Q1`$X <- NULL
`2016.Q2`$X <- NULL
`2016.Q3`$X <- NULL
`2016.Q4`$X <- NULL
`2017.Q1`$X <- NULL
`2017.Q2`$X <- NULL
`2017.Q3`$X <- NULL
`2017.Q4`$X <- NULL
`2018.Q1`$X <- NULL
`2018.Q2`$X <- NULL
`2018.Q3`$X <- NULL


# Merging all the datasets into one aggregated one.
Aggregate<-rbind(`2012.Q2`,`2012.Q3`,`2012.Q4`,`2013.Q1`,`2013.Q2`,`2013.Q3`,`2013.Q4`,`2014.Q1`,`2014.Q2`,`2014.Q3`,`2014.Q4`,`2015.Q1`,`2015.Q2`,`2015.Q3`,`2015.Q4`,`2016.Q1`,`2016.Q2`,`2016.Q3`,`2016.Q4`,`2017.Q1`,`2017.Q2`,`2017.Q3`,`2017.Q4`,`2018.Q1`,`2018.Q2`,`2018.Q3`)

# Adding ULCC_ENTERED Dummy variable (this one does not include Allegiant)
Aggregate$ULCC_ENTERED <- ifelse(Aggregate$TK_CARRIER_GROUP == "F9" | Aggregate$TK_CARRIER_GROUP == "NK", 1, 0)

# time index is a year and quarter, so making new variable time to represent YEAR.QUARTER
Aggregate$timestamp<-paste(Aggregate$YEAR,Aggregate$QUARTER,sep="")

# Drop 2018 Quarter 4 because not in Greg's paper
Aggregate<-Aggregate[Aggregate$timestamp != "2018Q4",]

# Drop Allegiant because not in Greg's paper
Aggregate<-Aggregate[Aggregate$TK_CARRIER_GROUP != "G4",]

# logging MARKET_FARES
Aggregate[,10]<-log(Aggregate[,10])
names(Aggregate)[10]<-"log(fare)"

# making observation ID for each time/route/carrier level
Aggregate$observID<-paste(Aggregate$timestamp,Aggregate$AIRPORT_GROUP,Aggregate$TK_CARRIER_GROUP,sep=".")

# Saving the Aggregate data
write.table(`Aggregate`,file="Aggregate",row.names = FALSE)

# Getting each unique observation (time/route/carrier level)
observID<-as.list(ReplicationAggregate$observID)
uniqueObservIDtemp<-unique(observID)
uniqueObservID<-data.frame(uniqueObservIDtemp = unlist(uniqueObservIDtemp))

### Getting average fares (not traffic weighted) for each unique observation(route/carrier/time combo)
# Creating data frame
AvgFare<-data.frame()
# Establishing each unique observation in the data frame
AvgFare<-data.frame(uniqueObservID)
names(AvgFare)[1]<-"observID"
# temporarily make each avg fare 0
AvgFare$Sums<-0
# Calculating Averages
library(plyr)
AvgFare <- ddply(ReplicationAggregate,.(YEAR,QUARTER,AIRPORT_GROUP,TK_CARRIER_GROUP,time,observID),summarize,lfare=mean(MARKET_FARE))

# Merging AvgFare with ULCC_Entry and ULCC_Exit
library(dplyr)
AvgFare<-left_join(AvgFare,ULCC_Entry,by="AIRPORT_GROUP")
AvgFare$entry<-ifelse(AvgFare$TK_CARRIER_GROUP %in% c("F9", "WN", "AA", "UA", "US", "DL", "NK"),AvgFare$entry,NA) # This is not necessary at all. Reminder that it was previously wrong.

AvgFare<-left_join(AvgFare,ULCC_Exit,by="AIRPORT_GROUP")
AvgFare$exit<-ifelse(AvgFare$TK_CARRIER_GROUP %in% c("F9", "WN", "AA", "UA", "US", "DL", "NK"),AvgFare$exit,NA) # This is not necessary at all. Reminder that it was previously wrong.


## ULCC_ENTERED dummy
# 0 if the route i is not treated, or if the route i is treated but the time t is in
# either of the two quarters prior to entry.
# 1 if the route i is treated, and t is in the quarter of entry or one of the four
# quarters proceeding entry.
AvgFare$ULCC_ENTERED <- ifelse((AvgFare$TK_CARRIER_GROUP == "F9" & AvgFare$time >= AvgFare$entry & AvgFare$time <= AvgFare$exit) | (AvgFare$TK_CARRIER_GROUP == "NK" & AvgFare$time >= AvgFare$entry & AvgFare$time <= AvgFare$exit),1,0)
# It's not just that if the TK_CARRIER_GROUP is F9 or NK, because that is only capturing if we are looking at whether
# a ULCC is not that particular observation
# We need to do:
# If for each route, any ULCC was also running that market in that same timestamp or after, then the dummy is 1. 

# Making ID column based on the route
AvgFare$RouteID<-AvgFare %>% group_indices(AIRPORT_GROUP)

# Making ID column based on the YEAR.QUARTER
AvgFare$timeID<-AvgFare %>% group_indices(time)
# or AvgFare$timeID<-AvgFare %>% group_indices(YEAR, QUARTER)

## Making each route the same: e.g. both DEN-RNO and RNO-DEN would be designated “DEN:RNO”
# Ordering data frame by AIRPORT_GROUP
AvgFare<-AvgFare[order(AvgFare$AIRPORT_GROUP),]
# unlisting
AvgFare<-as.data.frame(lapply(AvgFare,unlist))
# Ordering each AIRPORT_GROUP element
working2<-AvgFare[25745,3]
working2<-strsplit(working2,":")
working2<-as.character(working2[[1]])
working2<-working2[sort.list(order(working2))]
working2<-paste(working2,collapse=":")

templist<-vector(mode="list")

for (i in seq_along(AvgFare$AIRPORT_GROUP)) 
{
   working<-AvgFare[i,3]
   working<-strsplit(working,":")
   working<-as.character(working[[1]])
   working<-working[sort.list(order(working))]
   working<-paste(working,collapse=":")
   templist<-append(templist,working)
}
templistDF<-data.frame(I(templist))
names(templistDF)[1]<-"AIRPORT_GROUP"
AvgFare$AIRPORT_GROUP<-templistDF

# Make it a data table?
AvgFare<-data.table(AvgFare)

# making observation ID for each time/route/carrier level
AvgFare$observID<-paste(AvgFare$timestamp,AvgFare$AIRPORT_GROUP,AvgFare$TK_CARRIER_GROUP,sep=".")

# Saving the Aggregate data
write.csv(AvgFare,file="AvgFare.csv",row.names = FALSE)

write.table(AvgFare,file="AvgFare",row.names = FALSE)

### YOU NEED TO INCORPORATE THE Difference in Difference effects!! If ULCC is currently on the route then 1, if it has exited, then 0!!! ###

###### OLS ESTIMATION ######

### Loading Required Packages 
install.packages("gplots")
library(gplots)
install.packages("stargazer")
library(stargazer)
# Panel Linear Model package
install.packages("plm")
library(plm)
library(foreign) # allows us to read data from the web.
install.packages("dplyr")
library(dplyr)


### Scratch

# Should work with this data set first: Legacy carriers are only US,UA,DL,AA, along with the ULCCs
ReplicationAggregate<-Aggregate[Aggregate$TK_CARRIER_GROUP %in% c("US","UA","DL","AA","WN","F9","NK"),]
write.table(ReplicationAggregate,file="ReplicationAggregate",row.names = FALSE)
RobservID<-as.list(ReplicationAggregate$observID)
RuniqueObservID<-unique(RobservID)
#making it into a more usable dataframe:
RuniqueObservID<-data.frame(RuniqueObservID = unlist(RuniqueObservID))


#### Some useful descriptive statistics to check with figures with Greg's ####
#Low Cost Carrier Overlap with Major Network Carriers
 See example at Low Cost Carrier Growth in the U.S. Airline Industry.pdf by Harumi Ito and Darin Lee
#This gives us the (traffic-weighted??) mean fare for each carrier
aggregate(AvgFare[,7],list(AvgFare$TK_CARRIER_GROUP),mean)
#This gives us the (traffic-weighted??) mean fare for each year
aggregate(AvgFare[,7],list(AvgFare$YEAR),mean)
# Unique routes per year for each carrier 

# Unique routes shared by ULCCs and Legacys and Southwest per year 

# Number of new markets established by ULCCs per year (compare that trend with Legacys and Southwest)


##### Regular OLS regression does not consider heterogeneity
#(unobserved characteristics that do not change over time) across time or routes, 
#so need to have fixed effects for that. ##### 

# Shows heterogeneity in market fare by year
plotmeans(lfare ~ YEAR, data=`Aggregate`)

# Simple OLS of lfare on Carrier
summary(lm(lfare~TK_CARRIER_GROUP,data=AvgFare))

# fixed effects on year
fixed<-lm(lfare~ULCC_ENTERED + factor(YEAR) - 1, data=AvgFare)
summary(fixed)

# Simple OLS of MARKET_FARES on ULCC_ENTERED
SimpleOLS <- lm(lfare~ULCC_ENTERED,data=AvgFare)
summary(SimpleOLS)

# Implementing Specification 1: Baseline DD Model with Time & Route Fixed Effects

fixed.test<-plm(lfare ~ ULCC_ENTERED, data=Aggregate,model="within")

fixed<-plm(Aggregate$MARKET_FARE~Aggregate$ULCC_ENTERED, data=Aggregate, index=c('AIRPORT_GROUP','YEAR'), model="within")

Aggregate$id <- group_indices(Aggregate,AIRPORT_GROUP,YEAR)
Fixed.dum <- plm(MARKET_FARE ~ ULCC_ENTERED, data=Aggregate,index=c("id"),model="within")

############  ############  ############
### Helpful miscellaneous function ###
### AND EVENT STUDY CODE  ############
############  ############  ############
# Changing "time" or "timestamp" column so it doesn't have the "Q"
test$time=paste(test$YEAR,test$QUARTER,sep="")
## Changing entry column to it doesnt have the "Q"
# Entry:
# Make sure entry column is characters
test<-left_join(test,ULCC_Entry,by="AIRPORT_GROUP")
names(test)[9]="entry"
test=test[,-c(11)]
templist<-vector(mode="list")
for (i in seq_along(test$entry)) 
{
   working<-test[i,9]
   working<-strsplit(working,"Q")
   working<-as.character(working[[1]])
   working<-paste(working[1],working[2],sep="")
   templist<-append(templist,working)
}
templistDF<-data.frame(I(templist))
names(templistDF)[1]<-"entry"
test$entry<-templistDF
names(test)[9]="entry"

test$entry=na_if(test$entry,"NANA")
test$entry=withNA

# Exit:
# Make sure entry column is characters
test<-left_join(test,ULCC_Exit,by="AIRPORT_GROUP")
names(test)[10]="exit"
test=test[,-c(11)]
templist<-vector(mode="list")
for (i in seq_along(test$exit)) 
{
   working<-test[i,10]
   working<-strsplit(working,"Q")
   working<-as.character(working[[1]])
   working<-paste(working[1],working[2],sep="")
   templist<-append(templist,working)
}
templistDF<-data.frame(I(templist))
names(templistDF)[1]<-"exit"
test$exit<-templistDF
names(test)[10]="exit"


test$entry<-as.data.frame(lapply(test$entry,unlist))
test$exit<-as.data.frame(lapply(test$exit,unlist))

# Converting those columns of interest to numeric
test=data.table(test)
sapply(test, mode)
test$time=sapply(test[,test$time],as.numeric)
test$entry=sapply(test[,test$entry],as.numeric)
test$exit=sapply(test[,test$exit],as.numeric)

test=mutate(test,T01_Entry=ifelse((entry-time==1|entry-time==7),1,0))
test=mutate(test,T02_Entry=ifelse((entry-time==2|entry-time==8),1,0))
test=mutate(test,T0_Entry=ifelse((entry-time==0),1,0))

test=mutate(test,T1_Entry=ifelse((time-entry==1|time-entry==7),1,0))
test=mutate(test,T2_Entry=ifelse((time-entry==2|time-entry==8),1,0))
test=mutate(test,T3_Entry=ifelse((time-entry==3|time-entry==9),1,0))

test$Treated=ifelse(test$time>=test$entry & test$time<=test$exit,1,0)
test$DID=test$ULCC_ENTERED*test$Treated

## SAVING
write.csv(test,file="RegressUpdate.csv",row.names = FALSE)

#########################
###### Event Study ######
#########################
summary(lm(test$lfare~test$T02_Entry+test$T01_Entry+test$T0_Entry+test$T1_Entry+test$T2_Entry+test$T3_Entry))


