library(magrittr)
library(tidyverse) ##Load the library which include dplyr.
library(fastDummies) ##Load the library that create dummy variables.
library(lubridate)
library(plyr)
library(dplyr)
library("RODBC")
ques1 <- read.csv("IC_BP_v2.csv") ##Read in the data frame applying the read.csv() function.
library(tidyverse) ##Load the library which include dplyr.
ques1 %>% 
  ## rename BP alerts to BP status using rename(), can also use colnames() in R base functions.
  rename(
    BPStatus = BPAlerts)
sample_n(ques1, 10) ##print random 10 rows to show BPStatus column name changed

## different blood pressure type using if else method then to create dummy categorical value using dummy_cols()     
outcomes <- ques1 %>% mutate(dy =ifelse(grepl("Hypo1",BPAlerts),"Controlled",
                                        ifelse(grepl("Normal",BPAlerts),"Controlled",
                                               ifelse(grepl("Hypo2",BPAlerts),"UnControlled",
                                                      ifelse(grepl("HTN1",BPAlerts),"UnControlled",ifelse(grepl("HTN1",BPAlerts),"UnControlled",
                                                                                                          ifelse(grepl("HTN2",BPAlerts),"UnControlled",
                                                                                                                 ifelse(grepl("HTN3",BPAlerts),"UnControlled","NA")))))))) %>% dummy_cols(select_columns = "dy")
ques1c<-sqlQuery(myconn,"select * from Demographics") ## import Demographics table from SQL Server
newtable <- merge(outcomes, ques1c, by.x = "ID", by.y = "contactid") ## merger the previous outcomes table with Demographics
newtable[sample(nrow(newtable), 10), ] ## display the result of random 10 rows

## question 1d: Reformat time to proper date type, that make the 12 week interval calculation simpler
newtable$tri_imaginecareenrollmentemailsentdate <- as.Date(newtable$tri_imaginecareenrollmentemailsentdate, format = "%m/%d/%Y")
newtable$ObservedTime <- as.Date("1900-01-01") + days(newtable$ObservedTime)

## Next select the data that is within the interval 
newtable <- subset(newtable, newtable$ObservedTime >= 
                     newtable$tri_imaginecareenrollmentemailsentdate & newtable$ObservedTime < 
                     newtable$tri_imaginecareenrollmentemailsentdate + 12*7)

## Then summarize the diastolic, systolic values and average scores for grouped 12 weeks period
count <- ddply(newtable,.(ID),nrow)
colnames(count) [2] <- "total_ counts"
newtable <- merge(newtable, count, by.x = "ID", by.y = "ID")
newtable <- newtable[rowSums(is.na(newtable[1]))!=1,]
total_ table <- newtable %>% 
  group_by(ID) %>% 
  summarise(AvgSys = sum(SystolicValue, na.rm=TRUE),
            AvgDia = sum(Diastolicvalue, na.rm=TRUE),
            AvgScore = sum(BPAlerts, na.rm=TRUE))
## Question 1e:Each calculation on average difference on diastolic, systolic, and
## score changes during the first and last week and pass back to 
## new difference columns. 
newtable$AvgDiaDifference <- newtable$AvgDia[12]-newtable$AvgDia[1]
newtable$AvgSysDifference <- newtable$AvgSys[12]-newtable$AvgSys[1]
newtable$AvgScoreDifference <- newtable$AvgScore[12]/newtable$AvgScore[1]
## Store result to comparasion tablee
comparasion <- newtable %>%
  group_by(ID) %>%
  filter(row_number()==1 | row_number()==n())
compare_list[sample(nrow(compare_list), 10), ]

## Question 3: to find how many in newtable, the customers change status from uncontrolled to controlled
change_status <- newtable %>%
  group_by(ID) %>%
  filter(row_number()==1 | row_number()==n()) %>%
  select(ID, BPStatus )
change_data <- change_data %>% group_by(ID) %>% mutate(count = row_number())

library(RODBC) ##connect using ODBC driver to retrieve datasets
ques1c<-sqlQuery(myconn,"select * from Demographics")
## First I import Text, Conditions table and 
## merge them with Demographics table from question1 
## and select only the necessary column values to keep
ques3text <- sqlQuery(myconn, "select * from Text") 
ques3text_demo <- inner_join(ques1c,ques3text, by = c("contactid"="tri_contactId"))
ques3cond <- sqlQuery(myconn, "select * from Conditions")
ques3joined <- merge(ques3text_demo, ques3cond, by.x = "contactid", by.y = "tri_patientid") %>%
  select(-ContactId,-SenderName,-SentDate,-tri_name2)
## Then prior to final Date execution, I convert 
## the SentDate to proper format and check to 
##keep the unique values
ques3joined$TextSentDate <- as.Date(ques3joined$TextSentDate, "%m/%d/%y") %>%
  length(unique(ques3joined$contactid))
## Final step, I use slice() to get latest 
## date when the text was sent and display result
ques3joined %>% 
  group_by(tri_contactId) %>%
  slice(which.max(TextSentDate))
sample_n(ques3joined, 10) 
