
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- packages-------------------------------------------------------
library(SASxport)
library(RODBC)
library(dplyr)
library(sqldf)
library(ggpubr)


## ----import from downloaded raw_data--------------------------------------------------------
cdx<-read.xport("DIQ_I.XPT")
sqldata(myconn, df, tablename = "RawData", rownames = FALSE)


## ---connection-----------------------------------------------------------
raw_data<-sqlQuery(myconn, "SELECT * FROM RawData")


## ----to count na------------------------------------------------------------
na_count_list = c()
for (colname in raw_data) {
  na_count = 0
  for (val in colname) {
    if (is.na(val)) {
      na_count = na_count+1
    }
  }
  na_count_list=c(na_count_list,na_count)
}
Col_Name<-colnames(raw_data)


## ----to plot na-------------------------------------------------------------
(counts_by_col <- data.frame(cbind(names(raw_data),na_count_list))) # number of missing values by column name
count_bar <- ggplot(counts_by_col, aes(x=V1, y=na_count_list)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=na_count_list)) +
  coord_flip()

count_bar + scale_y_discrete('NaN Counts', breaks=c())+
  scale_x_discrete("Categories")


## ----to clean------------------------------------------------------------
#New column for YES
predia$Prediabetes_YES[predia$DIQ160 == 1]<-1
predia$Prediabetes_YES[is.na(predia$Prediabetes_YES)]<-0

table(predia$Prediabetes_YES) #count 513

#convert to factor
predia$Prediabetes_YES<-as.factor(predia$Prediabetes_YES)

#New column for NO
predia$Prediabetes_NO[predia$DIQ160 == 2]<-1
predia$Prediabetes_NO[is.na(predia$Prediabetes_NO)]<-0

table(predia$Prediabetes_NO) #count 5521

#convert to factor
predia$Prediabetes_NO<-as.factor(predia$Prediabetes_NO)

#New column for DONT KNOW
predia$Prediabetes_IDK[predia$DIQ160 == 9]<-1
predia$Prediabetes_IDK[is.na(predia$Prediabetes_IDK)]<-0

table(predia$Prediabetes_IDK) #count 11

#convert to factor
predia$Prediabetes_IDK<-as.factor(predia$Prediabetes_IDK)

#Rearranging columns to ensure DIQ160 are next to one another
rearr_predia<-predia[,c(1:7,58:60,8:57)]

#Dropping the original DIQ160 column as we have one-hot encoded it to handle the missing values
Prediabetes<-select(rearr_predia, -c(DIQ160))
head(Prediabetes)


## ----c4 counts-----------------------------------------------------------
#YES
sqldf("SELECT Prediabetes_YES, count(Prediabetes_YES) FROM Prediabetes GROUP BY Prediabetes_YES")

#NO
sqldf("SELECT Prediabetes_NO, count(Prediabetes_NO) FROM Prediabetes GROUP BY Prediabetes_NO")

#IDK
sqldf("SELECT Prediabetes_IDK, count(Prediabetes_IDK) FROM Prediabetes GROUP BY Prediabetes_IDK")

#Check for datatype
class(Prediabetes$Prediabetes_YES)
class(Prediabetes$Prediabetes_NO)
class(Prediabetes$Prediabetes_IDK)


## ----c5 eval-------------------------------------------------------------
#copying the dataset to a new dataframe
risk<-Prediabetes

#Count of column
sqldf("SELECT count(DIQ170) FROM risk") #Only the non-NULL values are picked up in the count

#Validating the total of null values - 3389
length(which(is.na(risk$DIQ170)))

#Assessing the datatype of this column
class(risk$DIQ170)

#Checking for all codes
table(risk$DIQ170)


## ----sp clean------------------------------------------------------------
#selecting only those with ranges
sp_range<-sqldf("SELECT DIQ230 FROM sp WHERE DIQ230 < 5")

#finding the median
median(sp_range$DIQ230)

#replacing null values with the median
sp$DIQ230[is.na(sp$DIQ230)]<-as.integer(median(sp_range$DIQ230))

#Assigning ranges
sp$DIQ230[sp$DIQ230 == 1]<-'<1'
sp$DIQ230[sp$DIQ230 == 2]<-'1-2'
sp$DIQ230[sp$DIQ230 == 3]<-'2-5'
sp$DIQ230[sp$DIQ230 == 4]<-'>5'
sp$DIQ230[sp$DIQ230 == 5]<-'Never'
sp$DIQ230[sp$DIQ230 == 7]<-'Refused'
sp$DIQ230[sp$DIQ230 == 9]<-'Dont Know'

table(sp$DIQ230)
which(is.na(sp$DIQ230))

#REnaming ther column
colnames(sp)[colnames(sp)==colnames(sp)[54]] <- toupper("last specialist visit_yrs")

head(sp)

#New column for LESS THAN 6
lev$A1CRecom_6[lev$DIQ291==1]<-1
lev$A1CRecom_6[is.na(lev$A1CRecom_6)]<-0
table(lev$A1CRecom_6)
lev$A1CRecom_6<-as.factor(lev$A1CRecom_6)

#New column for LESS THAN 7
lev$A1CRecom_7[lev$DIQ291==2]<-1
lev$A1CRecom_7[is.na(lev$A1CRecom_7)]<-0
table(lev$A1CRecom_7)
lev$A1CRecom_7<-as.factor(lev$A1CRecom_7)

#New column for LESS THAN 8
lev$A1CRecom_8[lev$DIQ291==3]<-1
lev$A1CRecom_8[is.na(lev$A1CRecom_8)]<-0
table(lev$A1CRecom_8)
lev$A1CRecom_8<-as.factor(lev$A1CRecom_8)

#New column for LESS THAN 9
lev$A1CRecom_9[lev$DIQ291==4]<-1
lev$A1CRecom_9[is.na(lev$A1CRecom_9)]<-0
table(lev$A1CRecom_9)
lev$A1CRecom_9<-as.factor(lev$A1CRecom_9)

#New column for LESS THAN 10
lev$A1CRecom_10[lev$DIQ291==5]<-1
lev$A1CRecom_10[is.na(lev$A1CRecom_10)]<-0
table(lev$A1CRecom_10)
lev$A1CRecom_10<-as.factor(lev$A1CRecom_10)

#New column for NOT SPECIFIED
lev$A1CRecom_NS[lev$DIQ291==6]<-1
lev$A1CRecom_NS[is.na(lev$A1CRecom_NS)]<-0
table(lev$A1CRecom_NS)
lev$A1CRecom_NS<-as.factor(lev$A1CRecom_NS)

#Rearranging columns to ensure DIQ291 are next to one another
rearr_lev<-lev[,c(1:68,80:85,69:79)]

#Dropping the original DIQ291 column as we have one-hot encoded it to handle the missing values
Recom_Level<-select(rearr_lev, -c(DIQ291))


## ----plot code-----------------------------------------------------------
bp<- Recom_Level

#to create plots to understand the relationship
raw_data<-bp
head(raw_data)
case_sbps <- c()
case_dbps <- c()
case_sbps_doc <- c()
case_dbps_doc <- c()

subset_cols <- colnames(raw_data[74:77])
for (entry in 1:length(raw_data$DIQ300S)) {
  sbp <- 0
  dbp <- 0
  sbp_doc <- 0
  dbp_doc <- 0
  if (raw_data[entry, subset_cols[1]] > 79 & raw_data[entry, subset_cols[1]] < 202 & !is.na(raw_data[entry, subset_cols[1]])) {
    if (raw_data[entry, subset_cols[2]] > 16 & raw_data[entry, subset_cols[2]] < 252 & !is.na(raw_data[entry, subset_cols[2]])) {
      if (raw_data[entry, subset_cols[3]] > 79 & raw_data[entry, subset_cols[3]] < 176 & !is.na(raw_data[entry, subset_cols[3]])) {
        if (raw_data[entry, subset_cols[4]] > 17 & raw_data[entry, subset_cols[4]] < 141 & !is.na(raw_data[entry, subset_cols[4]])) {
          case_sbps <- c(case_sbps, raw_data[entry, subset_cols[1]])
          case_dbps <- c(case_dbps, raw_data[entry, subset_cols[2]])
          case_sbps_doc <- c(case_sbps_doc, raw_data[entry, subset_cols[3]])
          case_dbps_doc <- c(case_dbps_doc, raw_data[entry, subset_cols[4]])
        }
      }
    }
  }
}



## ----sbp plot------------------------------------------------------------
recentsbp<-subset(bp, bp$DIQ300S < 7777 & !is.na(bp$DIQ300S))
boxplot(recentsbp$DIQ300S)

recentdbp<-subset(bp, bp$DIQ300D < 7777 & !is.na(bp$DIQ300D))
boxplot(recentdbp$DIQ300D)


## ----sbp clean-----------------------------------------------------------
bp$DIQ300S[bp$DIQ300S == 7777] <- as.integer(median(recentsbp$DIQ300S))
bp$DIQ300S[bp$DIQ300S == 9999] <- as.integer(median(recentsbp$DIQ300S))
bp$DIQ300S[is.na(bp$DIQ300S)]<-as.integer(median(recentsbp$DIQ300S))
#Renaming the column
colnames(bp)[colnames(bp)==colnames(bp)[74]] <- toupper("recent sbp")

bp$DIQ300D[bp$DIQ300D == 7777] <- as.integer(median(recentdbp$DIQ300D))
bp$DIQ300D[bp$DIQ300D == 9999] <- as.integer(median(recentdbp$DIQ300D))
bp$DIQ300D[is.na(bp$DIQ300D)]<-as.integer(median(recentdbp$DIQ300D))
#Renaming the column
colnames(bp)[colnames(bp)==colnames(bp)[75]] <- toupper("recent dbp")
head(bp)


## ----bp recom plot-------------------------------------------------------

drsbp<-subset(bp, bp$DID310S < 6666 & !is.na(bp$DID310S))
boxplot(drsbp$DID310S)

drdbp<-subset(bp, bp$DID310D < 6666 & !is.na(bp$DID310D))
boxplot(drdbp$DID310D)



## ----recom bp clean------------------------------------------------------
bp$DID310S[bp$DID310S == 6666] <- as.integer(median(drsbp$DID310S))
bp$DID310S[bp$DID310S == 7777] <- as.integer(median(drsbp$DID310S))
bp$DID310S[bp$DID310S == 9999] <- as.integer(median(drsbp$DID310S))
bp$DID310S[is.na(bp$DID310S)]<-as.integer(median(drsbp$DID310S))
#Renaming the column
colnames(bp)[colnames(bp)==colnames(bp)[76]] <- toupper("recom sbp")

bp$DID310D[bp$DID310D == 6666] <- as.integer(median(drdbp$DID310D))
bp$DID310D[bp$DID310D == 7777] <- as.integer(median(drdbp$DID310D))
bp$DID310D[bp$DID310D == 9999] <- as.integer(median(drdbp$DID310D))
bp$DID310D[is.na(bp$DID310D)]<-as.integer(median(drdbp$DID310D))
#Renaming the column
colnames(bp)[colnames(bp)==colnames(bp)[77]] <- toupper("recom dbp")


## ----sbp comp------------------------------------------------------------
df1 <- data.frame(SelfReported=case_sbps,DoctorRecommended=case_sbps_doc)
ggpaired(df1, cond1 = "SelfReported", cond2 = "DoctorRecommended",
         fill = "condition", palette = "uchicago", line.color = "black", line.size = 0.05,
         ylab = "Systolic Blood Pressure", yscale(0,200), xlab = '')


## ----dbp comp------------------------------------------------------------
df2 <- data.frame(SelfReported=case_dbps, DoctorRecommended=case_dbps_doc)
ggpaired(df2, cond1 = "SelfReported", cond2 = "DoctorRecommended",
         fill = "condition", palette = "uchicago", line.color = "black", line.size = 0.05,
         ylab = "Diastolic Blood Pressure", yscale(0,200), xlab='')


## ----did320 clean--------------------------------------------------------
ldl <- bp
table(ldl$DID320)

#New column for values
ldl$RecentLDL_Vals<-ifelse(ldl$DID320 < 5555 & !is.na(ldl$DID320), ldl$DID320, 0)
table(ldl$RecentLDL_Vals)
ldl$RecentLDL_Vals<-as.factor(ldl$RecentLDL_Vals)

#New column for never heard of LDL
ldl$RecentLDL_NEVERH<-ifelse(ldl$DID320 == 5555 & !is.na(ldl$DID320), 1, 0)
table(ldl$RecentLDL_NEVERH)
ldl$RecentLDL_NEVERH<-as.factor(ldl$RecentLDL_NEVERH)

#New column for never tested
ldl$RecentLDL_NEVERTEST<-ifelse(ldl$DID320 == 6666 & !is.na(ldl$DID320), 1, 0)
table(ldl$RecentLDL_NEVERTEST)
ldl$RecentLDL_NEVERTEST<-as.factor(ldl$RecentLDL_NEVERTEST)

#Rearranging columns to ensure DID320 are next to one another
rearr_ldl<-ldl[,c(1:78,85:87,79:84)]



## ----did330 plot---------------------------------------------------------
drldl<-subset(rearr_ldl, rearr_ldl$DID330 < 6666 & !is.na(rearr_ldl$DID330))
boxplot(drldl$DID330)
mean(drldl$DID330) #not too many outliers hence mean and not median


## ----did330 clean--------------------------------------------------------
rearr_ldl$DID330[rearr_ldl$DID330 == 6666] <-as.integer(mean(drldl$DID330))
rearr_ldl$DID330[rearr_ldl$DID330 == 7777] <-as.integer(mean(drldl$DID330))
rearr_ldl$DID330[rearr_ldl$DID330 == 9999] <-as.integer(mean(drldl$DID330))
rearr_ldl$DID330[is.na(rearr_ldl$DID330) & (ldl$RecentLDL_NEVERH==1)]<-as.integer(888)
rearr_ldl$DID330[is.na(rearr_ldl$DID330) & (ldl$RecentLDL_NEVERTEST==1)]<-as.integer(888)
rearr_ldl$DID330[is.na(rearr_ldl$DID330)]<-as.integer(mean(drldl$DID330))

#Renaming the column
colnames(rearr_ldl)[colnames(rearr_ldl)==colnames(rearr_ldl)[82]] <- toupper("recom ldl")

#Dropping the original DID320column as we have one-hot encoded it to handle the missing values
Recent_LDL<-select(rearr_ldl, -c(DID320))

head(Recent_LDL)


## ----did341 clean--------------------------------------------------------
feet<-Recent_LDL

feetsores<-subset(feet, feet$DID341 > 0 & feet$DID341 < 7777 & !is.na(feet$DID341))
boxplot(feetsores$DID341)
median(feetsores$DID341) #too many outliers so median

feet$DID341<-ifelse(feet$DID341 > -1 & feet$DID341 < 7777 & !is.na(feet$DID341), feet$DID341, median(feetsores$DID341))
length(which(is.na(feet$DID341)))

feet$DID341<-as.integer(feet$DID341)
#Renaming the column
colnames(feet)[colnames(feet)==colnames(feet)[82]] <- toupper("prof feet check")


## ----did350 clean--------------------------------------------------------

#DID350
feetcheck<-subset(feet, feet$DID350 > 0 & feet$DID350 < 7777 & !is.na(feet$DID350))
boxplot(feetcheck$DID350)
median(feetcheck$DID350) #too many outliers & wide variance so median

feet$DID350<-ifelse(feet$DID350 > -1 & feet$DID350 < 7777 & !is.na(feet$DID350), feet$DID350, median(feetcheck$DID350))
length(which(is.na(feet$DID341)))

feet$DID350<-as.integer(feet$DID350)
#Renaming the column
colnames(feet)[colnames(feet)==colnames(feet)[83]] <- toupper("self feet check")


#DIQ350U
feet$FeetSelfCheckU_DAY<-ifelse(feet$DIQ350U == 1 & !is.na(feet$DIQ350U),1,0)
table(feet$FeetSelfCheckU_DAY)

feet$FeetSelfCheckU_WEEK<-ifelse(feet$DIQ350U == 2 & !is.na(feet$DIQ350U),1,0)
table(feet$FeetSelfCheckU_WEEK)

feet$FeetSelfCheckU_MONTH<-ifelse(feet$DIQ350U == 3 & !is.na(feet$DIQ350U),1,0)
table(feet$FeetSelfCheckU_MONTH)

feet$FeetSelfCheckU_YEAR<-ifelse(feet$DIQ350U == 4 & !is.na(feet$DIQ350U),1,0)
table(feet$FeetSelfCheckU_YEAR)


#Rearranging columns to ensure DIQ350U are next to one another
rearr_feet<-feet[,c(1:84,87:90,85:86)]

#Dropping the original DIQ350U column as we have one-hot encoded it to handle the missing values
FeetSelfCheck_Unit<-select(rearr_feet, -c(DIQ350U)

## ----final table loop----------------------------------------------------

clean_data<-Eye_Diabetes
for (col in 2:length(clean_data)) {
  cat('+--------------------------+', colnames(clean_data[col]), '+---------------------------+', '\n')
  cat('DATA CLASS:', unique(class(clean_data[,col])), '\n')
  cat('TOTAL NUMBER OF CASES:', length(clean_data[,col]), '\n')
  cat('COUNT OF NA:', sum(is.na(clean_data[,col])), '\n')
  cat('\n')
  cat('UNIQUE VALUES AND COUNTS')
  print(table(clean_data[,col]))
  cat('\n', '\n')
}

sqlSave(myconn, clean_data, tablename = "z.CleanData", rownames = FALSE)
length(which(is.na(clean_data)))



