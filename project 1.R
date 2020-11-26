library(dplyr)
hw2callenc %>%
  mutate(enrollemnt_group = ifelse(
    grepl("125060000", EncounterCode),
    "Clinical Alert",
    ifelse(grepl("125060001", EncounterCode), "Health Coaching",
           ifelse(grepl("125060002", EncounterCode), "Technical Question",
                  ifelse(grepl("125060003", EncounterCode), "Administrative",
                         ifelse(grepl("125060004", EncounterCode), "Other",
                                ifelse(grepl("125060005", EncounterCode), "Lack of
engagement",
                                       "Other")))))))

library(plyr)
count(hw21callenc, "enrollemnt_group")
calldur_pcencounter <- sqlQuery(mycon,"select A.*, B.* from CallDuration A
inner join PhoneCall_Encounter B
on A.tri_CustomerIDEntityReference=B.CustomerID
")
View(calldur_pcencouter)
head(calldur_pcencounter,10)
hw2calldur<-sqlQuery(mycon, "select * from CallDuration")
library(plyr)
count(hw2calldur, "CallType")
count(hw2calldur, "CallOutcome")
hw2q4 <- sqlQuery (mycon,
                   "select * from ((Conditions
inner join Demographics ON Conditions.tri_patientid = Demographics.contactid)
inner join Text ON Conditions.tri_patientid = Text.tri_contactId) ")
library(lubridate)
diff_in_weeks = as.numeric(difftime(max(hw2q4$TextSentDate),
                                    min(hw2q4$TextSentDate), units = "weeks"))
group = count(hw2q4, "SenderName")
group$freq/diff_in_weeks
condi_text <- sqlQuery(mycon,"select C.*, D.* from Conditions C
inner join Text D
on C.tri_patientid=D.tri_contactId
")diff_in_week = as.numeric(difftime(max(condi_text$TextSentDate),
                                     min(condi_text$TextSentDate), units = "weeks"))
group = count(condi_text, "tri_name")
group$freq/diff_in_weeks