----First join demographics and conditions
select * into z.democond from z.Demographics
inner join z.Conditions on z.Demographics.ID = z.Conditions.tri_patientid;

----Then join previous merged tables and text so to get the finaltable which include all three tables
select * into z.finaltable from z.democond
inner join text on z.democond.ID= z.textmessage.tri_contactId;

----Next to execute max() to find the lastest Sent date after identifying all the unique ids
select ID as IDunique,
max(Sentdate) as lasttextdate into z.LastTextID1 from z.finaltable
group by ID;

----Finally store the result in the output table
select distinct * into z.FinalOutput from z.finaltable;
select top 10 * from z.FinalOutput;











