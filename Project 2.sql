select COUNT(tri_patientid),tri_name from Conditions
group by tri_name
having COUNT(tri_patientid)>100

2. What is average height of both an inpatient and an outpatient where patient¡¯s age is over 65
select avg(MEAS_VALUE) from Flowsheets
inner join
(select NEW_PAT_ENC_CSN_ID,DOB_CATEGORY from Inpatient
where DOB_CATEGORY = 'Over 64'
union
select NEW_PAT_ENC_CSN_ID,PATIENT_DOB_CATEGORY from Outpatient
where PATIENT_DOB_CATEGORY = 'Over 64') g on Flowsheets.PAT_ENC_CSN_ID =
g.NEW_PAT_ENC_CSN_ID
where DISP_NAME = 'Height'

3. What are the average height and weight of a patient suffering with
Hypertension? Calculate the BMI and then compare it with actual BMI in the data 
Average height:
select avg(MEAS_VALUE) from Flowsheets
inner join
(select * from Dx where DX_NAME like '%hypertension%') h
On Flowsheets.PAT_ENC_CSN_ID = h.NEW_PAT_ENC_CSN_ID
where DISP_NAME = 'Height'
Output: 66.7867158286778
Average weight:
select avg(MEAS_VALUE) from Flowsheets
inner join
(select * from Dx where DX_NAME like '%hypertension%') h
On Flowsheets.PAT_ENC_CSN_ID = h.NEW_PAT_ENC_CSN_ID
where DISP_NAME = 'Weight'
Output: 3157.40724615848
BMI in actual data:
select avg(MEAS_VALUE) from Flowsheets
inner join
(select * from Dx where DX_NAME like '%hypertension%') h
On Flowsheets.PAT_ENC_CSN_ID = h.NEW_PAT_ENC_CSN_IDwhere DISP_NAME = 'BMI (Calculated)'
Output: 31.1507900677201