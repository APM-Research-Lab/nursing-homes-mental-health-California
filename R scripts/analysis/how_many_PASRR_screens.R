library(tidyverse)
library(data.table)

# read in 2022 CA residents 
df <- read_csv('final_processed_snapshot/2022_CA_all_residents_snapshot.csv')

# to see full landscape of mental illness and dementia and how they were screened, create new 
# columns that specify mental health diagnosis status 
df2 <-  df %>% 
  mutate(other_MI_dementia = ifelse((i5700_anxty_dsordr_cd == 1 | i5800_dprsn_cd == 1 | i6100_ptsd_cd == 1) & SMI != 1 & (i4200_alzhmr_cd == 1 | i4800_dmnt_cd == 1), 1, 0)) %>%
  mutate(other_MI_NO_dementia = ifelse((i5700_anxty_dsordr_cd == 1 | i5800_dprsn_cd == 1 | i6100_ptsd_cd == 1) & SMI != 1 & i4200_alzhmr_cd != 1 & i4800_dmnt_cd != 1, 1, 0)) %>%
  mutate(just_dementia = ifelse(SMI != 1 & other_MI_NO_dementia != 1 & (i4200_alzhmr_cd == 1 | i4800_dmnt_cd == 1), 1, 0)) %>%
  mutate(none = ifelse(SMI != 1 & other_MI_NO_dementia != 1 & i4200_alzhmr_cd != 1 & i4800_dmnt_cd != 1, 1, 0)) %>%
  mutate(SMI_status = case_when((SMI == 1 & (i4200_alzhmr_cd == 1 | i4800_dmnt_cd == 1) ~ 'SMI + Dementia'),
                                SMI_NO_DEMENTIA == 1 ~ 'SMI and No Dementia', 
                                other_MI_dementia == 1 ~ 'Other MI + Dementia',
                                other_MI_NO_dementia == 1 ~ 'Other MI and No Dementia',
                                just_dementia == 1 ~ 'Just Dementia',
                                none == 1 ~ 'No MI or Dementia'))


PASRR_screens <- df2 %>% 
  filter(SMI_status %in% c('SMI and No Dementia', 'SMI + Dementia')) %>%
  group_by(a1500_pasrr_cd, a1510a_srus_mentl_ill_cd) %>%
  summarise(number = n())

View(PASRR_screens)

# key for PASRR screens (variable: a1500_pasrr_cd):
# 0 : PASRR Level I screening did not result in a referral for Level II screening, 
# or  — Level II screening determined that the resident does not have a serious MI and/or ID/DD or related conditions, 
# or — PASRR screening is not required because the resident was admitted from a hospital after requiring acute inpatient care, 
# is receiving services for the condition for which they received care in the hospital, and the attending physician has certified 
# before admission that the resident is likely to require less than 30 days of nursing home care.  
# 1 :  if PASRR Level II screening determined that the resident has a serious mental illness and/or ID/DD or related condition, 
# and continue to A1510, Level II Preadmission Screening and Resident Review (PASRR) Conditions. 

# SO, sometimes a Level I screening can "not result in a referral for Level II" because there 
# is an exception for people with dementia. therefor, the "SMI + no dementia" is the 
# most reasonable diagnosis category to look at #. 

PASRR_screens_no_dementia <- df2 %>% 
  filter(SMI_status == 'SMI and No Dementia') %>%
  group_by(a1500_pasrr_cd, a1510a_srus_mentl_ill_cd) %>%
  summarise(number = n())

View(PASRR_screens_no_dementia)

# And, because of the possibility that some people with mental illness may not have gotten a 
# Level II screening because the resident is likely to require less than 30 days of nursing 
# home care, we look only at those long-term residents in our census receiving their annual assessment, 
# indicating they have been in the same facility for at least the last year. 

PASRR_screens_long_term <- df2 %>% 
  filter(SMI_status == 'SMI and No Dementia') %>%
  filter(a0310a_fed_obra_cd == 3) %>%
  group_by(a1500_pasrr_cd, a1510a_srus_mentl_ill_cd) %>%
  summarise(number = n())

View(PASRR_screens_long_term)

# Result --> Of this population, a1500_pasrr_cd = 0 for 4,764 residents. Using the follow-up variable a1510a_srus_mentl_ill_cd, 
# indicating whether a resident's Level II screening indicated the resident had a serious mental illness, 
# there were 1,359 residents with a serious mental illness diagnosis, no dementia diagnosis, and a 
# confirmed serious mental illness via PASRR 


# Just for additional information, what are the results for admissions assessments and those with corrected assessments? 

PASRR_screens_admissions_and_corrected <- df2 %>% 
  filter(SMI_status == 'SMI and No Dementia') %>%
  filter(a0310a_fed_obra_cd != 3) %>%
  group_by(a1500_pasrr_cd, a1510a_srus_mentl_ill_cd) %>%
  summarise(number = n())

View(PASRR_screens_admissions_and_corrected)

# Result --> Of this population, a1500_pasrr_cd = 0 for another nearly 4,000 residents. Using the follow-up variable a1510a_srus_mentl_ill_cd, 
# indicating whether a resident's Level II screening indicated the resident had a serious mental illness, 
# there were 1,531 residents with a serious mental illness diagnosis, no dementia diagnosis, and a 
# confirmed serious mental illness via PASRR 


## are there people that have a dementia-related diagnosis AND PASRR SMI diagnosis? 
PASRR_screens_dementia <- df2 %>% 
  filter(SMI_status %in% c('SMI + Dementia','Other MI + Dementia', 'Just Dementia')) %>%
  #filter(a0310a_fed_obra_cd != 3) %>%
  group_by(a1500_pasrr_cd, a1510a_srus_mentl_ill_cd) %>%
  summarise(number = n())

View(PASRR_screens_dementia)

# Result --> Of this population, which may not be expected to have a PASRR SMI diagnosis due 
# to dementia diagnosis, there were 2,401 residents with a dementia diagnosis as well as 
# as PASRR SMI indicator 

## what about an MDS SMI diagnosis, a dementia-related diagnosis AND PASRR SMI diagnosis? 
PASRR_screens_dementia_SMI <- df2 %>% 
  filter(SMI_status == 'SMI + Dementia') %>%
  #filter(a0310a_fed_obra_cd != 3) %>%
  group_by(a1500_pasrr_cd, a1510a_srus_mentl_ill_cd) %>%
  summarise(number = n())

View(PASRR_screens_dementia_SMI)

# Result --> Of this population, which may not be expected to have a PASRR SMI diagnosis due 
# to dementia diagnosis, there were 1,618 residents with an SMI and dementia diagnosis, as well as 
# as PASRR SMI indicator 

# and how about total SMI + dementia patients, not including PASRR 
SMI_dementia_total <- df2 %>% filter(SMI_status == 'SMI + Dementia')
length(SMI_dementia_total$rsdnt_intrnl_id)
