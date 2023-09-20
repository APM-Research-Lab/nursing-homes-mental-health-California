library(tidyverse)
library(data.table)


# The DHCS analysis provided to us as a response to our data looks only at data from SNF placements, not from residents currently residing in nursing homes

# We do not have the full PASRR records and cannot directly replicate their analysis with our data.
# We do, however, have data that allows us to replicate their final finding, that 
# 'True presence of SMI was then identified in 12,968 individuals' 

# load all 2022 assessments
df_2022_all_assessments <- read_csv('all_assessments/CA_nonSTP/CA_nonSTP_all_asmnts_2022.csv')

# isolate just those that 1) require PASRR and 2) are related to placement, so i.e. admissions OBRA assessments
df_2022_entry_only <- df_2022_all_assessments %>% filter(a0310a_fed_obra_cd == 1)

# calculate the number of admissions assessments that detect "true presence" of SMI via PASRR 
df_2022_PASRR_yes <- df_2022_entry_only %>% filter(a1510a_srus_mentl_ill_cd == 1)
## RESULT --> 12,850 residents, nearly identical to what the state found 

# what percent of admissions assessments indicate via PASRR that a resident has an SMI? 
length(df_2022_PASRR_yes$mds_asmt_id)/length(df_2022_entry_only$mds_asmt_id)
## RESULT --> 6% 

 