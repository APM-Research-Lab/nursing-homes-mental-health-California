library(data.table)
library(tidyverse)

# read in 2022 data by individual 
df <- read_csv('final_processed_snapshot/2022_CA_all_residents_snapshot.csv')


# how many people with SMI have been in for at least a year? 
# total number of SMI people (can just sum since if SMI, that variable = 1)
total_SMI <- sum(df$SMI, na.rm = TRUE)
# SMI people in the same facility for at least a year 
# use the number that have an annual assessment, which indicates they have been there for at least a year
yearly <- df %>% filter(a0310a_fed_obra_cd == 3)
yearly_SMI <- sum(yearly$SMI, na.rm = TRUE) 
# divide 
yearly_SMI/total_SMI
## RESULT == 0.55 = 55% have been in for at least a year 

## how many facilities with one year or longer SMI patient
yearly_SMI_fac <- yearly %>% filter(SMI == 1) %>% select(a0100b_cms_crtfctn_num) %>% unique()
## RESULT = over 1,000 

# Average age 
# of residents with and without SMI
df_age <- df %>% group_by(SMI) %>%
  summarise(median(c_rsdnt_age_num))

## RESULT --> 77 for no SMI, 71 for SMI

# Nearly 1 in 3 residents who’d been in a California nursing home for over a year had a serious mental illness.
# use the number that have an annual assessment, which indicates they have been 
# in the same facility for at least a year 
yearly <- df %>% filter(a0310a_fed_obra_cd == 3) 
# how many of those residents have an SMI? 
yearly_SMI <- yearly %>% filter(SMI == 1)
# take the length of the filtered yearly SMI vector divided by overall yearly vector length
length(yearly_SMI$rsdnt_intrnl_id) / length(yearly$rsdnt_intrnl_id)
## RESULT == 0.31 = 31% of long-term residents have an SMI 

# how many long-term residents with SMI had any therapy in week before assessment?
# Uses just the people with an annual assessment (meaning they have been at same 
# facility for at least a year). This is because the admission assessment might be done 
# before the patient has been there for a full week, or otherwise hasn't had the opportunity to 
# get therapy yet 
therapy <- df %>% 
  filter(a0310a_fed_obra_cd == 3) %>% 
  filter(SMI == 1) %>%
  mutate(therapy = ifelse(o0400e1_psych_thrpy_min_num > 0, 1, 0))

sum(therapy$therapy, na.rm = TRUE) / length(therapy$therapy)
## RESULT = 0.007 = 0.7% of people with SMI had any minutes of therapy


# how many residents came from hospitals? 
df_SMI <- df %>%
  filter(SMI == 1) 
df_SMI_referred_from_hospital <- df_SMI %>%
  # including acute hospitals
  # row 24: misc_data/MDS_data_dictionary.xlsx
  filter(a1800_entrd_from_txt == 3)
length(df_SMI_referred_from_hospital$rsdnt_intrnl_id)/length(df_SMI$rsdnt_intrnl_id)
## RESULT = .87 = 87% 

df_SMI_referred_from_hospital_both <- df_SMI %>%
  # including both acute hospitals and psychiatric hospitals
  # row 24:misc_data/MDS_data_dictionary.xlsx
  filter(a1800_entrd_from_txt %in% c(3,4))
length(df_SMI_referred_from_hospital_both$rsdnt_intrnl_id)/length(df_SMI$rsdnt_intrnl_id)
## RESULT = .90 = 90% 

