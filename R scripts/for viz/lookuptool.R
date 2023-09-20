library(tidyverse)
library(data.table)

df <- read_csv('final_processed_snapshot_facilities/2022_CA_all_facilities_snapshot.csv')

## select just variables of interest 
df_tool <- df %>% select(`Provider Name`, `Provider Address`, `Provider City`, `Provider County Name`, CALC_SMI_per_resident, SMI_NO_DEMENTIA_per_resident, SMI_dementia_per_resident, percent_SMI_therapy, average_age, a0100b_cms_crtfctn_num, total_residents_dec_31, `Provider Zip Code`) %>%
  # format percentages, mask any SMI % above 75 as ">75%"
  mutate(across(CALC_SMI_per_resident:percent_SMI_therapy, ~ .*100))%>%
  mutate(across(CALC_SMI_per_resident:average_age, round, 0))%>%
  mutate(CALC_SMI_per_resident = ifelse(CALC_SMI_per_resident>75, "\\>75", CALC_SMI_per_resident)) %>%
  mutate(SMI_NO_DEMENTIA_per_resident = ifelse(SMI_NO_DEMENTIA_per_resident>75, "\\>75", SMI_NO_DEMENTIA_per_resident)) %>% 
  mutate(SMI_dementia_per_resident = ifelse(SMI_dementia_per_resident>75, "\\>75", SMI_dementia_per_resident)) %>% 
  mutate(across(CALC_SMI_per_resident:percent_SMI_therapy, ~ paste0(.,"%"))) %>%
  # rename columns 
  rename(CCN = a0100b_cms_crtfctn_num, 
         `% of residents with an SMI` = CALC_SMI_per_resident,
         `% of residents with an SMI and no dementia diagnosis` = SMI_NO_DEMENTIA_per_resident, 
         `% of residents with an SMI and a dementia-related diagnosis` = SMI_dementia_per_resident,
         `Average resident age` = average_age, 
         `% of residents with an SMI who received recent pyschotherapy` = percent_SMI_therapy) %>%
  # remove rows where provider name is unknown (n = 10)
  filter(!is.na(`Provider Name`)) %>%
  # format provider information in title case 
  mutate(across(1:3, ~ str_to_title(.))) %>%
  # add asterisk to two facilities that have applied for STP license
  mutate(`Provider Name` = ifelse(`Provider Name` %in% c('Country Villa Plaza Convalescent Center', 'Heritage Park Nursing Center'), paste0(`Provider Name`,'*'), `Provider Name`))

write.csv(df_tool, 'lookuptool.csv')

