library(data.table)
library(tidyverse)

# read in 2022 data by facility 
df <- read_csv('final_processed_snapshot_facilities/2022_CA_all_facilities_snapshot.csv')

#remove facility with 100% SMI that is just 1 resident and we think is an error 
df <- df %>% filter(a0100b_cms_crtfctn_num != '050335')

# in order of footnotes 

# how many facilities with SMI patients? 
df_facilities_greater_than_zero_SMI <- df %>% filter(CALC_SMI > 0)
dim(df_facilities_greater_than_zero_SMI)
## RESULT = 1,086 facilities 

# how many total patients with SMI? 
total_SMI <- sum(df$CALC_SMI)
total_SMI
## RESULT = 21,932

# how many facilities with 50% or more? 
df_facilities_greater_than_50_percent <- df %>% filter(CALC_SMI_per_resident >= 0.5)
dim(df_facilities_greater_than_50_percent)
## RESULT = 94 facilities 

# how many facilities with 90% or more? 
df_facilities_greater_than_90_percent <- df %>% filter(CALC_SMI_per_resident >= 0.90)
dim(df_facilities_greater_than_90_percent)
## RESULT = 10 facilities 

# percent of total residents with SMI? 
total_SMI / sum(df$total_residents_dec_31)
## RESULT = 0.25 = 25%

# percent of LA County residents with SMI? 
df_counties <- df %>% 
  #group by county, then get sum of SMI residents, total residents, and percent
  group_by(`Provider County Name`) %>%
  summarise(total_SMI = sum(CALC_SMI),
            number_residents_dec_31 = sum(total_residents_dec_31),
            percent = total_SMI/number_residents_dec_31) %>%
  #arrange in descending order of percent SMI
  arrange(desc(percent))
# see the top 6 counties in the console
head(df_counties)
## RESULT = 0.31 for LA County - San Diego is next at 0,29

# how many facilities with 80% or more? 
df_facilities_greater_than_80_percent <- df %>% filter(CALC_SMI_per_resident >= 0.8)
dim(df_facilities_greater_than_80_percent)
## RESULT = 33 facilities 


