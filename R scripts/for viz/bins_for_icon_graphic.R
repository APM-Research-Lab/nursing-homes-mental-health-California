library(tidyverse)
library(data.table)
`%!in%` <- Negate(`%in%`)


## load in 2022 facility-level data 
df_dec_31 <- read_csv('final_processed_snapshot_facilities/2022_CA_all_facilities_snapshot.csv')

df_bins <- df_dec_31 %>% mutate(bins = case_when(CALC_SMI_per_resident < 0.5 ~ '0-49%',
                                                 CALC_SMI_per_resident < 0.6 ~ '50-59%',
                                                 CALC_SMI_per_resident < 0.7 ~ '60-69%',
                                                 CALC_SMI_per_resident < 0.8 ~ '70-79%',
                                                 CALC_SMI_per_resident < 0.9 ~ '80-89%',
                                                 CALC_SMI_per_resident < 1 ~ '90-98%'))

df_bins_people <- df_bins %>% group_by(bins) %>%
  select(total_residents_dec_31, bins) %>%
  summarise(bin_total = sum(total_residents_dec_31),
            number_facilities = n()) %>%
  filter(bins != '0-49%')


write.csv(df_bins_people, 'binned_data_people.csv')  
