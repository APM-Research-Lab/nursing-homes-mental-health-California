library(data.table)
library(tidyverse)

# read in 2022 data by facility 
df <- read_csv('final_processed_snapshot_facilities/2022_CA_all_facilities_snapshot.csv')

# select just hyde park 
hyde_park <- df %>% filter(`Provider Name` == 'HYDE PARK HEALTHCARE CENTER') 

# RESULT
# print proportion of patients with SMI 

hyde_park$CALC_SMI_per_resident
## = 0.74 = 74% 

