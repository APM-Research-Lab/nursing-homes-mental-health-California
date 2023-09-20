library(tidyverse)

# read in 2022 data by facility 
df <- read_csv('final_processed_snapshot_facilities/2022_CA_all_facilities_snapshot.csv')

# how many total patients with SMI? 
total_SMI <- sum(df$CALC_SMI)
total_SMI
## RESULT = 21,933

# all residents minus residents with SMI 
all_residents <- sum(df$total_residents_dec_31)
all_residents-total_SMI

## LA COUNTY 
df_LA <- df %>% filter(`Provider County Name` == 'Los Angeles')

# how many total patients with SMI? 
total_SMI <- sum(df_LA$CALC_SMI)
total_SMI
## RESULT = 21,933

# all residents minus residents with SMI 
all_residents <- sum(df_LA$total_residents_dec_31)
all_residents-total_SMI

