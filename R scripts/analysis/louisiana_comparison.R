library(data.table)
library(tidyverse)

# read in Louisiana 2014 residents, calculate total number with SMI and no dementia and then divide by total for percent 
df <- read_csv('Louisiana/final_processed_snapshot_LA/2014_LA_all_residents_snapshot.csv')

Louis_2014_SMI_no_dementia <- sum(df$SMI_NO_DEMENTIA, na.rm = TRUE)
Louis_2014_SMI_no_dementia/length(df$rsdnt_intrnl_id) 

## RESULT --> 0.148 = 14.8% of Louisiana residents had serious mental illness and no dementia/alz diagnosis


# read in California 2022 residents, calculate total number with SMI and no dementia and then divide by total for percent 
df2 <- read_csv('final_processed_snapshot/2022_CA_all_residents_snapshot.csv')

Cali_2022_SMI_no_dementia <- sum(df2$SMI_NO_DEMENTIA, na.rm = TRUE)
Cali_2022_SMI_no_dementia/length(df2$rsdnt_intrnl_id) 

## RESULT --> 0.140 = 14.0% of California residents had serious mental illness and no dementia/alz diagnosis


