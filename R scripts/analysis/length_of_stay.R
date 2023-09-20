library(tidyverse)
library(data.table)
`%!in%` <- Negate(`%in%`)

##census 2022 residents - read in specifying date type for second column, character for other initial columns (ID numbers). and default for the rest
df_census_2022 <- read_csv('final_processed_snapshot/2022_CA_all_residents_snapshot.csv', col_types  = 'cDccc')

#read in all California assessments for all years - filtering for just those resident IDs that appear in our 2022 resident census
files <- paste0("all_assessments/CA_nonSTP/", list.files(path = "all_assessments/CA_nonSTP/", pattern = "*.csv"))

df <- files %>%
  map_df(~fread(., colClasses = 'character') %>% filter(rsdnt_intrnl_id %in% df_census_2022$rsdnt_intrnl_id))

#format dates 
df$date <- as.character(df$trgt_dt)
df$date <- as.Date(df$date, format = '%Y%m%d')

df$entry_date <- as.character(df$a1600_entry_dt)
df$entry_date <- as.Date(df$entry_date, format = '%Y%m%d')

# sort by date and ID to preview data if desired 
df <- df %>% arrange(rsdnt_intrnl_id, date) %>% select(date, entry_date, a1700_entry_type_cd, everything())

# get rid of unneeded columns to reduce dataset size 
df_census_2022_all_assessments <- df %>% select(c(1:43))

# remove larger version dataset to free up disk space 
rm(df)
glimpse(df)
# create Dec 31 2022 as a date type value 
dec_31_2022 <- as.Date('12-31-2022', '%m-%d-%Y')

# find the first entry for all Dec 31 residents 
df_census_2022_first_entry <- df_census_2022_all_assessments %>% 
  # this filters for just those that are original dates of admissions, not 're-entries' - which are those 
  # instances when someone was discharged with return anticipated, 
  # and then they returned within 30 days 
  filter(a1700_entry_type_cd == 1) %>%
  # select just the date, ID 
  select(entry_date, rsdnt_intrnl_id) %>%
  # drop NAs and duplicate entries now that other variables are removed 
  drop_na() %>%
  unique() %>%
  # group by resident ID to get most recent entry date 
  group_by(rsdnt_intrnl_id) %>%
  filter(entry_date == max(entry_date)) %>%
  # join with 2022 census and use the SMI value from that record 
  left_join(df_census_2022) %>%
  #filter(!is.na(date)) %>%
  mutate(length_of_stay = dec_31_2022 - entry_date) %>%
  select(length_of_stay, everything())

length_by_SMI <- df_census_2022_first_entry %>%
  group_by(SMI) %>%
  summarise(mean(length_of_stay),
            n()) 

# SMI = 1 if they have a serious mental illness
View(length_by_SMI)

length_by_SMI <- length_by_SMI %>% 
  drop_na() %>%
  select(-`n()`) %>%
  pivot_wider(names_from = SMI, values_from = `mean(length_of_stay)`) %>%
  mutate(SMI_days_minus_nonSMI_days = `1` - `0`)


## RESULT --> Residents with SMI have an average length of stay 282 days longer 
# than residents without an SMI (or divide by 30.417 days per month to get ~ 9 months)