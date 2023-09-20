library(tidyverse)
library(data.table)

# read in facility level data 
files <- paste0("final_processed_snapshot_facilities/", list.files(path = "final_processed_snapshot_facilities/", pattern = "*.csv"))

df <- files %>%
  map_df(~fread(., colClasses = 'character'))

# filter for just those facilities where the provider name is missing
NAs2022 <- df %>% filter(year == 2022) %>% filter(is.na(`Provider Name`))

length(NAs2022$a0100b_cms_crtfctn_num)
## RESULT --> 10 facilities

# tried to determine what facilities they might be, but didn't feel certain enough
# to include in maps or lookup tool

