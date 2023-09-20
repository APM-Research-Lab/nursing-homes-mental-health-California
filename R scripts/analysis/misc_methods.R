library(data.table)
library(tidyverse)

## Calculate percent unknown each year 
files <- paste0("final_processed_snapshot/", list.files(path = "final_processed_snapshot/", pattern = "*.csv"))

df <- files %>%
  map_df(~fread(., colClasses = 'character'))

df2 <- df %>% 
  group_by(year, percent_unknowns) %>%
  summarise(n())

View(df2)

# Calculate number of facilities in California that don't appear in our dataset 
# read in facility data downloaded here: https://data.chhs.ca.gov/dataset/healthcare-facility-locations
df_names_SNFs <- read.xlsx('health_facility_locations.xlsx') %>% filter(FAC_TYPE_CODE == 'SNF')

length(df_names_SNFs$FACID)

## RESULT --> 1198. Around 30 of these are STPs. So if you consider our 1146 + 30 STPs = 1176, our data does not include 
# about 20 facilities that do no appear in MDS data. 