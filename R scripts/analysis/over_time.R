library(tidyverse)
library(data.table)

# read in facility level data 
files <- paste0("final_processed_snapshot_facilities/", list.files(path = "final_processed_snapshot_facilities/", pattern = "*.csv"))

df <- files %>%
  map_df(~fread(., colClasses = 'character'))

# sum the number of total residents and the number of people with serious mental 
# illness for each year, and then calculate percentages 
# first for the entire state
over_time_yearly_statewide <- df %>% group_by(year) %>% 
  summarize(CALC_SMI = sum(as.numeric(CALC_SMI), na.rm = TRUE),
            CALC_SMI_no_DEM = sum(as.numeric(CALC_SMI_NO_DEMENTIA), na.rm = TRUE),
            psychotic = sum(as.numeric(psychotic), na.rm = TRUE),
            bipolar = sum(as.numeric(bipolar), na.rm = TRUE),
            schiz = sum(as.numeric(schiz), na.rm = TRUE),
            all = sum(as.numeric(total_residents_dec_31))) %>%
  mutate(percent_CALC_SMI = CALC_SMI/all*100,
         percent_CALC_SMI_no_DEM = CALC_SMI_no_DEM/all*100,
         percent_psychotic = psychotic/all*100,
         percent_bipolar = bipolar/all*100,
         percent_schiz = schiz/all*100) %>%
  ungroup() %>%
  mutate(across(percent_CALC_SMI:percent_schiz, round, 1))%>%
  mutate(Location = "Statewide")

# and then for LA County only 
over_time_yearly_LA <- df %>% 
  # the CMS directory has different variable names for the county field in different years
  filter(`Provider County Name` == 'Los Angeles' | COUNTY_NAME == 'Los Angeles' | County_name == 'Los Angeles') %>%   
  group_by(year) %>% 
  summarize(CALC_SMI = sum(as.numeric(CALC_SMI), na.rm = TRUE),
            CALC_SMI_no_DEM = sum(as.numeric(CALC_SMI_NO_DEMENTIA), na.rm = TRUE),
            psychotic = sum(as.numeric(psychotic), na.rm = TRUE),
            bipolar = sum(as.numeric(bipolar), na.rm = TRUE),
            schiz = sum(as.numeric(schiz), na.rm = TRUE),
            all = sum(as.numeric(total_residents_dec_31))) %>%
  mutate(percent_CALC_SMI = CALC_SMI/all*100,
         percent_CALC_SMI_no_DEM = CALC_SMI_no_DEM/all*100,
         percent_psychotic = psychotic/all*100,
         percent_bipolar = bipolar/all*100,
         percent_schiz = schiz/all*100) %>%
  ungroup() %>%
  mutate(across(percent_CALC_SMI:percent_schiz, round, 1))%>%
  mutate(Location = "LA County")

# combine statewide and LA into one dataset 
combined <- full_join(over_time_yearly_statewide, over_time_yearly_LA)

# select just those variables of more interest 
combined_select_percent <- combined %>%
  select(year, Location, percent_psychotic, percent_bipolar, percent_schiz, percent_CALC_SMI, percent_CALC_SMI_no_DEM) 
View(combined_select_percent)

combined_select_number <- combined %>%
  select(year, Location, psychotic, bipolar, schiz, CALC_SMI, CALC_SMI_no_DEM) 
View(combined_select_number)

# for graphic 
combined_final <- combined %>% 
  mutate(all = format(all, big.mark = ',')) %>%
  select(year, Location, percent_psychotic, percent_bipolar, percent_schiz, percent_CALC_SMI, all, psychotic, bipolar, schiz, CALC_SMI)

write.csv(combined_final, 'over_time_graphic.csv')
