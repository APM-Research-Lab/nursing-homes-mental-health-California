library(tidyverse)
library(data.table)
library(openxlsx)
library(readxl)

# define function that means "not in" 
`%!in%` <- Negate(`%in%`)

## read in data downloaded from https://www.cdph.ca.gov/Programs/CHCQ/LCP/Pages/StateEnforcementActionsDashboard.aspx
# (under details tab, selected all citations using shift in the box listing all citations, then 
# at bottom of screen used download button)
# data downloaded as CSV but contained error 'embedded nul in string', so opened in excel and saved as excel file first
df_citations <- read_excel('misc_data/citations_new.xlsx', col_types = 'text') 

# fix date - have to read in data as text in case of leading zeros in ID numbers, but that makes
# for the funky excel "days after 1899" format https://stackoverflow.com/questions/43230470/how-to-convert-excel-date-format-to-proper-date-in-r
df_citations$DATE <- convertToDate(df_citations$`Penalty Issue Date`)
df_citations$violationdate <- convertToDate(df_citations$`Violation To Date`)
#df_citations$`Penalty Issue Date`<- convertToDate(df_citations$`Penalty Issue Date`)

# add leading zero to Facility ID 
df_citations$`Facility ID` <- sprintf("%09s", df_citations$`Facility ID`)

# select just skilled nursing facilities 
df_citations_SNFs <- df_citations %>% filter(`Facility Type` == 'Skilled Nursing Facility')

## FOR HYDE PARK statistic
# select just hyde park and relevant columns
hyde_park <- df_citations_SNFs %>% 
  select(`Facility Name...2`, DATE, violationdate, `Penalty Issue Date`, `Total Amount Due Final`, `Class Assessed Final`, `Class Assessed Initial`) %>%
   filter(`Facility Name...2` == 'Hyde Park Healthcare Center')

# select just more severe fines (class As)
hyde_park_As <- hyde_park %>% 
  # A trebleed is the same class as an A, it just indicates it's the third time it's happened in a year 
  # see e.g. page 20 https://canhr.org/wp-content/uploads/2021/08/Advocate_2019Q4.pdf 
  filter(`Class Assessed Final` %in% (c('A', 'AA', 'A Trebled'))) 

# summarise to see total number of penalties and resulting fines 
hyde_park_summary <- hyde_park_As %>%
   summarise(number = n(),
            sum(as.numeric(`Total Amount Due Final`)))

View(hyde_park_summary)
## RESULT --> Around 280K in fines for 14 citations.

## now for looking at high vs low SMI facilities 
# citations data only includes 'facility id' which is different from the CMS CCN 
# so we use a directory from the state that contains multiple identifiers to we can use to match everything up 
# read in facility data downloaded here: https://data.chhs.ca.gov/dataset/healthcare-facility-locations
df_names_SNFs <- read.xlsx('health_facility_locations.xlsx') %>% 
  # retain just SNFs
  filter(FAC_TYPE_CODE == 'SNF') %>%
  # retain just variables needed: facility name, city, and three different id numbers 
  select(FACNAME, OSHPD_ID, CITY, CCN, FACID)

# join together citation data and facility data on 'facility id' variable
df_citations_names <- left_join(df_citations_SNFs, df_names_SNFs, by = join_by("Facility ID" == FACID)) #%>%
  #rename(FAC_NO = OSHPD_ID)

## filter for just for more extreme citations
df_citations_names_As <- df_citations_names %>% filter(`Class Assessed Final` %in% c('AA', 'A', 'A Trebled'))

# group citations by year and facility
df_citations_names_As_summed <- df_citations_names_As %>% 
  mutate(year = year(DATE)) %>% 
  #not looking at 2023 because we don't have SMI data for 2023
  filter(year != 2023) %>%
  group_by(`Facility Name...2`, year) %>%
  mutate(citations_issued = sum(as.numeric(`Total Amount Due Final`), na.rm = TRUE),
         citations_number = n()) %>%
  select(`Facility Name...2`, `Facility Name When Issued`, citations_issued, citations_number, CCN, year, `Facility ID`) %>%
  unique()

## JOIN WITH SMI STATS

# read in facility data for all years 
files <- paste0("for publication/final_processed_snapshot_facilities/", list.files(path = "for publication/final_processed_snapshot_facilities/", pattern = "*.csv"))

df2 <- files %>%
  map_df(~fread(.)) 

# select just 2015-2022, group by facility CCN number and calculate the mean percent of SMI residents 
df_SMI <- df2 %>% filter(year >= 2015) %>%
  filter(year != 2023) %>%
  group_by(a0100b_cms_crtfctn_num) %>%
  mutate(mean_CALC_SMI_per_resident = mean(CALC_SMI_per_resident)) %>%
  select(mean_CALC_SMI_per_resident, year, a0100b_cms_crtfctn_num, `Provider Name`, PROVNAME) %>%
  # because the provider name variable has different names in different years, combine to one variable 
  mutate(Provider_Name = ifelse(is.na(PROVNAME), `Provider Name`, PROVNAME))

# select only desired variables 
df_SMI <- df_SMI %>% 
  select(a0100b_cms_crtfctn_num, mean_CALC_SMI_per_resident, Provider_Name, year) %>%
  unique() 

# combine citations data with SMI data 
df_SMI_citations <- full_join(df_SMI, df_citations_names_As_summed, by = join_by(a0100b_cms_crtfctn_num == CCN, year == year))

# identify STPs, which are present in citations data but we should remove since the story is focused on non-STPs
STPS <- read_excel('for publication/misc_data/STP List - Basic Info - 3.29.23.xlsx')
# retain only records that do not have an STP facility ID
df_SMI_citations <- df_SMI_citations %>% filter(a0100b_cms_crtfctn_num %!in% STPS$a0100b_cms_crtfctn_num)

# see which data we have citations for but no SMI information -- these are facilities where the 
# directory did not contain the CCN for use in matching up the two datasets
missing <- df_SMI_citations[is.na(df_SMI_citations$mean_CALC_SMI_per_resident),]
missing_just_facility_names <- missing %>% select(`Facility Name...2`, `Facility Name When Issued`) %>% unique()

## 24 missing facilities - let's see if we can find matches for them 

## try joining on facility name with CMS data
# make missing facility names all upper case like CMS data 
missing$`Facility Name When Issued` <- toupper(missing$`Facility Name When Issued`)

# make CMS provider name all in one variable 
df2 <- df2 %>%  mutate(Provider_Name = ifelse(is.na(PROVNAME), `Provider Name`, PROVNAME))

# left join missing data with CMS data 
found_name <- left_join(missing, df2, by = join_by("Facility Name When Issued" == "Provider_Name")) %>% 
  # facility number .y is from CMS data 
  select(`Facility Name...2`, `Facility Name When Issued`, a0100b_cms_crtfctn_num.x, a0100b_cms_crtfctn_num.y) %>%
  unique()

found <- found_name %>% 
  unique() %>%
  # select just data with available CMS CCN 
  filter(!is.na(a0100b_cms_crtfctn_num.y)) %>%
  # get rid of non-matchable CCNs
  select(-a0100b_cms_crtfctn_num.x) %>%
  rename(a0100b_cms_crtfctn_num = a0100b_cms_crtfctn_num.y) %>%
  unique()

# all of the names make sense except for kingston, which matched with a couple of other 
# that used to share a name in previous years.
# so remove extraneous kingston ones 
found <- found %>% 
  filter(a0100b_cms_crtfctn_num %!in% c('055671', '056052')) %>%
  #remove name of when issued 
  select(-`Facility Name When Issued`) %>%
  #rename column to match other data 
  mutate(Provider_Name = `Facility Name...2`)  %>%
  select(-`Facility Name...2`) %>%
  unique()

## 13 facilities found. let's look at what's still missing 
still_missing <- found_name %>% 
  unique() %>%
  filter(is.na(a0100b_cms_crtfctn_num.y)) %>%
  filter(`Facility Name...2` %!in% found$Provider_Name)  
## 2 of the still missing facilities are STPs that did not have the CCN listed in df_names
# the other 8 will not be included (out of over 1000 facilities)


## let's add back in the now-identified CCNs to the citations data and redo merge with SMI data 
# first just to look at which data had missing CCNs 
missing_from_citations <- df_citations_names_As_summed %>% filter(is.na(CCN))

df_citations_names_As_summed_v2 <- left_join(df_citations_names_As_summed, found, by = join_by("Facility Name...2" == "Provider_Name")) %>%
  # combine CCN data columns into one column 
  mutate(CCN = ifelse(is.na(CCN), a0100b_cms_crtfctn_num, CCN)) %>%
  select(-a0100b_cms_crtfctn_num)

## do integration of SMI and citations over again with revised data 
df_SMI_citations_v2 <- full_join(df_SMI, df_citations_names_As_summed_v2, by = join_by(a0100b_cms_crtfctn_num == CCN, year == year))

# remove STPs and data with missing SMI info 
df_SMI_citations_v2 <- df_SMI_citations_v2 %>% 
  filter(a0100b_cms_crtfctn_num %!in% STPS$a0100b_cms_crtfctn_num) %>%
  filter(!is.na(mean_CALC_SMI_per_resident))

#  yearly mean citations 2015-2022
df_SMI_citations_v2_50SMI_yearly <- df_SMI_citations_v2 %>%
  #   # turn NAs into 0s for doing math (NA is for rows (i.e. a facility/year) with SMI 
  #   # data and no citation data -- meaning there were no citations that year )
  mutate(citations_issued_2 = ifelse(is.na(citations_issued), 0, citations_issued)) %>%
  mutate(citations_number_2 = ifelse(is.na(citations_number), 0, citations_number)) %>%
  #   # create new column indicating if facility has more ('high') or less ('low') than 50% SMI 
  mutate(SMI = ifelse(mean_CALC_SMI_per_resident >= 0.5, 'high', 'low')) %>%
  # get total citations by SMI type and year 
  group_by(SMI, year) %>%
  summarise(total_fines = sum(citations_issued_2),
            total_citations = sum(citations_number_2),
            SMI_facilities = n()) %>%
  ungroup() %>%
  # get yearly averages 
  mutate(avg_fines = total_fines/SMI_facilities,
         avg_citations = total_citations/SMI_facilities) %>%
  group_by(SMI) %>%
  # get average of yearly averages 
  summarise(yearly_avg_fines = mean(avg_fines),
            yearly_avg_citations = mean(avg_citations))

View(df_SMI_citations_v2_50SMI_yearly)
## RESULT --> high SMI facilities have 2.5x the rate of citations as low SMI facilities 

df_SMI_citations_v2_50SMI_yearly2 <- df_SMI_citations_v2_50SMI_yearly %>% 
  select(SMI, yearly_avg_fines)
  
## EXPORT FOR GRAPHIC 
write.csv(df_SMI_citations_v2_50SMI_yearly2, 'df_SMI_citations_v2_50SMI_yearly.csv')


