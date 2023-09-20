# read in libraries 
library(tidyverse)
library(data.table)

# create list of file names to loop through from folder containing data files (one for each year)
files <- paste0("documents/", list.files(path = "documents/", pattern = "*.csv"))

# get number of files 
n_for_loop <- length(files)


# loop through and process each year, pulling out California, non-STP data only 
# save each year as new file 
# !!  if memory gets exhausted, do in batches, may have to restart R and adjust first item number in loop
for(item in 1:n_for_loop){
  # extract year from file name and assign to year variable
  year <- str_extract(files[item], 'Y[:digit:]*')
  year <- str_extract(year,'[:digit:]+')
  # read in file, using character type for resident's ID number because it's inconsistent across years
  df <- fread(files[item], header = T, colClasses = list(character = 'rsdnt_intrnl_id'))
  #select California
  cali <- df %>% filter(state_cd == 'CA')
  #load STPs (special treatment programs) -- which are specially licensed for caring for 
  #mental health patients. we want to exclude these unique facilities from our analysis, so we read
  #in their medicare ID numbers and then remove them from the the data 
  #this data came from the DHCS with the following description: 
  #"Please find a spreadsheet attached that includes a historic list of STPs over the last 10 years including facilities that have closed"
  df_stps_final_CCNs <- read_csv('misc_data/STP List - Basic Info - 3.29.23.csv')
  #join MDS dataset with STP dataset on the medicare ID number
  cali <- cali %>% left_join(df_stps_final_CCNs, by = c('a0100b_cms_crtfctn_num' = 'CCN'))
  #remove rows that are STPs ('NA' facility type is kept because non-STPs are 'NA')
  cali <- cali %>% filter(facility_type != 'STP'|is.na(facility_type))
  #add column with year 
  cali <- cali %>% mutate(year = year)
  #write file
  fwrite(cali, paste0('all_assessments/CA_nonSTP/CA_nonSTP_all_asmnts_', year, '.csv'))
  #remove data from environment before looping through again 
  rm(df, df_stps_final_CCNs, cali)
}
