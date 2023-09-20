library(tidyverse)
library(data.table)

# create list of file names to loop through from folder containing data files (one for each year)
files <- paste0("documents/", list.files(path = "documents", pattern = "*.csv"))

# get number of files 
n_for_loop <- length(files)

# loop through and process each year, pulling out California, non-STP data only 
# save each year as new file 
# !!  if memory gets exhausted, do in batches, may have to restart R and adjust first item number in loop

for(item in 3:n_for_loop){
  # extract year from file name and assign to year variable
  year <- str_extract(files[item], 'Y[:digit:]*')
  year <- str_extract(year,'[:digit:]+')
  # read in file, using character type for resident's ID number because it's inconsistent across years
  df <- fread(files[item], header = T, colClasses = list(character = 'rsdnt_intrnl_id'))
  #select Lousiana
  louisiana <- df %>% filter(state_cd == 'LA')
  #according to Louisiana health dept, there are no STP-like facilities in the state, 
  #so no special facilities to remove
  #add column with year 
  louisiana <- louisiana %>% mutate(year = year)
  #write file
  fwrite(louisiana, paste0('all_assessments/LA_nonSTP/LA_nonSTP_all_asmnts_', year, '.csv'))
  #remove data from environment before looping through again 
  rm(df, louisiana)
}
