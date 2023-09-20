library(tidyverse)
library(data.table)

# define function that means "not in" 
`%!in%` <- Negate(`%in%`)

# create list of file names to loop through from folder containing data files (one for each year)
files <- paste0("all_assessments/LA_nonSTP/", list.files(path = "all_assessments/LA_nonSTP/", pattern = "*.csv"))

# get number of files 
n_for_loop <- length(files)

# loop for each year / file          
for(item in 1:n_for_loop){ 
  #read in file
  df <- read_csv(files[item])
  #extract year as object 
  year <- df$year[1]
  #transform date to date data type and create month column  
  df$date <- as.character(df$trgt_dt)
  df$date <- as.Date(df$date, '%Y%m%d')
  df <- df %>% select(date, everything())
  df$month <- month(df$date)
  
  ### IDENTIFY CENSUS OF PATIENTS IN A FACILITY ON 12/31 AND 
  ### RETAIN ONLY THEIR LATEST-DATED COMPREHENSIVE ASSESSMENT
  # (Data contains more than one assessment per person per year, 
  # as well as assessments that do not contain patients' diagnoses)
  
  ## FIRST, IDENTIFY ALL PEOPLE WHOSE FINAL ENTRY IS A DISCHARGE 
  all_people_final_date_is_discharge <- df %>%
    # group by individual ID number
    group_by(rsdnt_intrnl_id) %>%
    # select just the maximum date associated with each individual ID
    filter(date == max(date)) %>% 
    # discharge codes in row 12: https://docs.google.com/spreadsheets/d/1lQjJjzLAtyHUk9eX01CWkczdCa7gcsf8uhpwOhczVUk/edit#gid=1168402967
    # retain just those in who have a discharge code on their max date
    filter(a0310f_entry_dschrg_cd %in% c(10, 11, 12)) %>%
    # select just the resident IDs
    select(rsdnt_intrnl_id) %>%
    unique()
  
  ## FIND ALL ASSESSMENTS THAT ARE COMPREHENSIVE 
  ##(i.e. OBRA assessments that have PASRR and other diagnosis info)
  df_all_obras <- df %>% filter(a0310a_fed_obra_cd %in% c(1, 3, 4, 5))
  
  ## REMOVE ASSESSMENTS FOR THOSE PEOPLE WHOSE FINAL ENTRY IS A DISCHARGE 
  # what remains are those who are still in a nursing facility on Dec 31
  df_all_obras_minus_discharged <- df_all_obras %>% filter(rsdnt_intrnl_id %!in% all_people_final_date_is_discharge$rsdnt_intrnl_id)
  
  ## RETAIN THE LATEST-DATED COMPRHENSIVE ASSESSMENT FOR EACH PERSON
  df_all_obras_final <- df_all_obras_minus_discharged %>%
    group_by(rsdnt_intrnl_id) %>%
    filter(date == max(date))
  
  ## ADD IN PEOPLE WHO DID NOT GET AN OBRA ASSESSMENT BUT ARE IN NURSING HOMES ON 12/31/2021 
  ## and HAVE ICD DIAGNOSIS CODE (consistent with also having other diagnosis codes but more reliable)
  df_all_minus_discharge_minus_obra <- df %>% 
    # retain people who are not in obra list 
    filter(rsdnt_intrnl_id %!in% df_all_obras_final$rsdnt_intrnl_id) %>%
    # remove people whose final entry is a discharge 
    filter(rsdnt_intrnl_id %!in% all_people_final_date_is_discharge$rsdnt_intrnl_id) %>%
    # remove people with missing diagnosis codes 
    filter(!is.na(i8000a_icd_1_cd)) %>%
    # retain latest-dated assessment for each person 
    group_by(rsdnt_intrnl_id) %>%
    filter(date == max(date))
  
  ## COMBINE obra data and the non-obra diagnosis data
  all_residents_final <- bind_rows(df_all_obras_final, df_all_minus_discharge_minus_obra)
  
  ## who is left? 
  df_remainders <- df %>%
    #take original dataset and remove people in final dataset with diagnoses 
    filter(rsdnt_intrnl_id %!in% all_residents_final$rsdnt_intrnl_id) %>%
    #remove people who were discharged before 12/31
    filter(rsdnt_intrnl_id %!in% all_people_final_date_is_discharge$rsdnt_intrnl_id) %>%
    # get max date per resident (also achieves getting unique ID numbers)
    group_by(rsdnt_intrnl_id) %>%
    filter(date == max(date))
  
  # get number of people with unknown diagnoses
  number_unknown <- length(unique(df_remainders$rsdnt_intrnl_id))  
  
  # calculate percent of people with unknown diagnoses per total population and add as column 
  all_residents_final <- all_residents_final %>% 
    mutate(unknowns = number_unknown) %>%
    mutate(percent_unknowns = round(number_unknown/(number_unknown + length(all_residents_final$rsdnt_intrnl_id))*100, 1))
  
  ### CALCULATIONS 
  ## calculate the number of people with SMI 
  # first convert relevant columns from characters to numbers 
  # PASRR serious mental illness check box
  all_residents_final$a1510a_srus_mentl_ill_cd <- as.numeric(all_residents_final$a1510a_srus_mentl_ill_cd)
  #Bipolar
  all_residents_final$i5900_mnc_dprsn_cd <- as.numeric(all_residents_final$i5900_mnc_dprsn_cd)
  #Psychotic disorder
  all_residents_final$i5950_psychtc_cd <- as.numeric(all_residents_final$i5950_psychtc_cd)
  #Schizophrenia 
  all_residents_final$i6000_schzoprnia_cd <- as.numeric(all_residents_final$i6000_schzoprnia_cd)
  #Alzheimers 
  all_residents_final$i4200_alzhmr_cd <- as.numeric(all_residents_final$i4200_alzhmr_cd)
  #Dementia 
  all_residents_final$i4800_dmnt_cd <- as.numeric(all_residents_final$i4800_dmnt_cd)
  
  # create new columns indicating various mental health status by diagnosis codes 
  all_residents_final <- all_residents_final %>% 
    # create new column 'SMI' that uses a 1 to signify that someone has bipolar, schizophrenia, 
    # and/or psychotic disorder - this first one is the main metric used in story (Serious Mental Illness = SMI)
    mutate(SMI = ifelse((i5900_mnc_dprsn_cd == 1 | i5950_psychtc_cd == 1 | i6000_schzoprnia_cd == 1), 1, 0),
           # create new column that uses a 1 to signify that someone has bipolar and/or schizophrenia
           bip_schz = ifelse((i5900_mnc_dprsn_cd == 1 | i6000_schzoprnia_cd == 1),1,0),
           # create new column that uses a 1 to signify that someone has bipolar, schizophrenia, 
           # and/or psychotic disorder AND does NOT have dementia or alzeihmers 
           SMI_NO_DEMENTIA = ifelse((SMI == 1 & i4200_alzhmr_cd == 0 & i4800_dmnt_cd == 0), 1, 0),
           # same thing for just bipolar and schizophrenia
           bip_schz_NO_DEMENTIA = ifelse((bip_schz == 1 & i4200_alzhmr_cd == 0 & i4800_dmnt_cd == 0), 1, 0))
  
  # convert therapy minutes to numeric column 
  all_residents_final$o0400e1_psych_thrpy_min_num <- as.numeric(all_residents_final$o0400e1_psych_thrpy_min_num)
  
  # add new column indicating if resident is under 65
  all_residents_final <- all_residents_final %>% 
    mutate(age_under_65 = ifelse(c_rsdnt_age_num < 65, 1, 0))
  
  ## export all residents snapshot as csv 
  write.csv(all_residents_final, paste0('Louisiana/final_processed_snapshot_LA/', year, '_LA_all_residents_snapshot.csv'))
  
  
  ## CALCULATIONS BY FACILITY           
  all_residents_final_grouped_summed <- all_residents_final %>%   
    # group by facility ID number 
    group_by(a0100b_cms_crtfctn_num) %>% 
    # calculations, starting with adding up the number of people with an SMI
    # per PASRR checkbox (since 1 = yes, summing gives number of 'yeses')
    summarise(PASRR_SMI = sum(a1510a_srus_mentl_ill_cd, na.rm = TRUE),
              # sum people with SMI
              CALC_SMI = sum(SMI, na.rm = TRUE),
              # sum people with SMI and no dementia
              CALC_SMI_NO_DEMENTIA = sum(SMI_NO_DEMENTIA, na.rm = TRUE),
              # get total number of residents in census for the eyar 
              total_residents_dec_31 = sum(n()),
              # calculate % of residents with PASRR SMI
              PASRR_SMI_per_resident = PASRR_SMI/total_residents_dec_31,
              # calculate % of residents with our definition of SMI
              CALC_SMI_per_resident = CALC_SMI/total_residents_dec_31,
              # calculate % of residents with our definition of SMI who do not have dementia
              SMI_NO_DEMENTIA_per_resident = CALC_SMI_NO_DEMENTIA/total_residents_dec_31,
              # calculate average age of residents at facility 
              average_age = median(c_rsdnt_age_num),
              # calculate percent of residents under 65 at facility
              percent_below_65 = sum(age_under_65)/sum(n())*100,
              # calculate total number of minutes of therapy logged at facility in assessments
              minutes_therapy_total = sum(o0400e1_psych_thrpy_min_num, na.rm = TRUE),
              # calculate number of minutes of therapy logged per number of patients with SMI
              minutes_therapy_per_CALC_SMI = sum(o0400e1_psych_thrpy_min_num, na.rm = TRUE)/CALC_SMI,
              # calculate number of minutes of therapy logged per number of patients with SMI and NO dementia
              minutes_therapy_per_CALC_SMI_NO_DEMENTIA = sum(o0400e1_psych_thrpy_min_num, na.rm = TRUE)/CALC_SMI_NO_DEMENTIA) %>%
    # save year in column 
    mutate(year = year) %>%
    select(everything(), year)
  
  ## join with information about facilities from CMS directory 
  # yearly data is available starting in 2016, so for 2013-2016 the 2016 data is used 
  # (any missing data due to this is fairly inconsequential because we don't use facility 
  # 'metadata' for those early years in anything)
  
  if (year %in% c(2013:2016)) {
    # read in CMS directory
    cms_list <- read_csv('CMS_list/NH_ProviderInfo_2016.csv') %>% 
      # select only some variables (name, location, quality metrics, number of beds)
      select(1:12, 23)
    
    # join together 
    all_residents_SMI_totals_CMS_list <- left_join(all_residents_final_grouped_summed, cms_list,
                                                   by = c("a0100b_cms_crtfctn_num" = "provnum"))
  }
  if (year == 2017){
    CMS_file <- paste0('CMS_list/NH_ProviderInfo_',year,'.csv')
    cms_list <- read_csv(CMS_file) %>% select(1:12, 23)
    all_residents_SMI_totals_CMS_list <- left_join(all_residents_final_grouped_summed, cms_list,
                                                   by = c("a0100b_cms_crtfctn_num" = "provnum"))
  }
  if (year %in% c(2018, 2019)){
    CMS_file <- paste0('CMS_list/NH_ProviderInfo_',year,'.csv')
    cms_list <- read_csv(CMS_file) %>% select(1:12, 23, 24)
    all_residents_SMI_totals_CMS_list <- left_join(all_residents_final_grouped_summed, cms_list,
                                                   by = c("a0100b_cms_crtfctn_num" = "PROVNUM"))
  }
  if (year %in% c(2020:2022)){
    CMS_file <- paste0('CMS_list/NH_ProviderInfo_',year,'.csv')
    cms_list <- read_csv(CMS_file) %>% select(1:12, 25)
    all_residents_SMI_totals_CMS_list <- left_join(all_residents_final_grouped_summed, cms_list,
                                                   by = c("a0100b_cms_crtfctn_num" = "Federal Provider Number"))
  }
  
  # write facilities-level file 
  write_csv(all_residents_SMI_totals_CMS_list, paste0('Louisiana/final_processed_snapshot_facilities_LA/', year, '_LA_all_facilities_snapshot.csv'))
}


