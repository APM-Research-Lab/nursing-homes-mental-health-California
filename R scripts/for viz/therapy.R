library(data.table)
library(tidyverse)

# read in 2022 data by individual 
df <- read_csv('final_processed_snapshot/2022_CA_all_residents_snapshot.csv')

# (this is the same as in analysis -> summary_stats_individual but with totals 
# used instead of percentage)

# how many long-term residents with SMI had any therapy in week before assessment?
# Uses just the people with an annual assessment (meaning they have been at same 
# facility for at least a year). This is because the admission assessment might be done 
# before the patient has been there for a full week, or otherwise hasn't had the opportunity to 
# get therapy yet 
therapy <- df %>% 
  filter(a0310a_fed_obra_cd == 3) %>% 
  filter(SMI == 1) %>%
  mutate(therapy = ifelse(o0400e1_psych_thrpy_min_num > 0, 1, 0))

sum(therapy$therapy, na.rm = TRUE)
length(therapy$therapy)
## RESULT FOR GRAPHIC USED IN ILLUSTRATOR --> 85 out of 12,012 residents had any amount 
# of therapy 
