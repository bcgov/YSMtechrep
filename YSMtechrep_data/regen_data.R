library(data.table)
library(dplyr)

# Path to folder where MYST data locates
external_path3 <- "//sfp.idir.bcgov/s164/S63016/!Transfr/MSYT_Delivery/delivery2023/pspl_validation/YSM"

# Set variable
decidspc <- c('A','AC','ACT','ACB','AT',
              'DM','DR','E','EA','EP','EW',
              'MB','MV','KC','RA','V','VB','VP','VV',
              'W','WB','WP','WT','ZH','XH','XC')

MSYT_current_input <- fread(paste0(external_path3, "/MSYT_current_input.csv"))
MSYT_reference <- fread(paste0(external_path3, "/MSYT_reference.csv"))

names(MSYT_current_input) <- toupper(names(MSYT_current_input))
names(MSYT_reference) <- toupper(names(MSYT_reference))

msyt_input1 <- MSYT_current_input %>%
  left_join(MSYT_reference, by = c("FEATURE_ID"), 
            suffix = c("", "_ref"))

msyt_input2 <- msyt_input1 %>%
  left_join(sample_data, 
            by = c("FEATURE_ID"), 
            suffix = c("", "_sample"))

regen <- msyt_input2 %>%
  dplyr::select(MGMT_UNIT, BEC_ZONE = BEC_ZONE_sample, TSA_DESC,
                FEATURE_ID, SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID,
                PLANTED_SPECIES1:PLANTED_SPECIES5, 
                PLANTED_DENSITY1:PLANTED_DENSITY5, 
                GENETIC_WORTH1:GENETIC_WORTH5, 
                NATURAL_SPECIES1:NATURAL_SPECIES5,
                NATURAL_DENSITY1:NATURAL_DENSITY5) 

regen1 <- tidyr::pivot_longer(regen, cols = matches("\\d{1}$"), 
                              names_pattern = "(\\w+)(\\d{1})", 
                              names_to = c(".value", "num")) %>% 
  filter(NATURAL_DENSITY > 0 | PLANTED_DENSITY > 0) %>% 
  mutate(regen_src = ifelse(PLANTED_DENSITY > 0, "P", ifelse(NATURAL_DENSITY > 0 , "N", NA)))

regen2 <- regen1 %>%  
  select(MGMT_UNIT, BEC_ZONE, TSA_DESC, 
         FEATURE_ID, SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, num,
         GENETIC_WORTH, ends_with("_SPECIES")) %>%
  tidyr::pivot_longer(cols = ends_with("_SPECIES"), 
                      names_to = 'regen_src', 
                      values_to = 'SPECIES')

regen2 <- regen2 %>%
  mutate(regen_src = substr(regen_src, 1, 1))

regen3 <- regen1 %>%
  select(MGMT_UNIT, BEC_ZONE, TSA_DESC, 
         FEATURE_ID, SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, num,
         GENETIC_WORTH, ends_with("_DENSITY")) %>%
  tidyr::pivot_longer(cols = ends_with("_DENSITY"), 
                      names_to = 'regen_src', 
                      values_to = 'DENSITY')

regen3 <- regen3 %>%
  mutate(regen_src = substr(regen_src, 1, 1))

regen_data <- merge(regen2, regen3, 
                    by =c('MGMT_UNIT', 'FEATURE_ID', 'SITE_IDENTIFIER', 'CLSTR_ID',
                          'VISIT_NUMBER','num', 'GENETIC_WORTH', 'regen_src')) %>%
  distinct()

regen_data <- regen_data %>%
  mutate(SPECIES_recode = toupper(substr(SPECIES, 1, 2)),
         SPC_GRP1 =  ifelse(SPECIES_recode %in% decidspc, 'DE', SPECIES_recode))


write.csv(regen_data, "//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/test/data/regen_data.csv")

