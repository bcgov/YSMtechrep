library(data.table)
library(dplyr)
library(tidyverse)

# Path to folder where MYST projection data locates
external_path3 <- "//sfp.idir.bcgov/s164/S63016/!Transfr/MSYT_Delivery/delivery2023/pspl_validation/YSM"
# Path to folder where VDYP projection data locates
external_path2 <- "//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/Compilation/ismc/external_inputs"

# Set variable
decidspc <- c('A','AC','ACT','ACB','AT',
              'DM','DR','E','EA','EP','EW',
              'MB','MV','KC','RA','V','VB','VP','VV',
              'W','WB','WP','WT','ZH','XH','XC')

# Import YSM sample data
sample_data <- fread("//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/test/data/sample_data_comp20240213.csv")

# Import TIPSY projection
MSYT_reference <- fread(paste0(external_path3, "/MSYT_reference.csv"))
MSYT_current_input <- fread(paste0(external_path3, "/MSYT_current_input.csv"))
MSYT_current_output <- fread(paste0(external_path3, "/MSYT_current_output.csv"))
names(MSYT_reference) <- toupper(names(MSYT_reference))
names(MSYT_current_input) <- toupper(names(MSYT_current_input))
names(MSYT_current_output) <- toupper(names(MSYT_current_output))

# Import VDYP projection 
VDYP <- fread(paste0(external_path2, 
                     "/spatial_overlay/ISMC_VRI_Overlay/VDYP7/compiled_ISMC_inputs/VDYP7_OUTPUT_YLDTBL.csv"))

# Import YSM tree data
tree_data <- fread("//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/test/data/tree_data_comp20240213.csv") 

# Import TASS output
tass_output <- readRDS("//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/Data/TASS_output_YSM_240723_final_all.rds")


# Select live standing trees
ysm_trees <- tree_data %>%
  filter(S_F == "S", !is.na(PHF_TREE), !is.na(DBH), 
         LV_D == "L") %>%
  # *add gross merch volume to summary outputs;
  mutate(vol_mer_ha = VOL_MER * PHF_TREE,
         vol_wsv_ha = VOL_WSV * PHF_TREE,
         vol_ntwb_ha = VOL_NTWB * PHF_TREE,
         SPC_GRP1 = substr(SPECIES,1,2),
         SPC_GRP1 = ifelse(SPECIES %in% decidspc, 'DE', SPC_GRP1))

# Additional columns for without residuals
ysm_trees_nores <- tree_data %>%
  filter(S_F == "S", !is.na(PHF_TREE), !is.na(DBH), 
         LV_D == "L", RESIDUAL %in% c("N", NA)) %>%
  # *add gross merch volume to summary outputs;
  mutate(vol_mer_ha_nores = VOL_MER * PHF_TREE,
         vol_wsv_ha_nores = VOL_WSV * PHF_TREE,
         vol_ntwb_ha_nores = VOL_NTWB * PHF_TREE)

ysm_trees1 <- ysm_trees %>%
  left_join(ysm_trees_nores %>% select(CLSTR_ID, TREE_NO, SPECIES, 
                                       vol_mer_ha_nores, vol_wsv_ha_nores, vol_ntwb_ha_nores),
            by = c("CLSTR_ID", "TREE_NO", "SPECIES"))

# *sum net merch volumes by species;
ysm_trees1 <- ysm_trees1 %>%
  filter(case_when(SPECIES == "PL" ~ DBH >= 12.5,
                   T ~ DBH >= 17.5)) 

ysm_trees2 <- ysm_trees1 %>%
  group_by(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, SPC_GRP1) %>%
  summarize(vol_wsv_ha = sum(vol_wsv_ha, na.rm = T),
            vol_mer_ha = sum(vol_mer_ha, na.rm = T),
            vol_ntwb_ha = sum(vol_ntwb_ha, na.rm = T),
            vol_wsv_ha_nores = sum(vol_wsv_ha_nores, na.rm = T),
            vol_mer_ha_nores = sum(vol_mer_ha_nores, na.rm = T),
            vol_ntwb_ha_nores = sum(vol_ntwb_ha_nores, na.rm = T))

ysm_trees3 <- ysm_trees2 %>%
  group_by(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID) %>%
  summarize(vol_wsv_ha = sum(vol_wsv_ha, na.rm = T),
            vol_mer_ha = sum(vol_mer_ha, na.rm = T),
            vol_ntwb_ha = sum(vol_ntwb_ha, na.rm = T),
            vol_wsv_ha_nores = sum(vol_wsv_ha_nores, na.rm = T),
            vol_mer_ha_nores = sum(vol_mer_ha_nores, na.rm = T),
            vol_ntwb_ha_nores = sum(vol_ntwb_ha_nores, na.rm = T))

## First, get list of MSYT FEATURE_ID and associate it with SITE_IDENTIFIER
msyt <- MSYT_reference %>%
  left_join(sample_data %>% select(SITE_IDENTIFIER, FEATURE_ID, CLSTR_ID, 
                                   MGMT_UNIT, MEAS_YR, PROJ_AGE_ADJ), 
            by = c("FEATURE_ID"), suffix = c("", "_YSM"))

msyt <- msyt %>%
  filter(!is.na(SITE_IDENTIFIER))

msyt <- msyt %>%
  rowwise() %>%
  mutate(
    results_age = as.numeric(RSLT_AGE),
    results_age_adj = ifelse(!is.na(results_age) & results_age > 0 & 
                               !is.na(RSLT_REFERENCE_YEAR) & RSLT_REFERENCE_YEAR > 0, 
                             results_age + (MEAS_YR - RSLT_REFERENCE_YEAR), NA),
    results_age_adj = ifelse(results_age_adj <= 0, NA, results_age_adj),
    # *create a new reference age, based on results if available, otherwise use proj_age_1;
    # *do not use results age (unreliable) for tfl based programs, just default to vegcomp age;
    ref_age_adj = ifelse(!is.na(results_age_adj) & results_age_adj > 0 & substr(MGMT_UNIT,1,3) != 'TFL', 
                         results_age_adj, PROJ_AGE_ADJ),
    ref_age_adj = ifelse(ref_age_adj < 10, 10, ref_age_adj),
    ref_age_cd = ifelse(!is.na(results_age_adj) & results_age_adj > 0 & substr(MGMT_UNIT,1,3) != 'TFL', 
                        "RESULTS", "VEGCOMP")) %>%
  arrange(FEATURE_ID, SITE_IDENTIFIER, CLSTR_ID)

# MSYT projected volume
msyt_vol <- MSYT_current_output %>%
  filter(FEATURE_ID %in% unique(msyt$FEATURE_ID)) %>%
  select(FEATURE_ID, starts_with("MVCON_"), starts_with("MVDEC_")) %>%
  mutate_at(vars(starts_with("MVCON_")), as.numeric) %>%
  mutate_at(vars(starts_with("MVDEC_")), as.numeric) 

msyt_vol1 <- tidyr::pivot_longer(msyt_vol, cols = matches('^MVCON_|^MVDEC_'), 
                                 names_to = c('.value', 'AGE'), 
                                 names_sep = '_')

msyt_vol2 <- msyt_vol1 %>%
  mutate(AGE = as.numeric(AGE),
         MVALL = MVCON + MVDEC,
         NETVOL = MVALL,
         NETVOL_CON = MVCON) %>%
  filter(AGE <= 100, AGE >= 10)

msyt_vol3 <- msyt_vol2 %>%
  group_by(FEATURE_ID) %>%
  transmute(AGE = list(seq(min(AGE), max(AGE), by = 1))) %>%
  unnest(AGE)

msyt_vol4 <- msyt_vol3 %>%
  left_join(msyt_vol2, by = c("FEATURE_ID", "AGE"))

msyt_vol5 <- msyt_vol4 %>% 
  group_by(FEATURE_ID) %>% 
  mutate(netvol = zoo::na.approx(NETVOL, AGE),
         netvol_con = zoo::na.approx(NETVOL_CON, AGE)) %>% 
  ungroup()

msyt1 <- msyt %>%
  left_join(msyt_vol5, by = c("FEATURE_ID", "ref_age_adj" = "AGE")) %>%
  distinct

# Meanwhile, VDYP projected volume
vdyp1 <- VDYP %>%
  rename(SITE_IDENTIFIER = POLYGON_ID) %>%
  filter(SITE_IDENTIFIER %in% unique(sample_data$SITE_IDENTIFIER))  %>%
  select(FEATURE_ID, LAYER_ID, MAP_ID, 
         SITE_IDENTIFIER, PROJECTION_YEAR, PRJ_TOTAL_AGE, 
         PRJ_DOM_HT, PRJ_BA, PRJ_TPH, PRJ_VOL_WS, PRJ_VOL_CU, PRJ_VOL_DWB)

# Finally, TASS volume
tass_nores <- tass_output %>%
  filter(SPECIES == "ALL", RESID %in% c("N", NA), rust == "N") %>%
  arrange(SITE_IDENTIFIER, VISIT_NUMBER, Age, desc(xy), desc(TASS_ver)) %>%
  group_by(SITE_IDENTIFIER, VISIT_NUMBER, Age) %>%
  slice(1)

tass_nores1 <- tass_nores %>%
  left_join(sample_data %>% select(CLSTR_ID, PROJ_AGE_ADJ), 
            by = c("CLSTR_ID")) %>%
  ungroup() %>%
  mutate(age_join = PROJ_AGE_ADJ - Age)

tass_nores2 <- tass_nores1 %>%
  group_by(CLSTR_ID) %>%
  slice(which.min(abs(age_join)))

# Create YSM data frame with current volume
currentvol <- sample_data %>%
  select(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, FEATURE_ID, MEAS_YR, PROJ_AGE_ADJ, 
         MGMT_UNIT, BEC = BEC_ZONE, BEC_SBZ, TSA_DESC) %>%
  left_join(ysm_trees3, by = c("SITE_IDENTIFIER", "CLSTR_ID", "VISIT_NUMBER")) 

# Merge TASS - YSM
currentvol1 <- currentvol %>%
  left_join(tass_nores2, by = c("SITE_IDENTIFIER", "CLSTR_ID", "VISIT_NUMBER", "PROJ_AGE_ADJ")) 

# Merge MSYT - YSM
# 1. TIPSY: Join with sample data 
currentvol2 <- currentvol1 %>%
  left_join(msyt1 %>% select(-MEAS_YR, -MGMT_UNIT, -PROJ_AGE_ADJ), 
            by = c("SITE_IDENTIFIER", "CLSTR_ID", "FEATURE_ID")) 

## Merge VDYP - ysm
currentvol3 <- currentvol2 %>%
  left_join(vdyp1, by = c("SITE_IDENTIFIER", "MEAS_YR" = "PROJECTION_YEAR"),
            suffix = c("_vegcomp", ""))

currentvol4 <- currentvol3 %>%
  ungroup() %>%
  mutate(#PRJ_VOL_DWB = ifelse(is.na(PRJ_VOL_DWB), 0, PRJ_VOL_DWB),
    vol_wsv_ha = ifelse(is.na(vol_wsv_ha), 0, vol_mer_ha),
    vol_wsv_ha_nores = ifelse(is.na(vol_wsv_ha_nores), 0, vol_mer_ha),
         vol_mer_ha = ifelse(is.na(vol_mer_ha), 0, vol_mer_ha),
         vol_mer_ha_nores = ifelse(is.na(vol_mer_ha_nores), 0, vol_mer_ha),
         vol_ntwb_ha = ifelse(is.na(vol_ntwb_ha), 0, vol_ntwb_ha),
         vol_ntwb_ha_nores = ifelse(is.na(vol_ntwb_ha_nores), 0, vol_ntwb_ha_nores),
         yt_source = ifelse(grepl('Managed:',CURRENT_YIELD), "Managed", 
                            ifelse(grepl('Aggregate',CURRENT_YIELD), "AGGREGATE", CURRENT_YIELD)),
         yt_source_f = factor(yt_source, levels = c("Managed", "AGGREGATE", "VDYP", "Excluded"), ordered = T),
         volTSR = ifelse(yt_source %in% c("Managed","AGGREGATE"), netvol,   
                         ifelse(yt_source == "VDYP", PRJ_VOL_DWB, NA)),
         voldiffTSR = volTSR - vol_mer_ha,
         grdnv = vol_ntwb_ha_nores,
         prednv = volTSR,
         tassnv = GMV,
         #grdnv = ifelse(is.na(vol_ntwb_ha_nores), 0, vol_ntwb_ha_nores),
         #prednv = ifelse(is.na(volTSR), 0, volTSR),
         #tassnv = ifelse(is.na(GMV), 0, GMV),
         voldiffTASS = tassnv - grdnv)  

ysm_msyt_vdyp <- currentvol4 %>%
  select(SITE_IDENTIFIER, CLSTR_ID, VISIT_NUMBER, FEATURE_ID, 
         OPENING_ID, LAYER_ID, TASS_ver, xy,
         MGMT_UNIT, TSA_DESC, BEC, BEC_SBZ, 
         MEAS_YR, PROJ_AGE_ADJ, ref_age_adj, ref_age_cd, PRJ_TOTAL_AGE,
         vol_wsv_ha, vol_mer_ha, vol_ntwb_ha,
         vol_wsv_ha_nores, vol_mer_ha_nores,
         vol_ntwb_ha_nores, PRJ_VOL_DWB, netvol, volTSR,
         yt_source, yt_source_f, 
         grdnv, prednv, tassnv, voldiffTSR, voldiffTASS)


write.csv(ysm_msyt_vdyp, "//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/test/data/ysm_msyt_vdyp_volume.csv")
write.csv(msyt_vol2, "//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/test/data/msyt_volproj.csv")
