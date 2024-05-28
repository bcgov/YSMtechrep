library(data.table)
library(dplyr)

# Path to folder where MYST projection data locates
external_path3 <- "//sfp.idir.bcgov/s164/S63016/!Transfr/MSYT_Delivery/delivery2023/pspl_validation/YSM"
# Path to folder where VDYP projection data locates
external_path2 <- "//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/Compilation/ismc/external_inputs"

# Set variable
decidspc <- c('A','AC','ACT','ACB','AT',
              'DM','DR','E','EA','EP','EW',
              'MB','MV','KC','RA','V','VB','VP','VV',
              'W','WB','WP','WT','ZH','XH','XC')

MSYT_reference <- fread(paste0(external_path3, "/MSYT_reference.csv"))
MSYT_current_output <- fread(paste0(external_path3, "/MSYT_current_output.csv"))
names(MSYT_reference) <- toupper(names(MSYT_reference))
names(MSYT_current_output) <- toupper(names(MSYT_current_output))

VDYP <- fread(paste0(external_path2, 
                     "/spatial_overlay/ISMC_VRI_Overlay/VDYP7/compiled_ISMC_inputs/VDYP7_OUTPUT_YLDTBL.csv"))

# Import YSM tree data
tree_data <- fread("//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/test/data/tree_data_comp20240213.csv") 

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
  summarize(vol_mer_ha = sum(vol_mer_ha, na.rm = T),
            vol_ntwb_ha = sum(vol_ntwb_ha, na.rm = T),
            vol_mer_ha_nores = sum(vol_mer_ha_nores, na.rm = T),
            vol_ntwb_ha_nores = sum(vol_ntwb_ha_nores, na.rm = T))

ysm_trees3 <- ysm_trees2 %>%
  group_by(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID) %>%
  summarize(vol_mer_ha = sum(vol_mer_ha, na.rm = T),
            vol_ntwb_ha = sum(vol_ntwb_ha, na.rm = T),
            vol_mer_ha_nores = sum(vol_mer_ha_nores, na.rm = T),
            vol_ntwb_ha_nores = sum(vol_ntwb_ha_nores, na.rm = T))

# Meanwhile, MSYT projection data
msyt_input1 <- MSYT_current_input %>%
  left_join(MSYT_reference, by = c("FEATURE_ID"), 
            suffix = c("", "_ref"))

msyt_input2 <- msyt_input1 %>%
  left_join(sample_data, 
            by = c("FEATURE_ID"), 
            suffix = c("", "_sample"))

## First, get list of MSYT FEATURE_ID and associate it with SITE_IDENTIFIER
msyt <- MSYT_reference %>%
  left_join(sample_data %>% select(FEATURE_ID, MGMT_UNIT, BEC_ZONE, TSA_DESC, 
                                   SITE_IDENTIFIER, CLSTR_ID, VISIT_NUMBER, MEAS_YR, 
                                   PROJ_AGE_1, PROJECTED_DATE, PROJ_AGE_ADJ), 
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

### MSYT annual predicted volume
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

# Merge MSYT - ysm
# Join with sample data by PROJECTIOn_YEAR = MEAS_YR
ysm_msyt <- msyt1 %>%
  left_join(ysm_trees3, by = c("SITE_IDENTIFIER", "CLSTR_ID", "VISIT_NUMBER")) 

## Meanwhile, VDYP projected volume
vdyp1 <- VDYP %>%
  rename(SITE_IDENTIFIER = POLYGON_ID) %>%
  filter(SITE_IDENTIFIER %in% unique(sample_data$SITE_IDENTIFIER))  %>%
  select(FEATURE_ID, LAYER_ID, MAP_ID, 
         SITE_IDENTIFIER, PROJECTION_YEAR, PRJ_TOTAL_AGE, 
         PRJ_DOM_HT, PRJ_BA, PRJ_TPH, PRJ_VOL_WS, PRJ_VOL_CU, PRJ_VOL_DWB)

## Merge VDYP - ysm
ysm_msyt_vdyp <- ysm_msyt %>%
  left_join(vdyp1, by = c("SITE_IDENTIFIER", "MEAS_YR" = "PROJECTION_YEAR"),
            suffix = c("_vegcomp", ""))

ysm_msyt_vdyp <- ysm_msyt_vdyp %>%
  mutate(PRJ_VOL_DWB = ifelse(is.na(PRJ_VOL_DWB), 0, PRJ_VOL_DWB),
         vol_mer_ha = ifelse(is.na(vol_mer_ha), 0, vol_mer_ha),
         vol_mer_ha_nores = ifelse(is.na(vol_mer_ha_nores), 0, vol_mer_ha),
         vol_ntwb_ha = ifelse(is.na(vol_ntwb_ha), 0, vol_ntwb_ha),
         vol_ntwb_ha_nores = ifelse(is.na(vol_ntwb_ha_nores), 0, vol_ntwb_ha_nores),  ### This is grdnv
         yt_source = ifelse(grepl('Managed:',CURRENT_YIELD), "Managed", 
                            ifelse(grepl('Aggregate',CURRENT_YIELD), "AGGREGATE", CURRENT_YIELD)),
         volTSR = ifelse(yt_source %in% c("Managed","AGGREGATE"), netvol,       ### This is prednv
                         ifelse(yt_source == "VDYP", PRJ_VOL_DWB, 0)),
         voldiffTSR = volTSR - vol_mer_ha,
         yt_source_f = factor(yt_source, levels = c("Managed", "AGGREGATE", "VDYP", "Excluded"), ordered = T))  


write.csv(ysm_msyt_vdyp, "//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/test/data/ysm_msyt_vdyp_volume.csv")

