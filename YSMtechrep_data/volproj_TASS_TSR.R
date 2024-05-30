library(data.table)
library(dplyr)

external_path2 <- "//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/Compilation/ismc/external_inputs"
external_path3 <- "//sfp.idir.bcgov/s164/S63016/!Transfr/MSYT_Delivery/delivery2023/pspl_validation/YSM"

# Import TASS output
tass_output <- readRDS("//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/Data/TASS_output_YSM_240723_final_all.rds")

# Import YSM sample data
sample_data <- fread("//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/test/data/sample_data_comp20240213.csv")

# Import MSYT projection
msyt_volproj <- fread("//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/test/data/msyt_volproj.csv")

# Import MSYT reference to retrieve age information
MSYT_reference <- fread(paste0(external_path3, "/MSYT_reference.csv"))
names(MSYT_reference) <- toupper(names(MSYT_reference))

# Import VDYP projection
VDYP_all <- fread(paste0(external_path2, 
                         "/spatial_overlay/ISMC_VRI_Overlay/VDYP7/VDYP7_OUTPUT_YLDTBL.csv"))

VDYP_input <- fread(paste0(external_path2, 
                           "/spatial_overlay/ISMC_VRI_Overlay/VDYP7/VDYP7_INPUT_LAYER.csv"))

# Remove residual
VDYP_input <- VDYP_input %>%
  filter(VDYP7_LAYER_CD == "P")

VDYP_all <- VDYP_all %>%
  inner_join(VDYP_input %>% select(FEATURE_ID, LAYER_ID = LAYER_LEVEL_CODE), 
             by = c("FEATURE_ID", "LAYER_ID"))

# Set OAF values
OAF1 = 0.15
OAF2 = 0.05

# Create a data frame with combinations of projection age
tsr_tass_proj <- merge(data.frame(AGE = c(30, 40, 50, 60, 70, 80, 90, 100)), 
           sample_data %>%
             select(CLSTR_ID, FEATURE_ID, MEAS_YR, PROJ_AGE_ADJ, 
                    MGMT_UNIT, BEC = BEC_ZONE, BEC_SBZ, TSA_DESC))

# TASS projection up to 200
tass_output1 <- tass_output %>%
  filter(SPECIES == "ALL", RESID %in% c("N", NA), CLSTR_ID %in% unique(sample_data$CLSTR_ID))

tass_output2 <- tass_output1 %>%
  arrange(SITE_IDENTIFIER, VISIT_NUMBER, rust, Age, desc(TASS_ver), desc(xy)) %>%
  group_by(SITE_IDENTIFIER, VISIT_NUMBER, rust, Age) %>%
  slice(1) %>%
  ungroup()

tsr_tass_proj1 <- tsr_tass_proj %>%
  left_join(tass_output2 %>% select(-SITE_IDENTIFIER, -VISIT_NUMBER), 
            by = c("CLSTR_ID", "AGE" = "Year"))

# Merge TIPSY-VDYP TSR projection
tsr_tass_proj2 <- tsr_tass_proj1 %>%
  left_join(msyt_volproj, 
            by = c("FEATURE_ID", "AGE"))

tsr_tass_proj3 <- tsr_tass_proj2 %>%
  left_join(VDYP_all %>% select(FEATURE_ID, POLYGON_ID, LAYER_ID, 
                                PROJECTION_YEAR, PRJ_TOTAL_AGE, 
                                PRJ_BA, PRJ_TPH, PRJ_VOL_WS, PRJ_VOL_DWB), 
            by = c("FEATURE_ID", "AGE" = "PRJ_TOTAL_AGE"))

# Get RESULTS AGE
tsr_tass_proj4 <- tsr_tass_proj3 %>%
  left_join(MSYT_reference, by = c("FEATURE_ID"))

tsr_tass_proj4 <- tsr_tass_proj4 %>%
  rowwise() %>%
  mutate(results_age = as.numeric(RSLT_AGE),
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
                             "RESULTS", "VEGCOMP"),
         yt_source = ifelse(grepl('Managed:', CURRENT_YIELD), "Managed", 
                            ifelse(grepl('Aggregate', CURRENT_YIELD), "AGGREGATE", "VDYP")),
         yt_source_f = factor(yt_source, levels = c("Managed", "AGGREGATE", "VDYP", "Excluded"), ordered = T),
         volTSR = ifelse(yt_source %in% c("Managed","AGGREGATE"), NETVOL,       ### This is prednv
                         ifelse(yt_source == "VDYP", PRJ_VOL_DWB, 0)),
         volTSR = ifelse(is.na(volTSR), 0, volTSR))

# Adjust TASS GMV based on OAF values
tsr_tass_proj4 <- tsr_tass_proj4 %>%
  ungroup() %>%
  mutate(GMV_adj = ifelse((AGE-ref_age_adj) < (80-ref_age_adj),
                           GMV*(1-OAF1*((AGE - ref_age_adj)/(80 - ref_age_adj)))*(1-OAF2*(AGE/100)),
                           GMV*(1-OAF1)*(1-OAF2*(AGE/100))))

tsr_tass_proj5 <- tsr_tass_proj4 %>%
  left_join(sample_data %>% select(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID),
            by = c("CLSTR_ID"))

# Should the rows without SITE_IDENTIFIER (becasue AGE is smaller than ref_ag_adj) be removed?
tsr_tass_volproj <- tsr_tass_proj5 %>%
  #filter(!is.na(SITE_IDENTIFIER)) %>%
  select(FEATURE_ID, SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, MEAS_YR, PROJ_AGE_ADJ, 
         BEC, BEC_SBZ, TSA_DESC, MGMT_UNIT, 
         POLYGON_ID, LAYER_ID, PROJECTION_YEAR, PRJ_BA, PRJ_TPH, PRJ_VOL_WS, PRJ_VOL_DWB, 
         OPENING_ID, RSLT_AGE, RSLT_REFERENCE_YEAR, VRI_AGE, CATEGORY, RSLT_AGE, RSLT_REFERENCE_YEAR, NETVOL,
         results_age, results_age_adj, ref_age_adj, ref_age_cd, yt_source, yt_source_f, volTSR,
         AGE, RESID, Age, BA, Stems, WSV, GMV, GMV_adj, TASS_ver, xy, rust)


write.csv(tsr_tass_volproj, "//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/test/data/tsr_tass_volproj.csv")
