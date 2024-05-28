library(data.table)
library(dplyr)
library(RODBC)
library(tidyverse)  ## date()

# Path to folder where VRI data locates
external_path <- "//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/Data"
# Path to folder where PSPL_Samples_Grid data locates
external_path2 <- "//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/Compilation/ismc/external_inputs"

vridatpath <- file.path(paste0(external_path,  "/ISMC_VRI_Overlay/2_All_VRI_Attributes_2024Jan31.accdb"))
channel<-odbcConnectAccess2007(vridatpath)
vegcomp1<-sqlFetch(channel,"All_VRI_Attributes")
pspl1 <- fread(paste0(external_path2, "/spatial_overlay/pspl_overlay/datasets/FME4_PSPL_Samples_Grid.csv"))

# Import spcs data
sample_data <- fread("//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/test/data/sample_data_comp20240213.csv") 

# *merge samples with 2021 vegcomp spatial overlay, by visit number;
# *merge with spatial lookup table;
vegcomp_sample <- sample_data %>%
  left_join(vegcomp1, by = c("SITE_IDENTIFIER"),
            suffix = c("_ismc", "_veg"))

# *merge with pspl layer;
vegcomp_pspl_sample <- vegcomp_sample %>%
  left_join(pspl1, by = "SITE_IDENTIFIER")

vegcomp_pspl_sample <- vegcomp_pspl_sample %>%
  mutate(projected_date_conv = date(PROJECTED_DATE_veg),
         harvest_date_conv = date(HARVEST_DATE),
         nonharvest_date_conv = date(EARLIEST_NONLOGGING_DIST_DATE),
         mm = as.numeric(substr(MEAS_DT, 6, 7))) %>%
  mutate(meas_yr = ifelse(mm >= 7, MEAS_YR, MEAS_YR-1),
         # *projection year in vegcomp is to the end of the growing season (not jan01 as shown in projection_date;
         # *ie., confirmed that projection_date should be more correctly defined as 31dec;
         projected_year = year(projected_date_conv),
         # *below is what was asusmed previously, now known as incorrect;
         #projected_year = ifelse(month(projected_date_conv) >= 7, year(projected_date_conv), year(projected_date_conv) - 1),
         # *define appropriate inventory age to match projected age to ground measurement year;
         proj_age_adj = PROJ_AGE_1_veg + (meas_yr - projected_year),
         proj_age_adj = ifelse(proj_age_adj <= 0, NA, proj_age_adj),
         # *prep steps to define management unit;
         tsa_desc2 = gsub(" TSA", "", TSA_DESC),
         tsa_desc3 = ifelse(tsa_desc2 %in% c('Queen Charlotte','Haida Gwaii'), "", tsa_desc2),
         tfl_no = as.numeric(gsub('TFL', "", TFL)),
         tfl_no = ifelse(tfl_no > 0 & tfl_no < 10, paste('0',tfl_no), as.character(tfl_no))
  )

write.csv(vegcomp_pspl_sample, "//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/test/data/vegcomp_pspl_sample.csv")

