library(data.table)
library(dplyr)

Import_gsdata <- function(datafolder,
                          compilationdate,
                          outputfolder){
  folderloc <- datafolder
  
  if (!is.na(compilationdate)){
    # If compilation data is provided
    compdate <- compilationdate
  } else {
    # If not, use the latest compilation
    compdate <- max(as.numeric(gsub("nonPSP_","",folderloc[grep("nonPSP_",folderloc)])))
  }
  
  indatapath <- file.path(folderloc, paste0("nonPSP_",compdate))
  comp_path <- file.path(paste0("//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/Compilation/ismc/Archive_nonPSP_", 
                                compilationdate))
  
  faib_header <-fread(paste0(indatapath, "/faib_header.csv"))
  faib_sample_byvisit <-fread(paste0(indatapath, "/faib_sample_byvisit.csv"))
  faib_spcsmries <-fread(paste0(indatapath, "/faib_compiled_spcsmries.csv"))
  faib_smeries <-fread(paste0(indatapath, "/faib_compiled_smeries.csv"))
  faib_siteage <-fread(paste0(indatapath, "/faib_compiled_spcsmries_siteage.csv"))
  faib_tree <-fread(paste0(indatapath, "/faib_tree_detail.csv"))
  
  vi_d<-readRDS(paste0(comp_path, "/compilation_nonPSP_db/compiled_vi_d.rds"))
  
  
  # Create list of YSM sample data
  sample_data <- faib_sample_byvisit %>% 
    left_join(faib_header, by = c("SITE_IDENTIFIER", "SAMPLE_ESTABLISHMENT_TYPE")) %>%
    mutate(BEClabel = paste0(BEC_ZONE, BEC_SBZ, 
                             ifelse(!is.na(BEC_VAR), BEC_VAR, '')),
           #proj_id = strsplit(SAMPLE_SITE_NAME, "_", fixed = TRUE)[[1]][1],
           proj_id = sub("_.*", "", SAMPLE_SITE_NAME)) 
  
  # Filter YSM sample
  sample_data <- sample_data %>%
    # *remove nvaf sample, which is a repeated sample of the same plot in same year;
    filter(SAMPLE_SITE_PURPOSE_TYPE_CODE != "N", YSM_MAIN_FM == "Y") %>%
    # *remove B-sample types located on same site_identifier as L-sample type;
    filter(!(SAMPLE_SITE_PURPOSE_TYPE_CODE == 'B' & proj_id == 'KOL1')) %>%
    # *test example ismc plots entered in 2019;
    #filter(as.numeric(gsub("([0-9]+).*$", "\\1", proj_id)) != 2019) %>%
    # *not interested in sample visits that originated as NFI samples in 2000 - 2003, 
    # as this was not part of the original population criteria;
    # *and measurement periods for both ysm and mature assessments;
    filter(!(SAMPLE_SITE_PURPOSE_TYPE_CODE %in% c('F') &
                MEAS_YR %in% c(2000, 2001, 2002, 2003) &
               proj_id %in% c('CMI1', 'CMI2', 'CMI3', 'CMI4', 'CMI5', 'CMI6'))) %>%
    # *not interested in remeasured nfi samples but still prior to start of ysm program;
    filter(!(SITE_IDENTIFIER == 1451976 & VISIT_NUMBER == 2)) %>%
    filter(!(SITE_IDENTIFIER == 1500241 & VISIT_NUMBER == 1)) %>%
    filter(!(SITE_IDENTIFIER == 1417566 & VISIT_NUMBER == 2)) %>%
    filter(!(SITE_IDENTIFIER == 1348756 & VISIT_NUMBER == 2)) %>%
    filter(!(SITE_IDENTIFIER == 1355611 & VISIT_NUMBER == 2)) %>%
    # *duplicate measurement to be deleted in ismc;
    filter(!(SITE_IDENTIFIER == 1424441 & VISIT_NUMBER == 1)) 
    
  
  # All CLSTR_ID in sample_data
  clstr_ids <- unique(sample_data$CLSTR_ID)
  
  # Subset data
  siteage_data <- faib_siteage %>%
    filter(CLSTR_ID %in% clstr_ids)
  
  spcs_data <- faib_spcsmries %>%
    filter(CLSTR_ID %in% clstr_ids)
  
  summary_data <- faib_smeries %>%
    filter(CLSTR_ID %in% clstr_ids)
  
  # Join damage information to tree data
  tree_data <- faib_tree %>%
    filter(CLSTR_ID %in% clstr_ids) %>%
    left_join(vi_d[, .(CLSTR_ID, PLOT, TREE_NO, SPECIES,
                       LOSS1_IN,LOSS2_IN,LOSS3_IN,LOSS4_IN,LOSS5_IN,
                       LOC1_FRO,LOC2_FRO,LOC3_FRO,LOC4_FRO,LOC5_FRO,
                       FREQ1,FREQ2,FREQ3,FREQ4,FREQ5)], 
              by = c('CLSTR_ID', 'PLOT', 'TREE_NO', 'SPECIES')) 
  
  rm(faib_header)
  rm(faib_sample_byvisit)
  rm(faib_spcsmries)
  rm(faib_smeries)
  rm(faib_siteage)
  rm(faib_tree)
  rm(vi_d)
  
  if (dir.exists(outputfolder)){
    # If compilation data is provided
    outputpath <- outputfolder
  } else {
    # If not, use the latest compilation
    dir.create(outputfolder)
  }
  
  write.csv(sample_data,
            file.path(outputpath,
                      paste0("sample_data_comp", compdate, ".csv")),
            na = "",
            row.names = FALSE)
  
  write.csv(siteage_data,
            file.path(outputpath,
                      paste0("siteage_data_comp", compdate, ".csv")),
            na = "",
            row.names = FALSE)
  
  write.csv(spcs_data,
            file.path(outputpath,
                      paste0("spcs_data_comp", compdate, ".csv")),
            na = "",
            row.names = FALSE)
  
  write.csv(tree_data,
            file.path(outputpath,
                      paste0("tree_data_comp", compdate, ".csv")),
            na = "",
            row.names = FALSE)
  
  write.csv(summary_data,
            file.path(outputpath,
                      paste0("summary_data_comp", compdate, ".csv")),
            na = "",
            row.names = FALSE)
  
  return(sample_data)
}

