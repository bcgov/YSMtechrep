library(data.table)

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
                             ifelse(!is.na(BEC_VAR), BEC_VAR, ''))) 
  
  # Filter YSM sample
  sample_data <- sample_data %>%
    filter(SAMPLE_SITE_PURPOSE_TYPE_CODE != "N", YSM_MAIN_FM == "Y")
  
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

