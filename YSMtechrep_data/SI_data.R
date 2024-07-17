library(data.table)
library(dplyr)


sample_data <- fread("//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/test/data3/sample_data_comp20240611.csv")

site <- sample_data %>%
  select(SITE_IDENTIFIER, TSA, TSA_DESC, BEC_ZONE, BEC_SBZ) %>%
  unique()

siteage_data <- fread("//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/test/data3/siteage_data_comp20240611.csv")

siteage_data <- siteage_data %>%
  left_join(sample_data[,.(SITE_IDENTIFIER, CLSTR_ID, VISIT_NUMBER, 
                           TSA, TSA_DESC, BEC_ZONE, BEC_SBZ)], by = ("CLSTR_ID"))


pspl <- fread("//sfp.idir.bcgov/S164/S63016/!Workgrp/Inventory/Compilation/ismc/external_inputs/spatial_overlay/pspl_overlay/datasets/FME4_PSPL_Samples_Grid.csv")

# Filter YSM samples
pspl <- pspl %>%
  filter(SITE_IDENTIFIER %in% unique(sample_data$SITE_IDENTIFIER))

# Join BEC zone information
pspl <- pspl %>%
  left_join(site[,.(SITE_IDENTIFIER, TSA, TSA_DESC, BEC_ZONE, BEC_SBZ)], 
            by = ("SITE_IDENTIFIER"))

# Reshape PSPL data
PSPL_data <- melt(pspl,
                  id.vars = c('SITE_IDENTIFIER', 'BEC_ZONE', 'BEC_SBZ'),
                  measure.vars = patterns("_SI"),
                  variable.name = "SPECIES",
                  value.name = "pspl_si")

PSPL_data <- PSPL_data %>%
  filter(!is.na(pspl_si)) %>%
  mutate(SPECIES = substr(SPECIES, 1, 2))

# Correction on SX 
for (i in 1:nrow(PSPL_data)){
  
  if (PSPL_data$SPECIES[i] == "SX"){
    
    sid<-PSPL_data$SITE_IDENTIFIER[i]
    
    if (sum(match(PSPL_data[PSPL_data$SITE_IDENTIFIER == sid,]$SPECIES, c("SE", "SS", "SW")), na.rm=T)==0){
      
      if (PSPL_data$BEC_ZONE[i] == "ESSF"){
        PSPL_data$SPECIES[i] <- "SE"
      }
      else if (PSPL_data$BEC_ZONE[i] %in% c("CMA", "CDF", "CWH", "MH")){
        PSPL_data$SPECIES[i] <- "SS"
      }
      else if (PSPL_data$BEC_ZONE[i] %in% c("IMA", "BFA", "BG", "BWBS", 
                                            "ICH", "IDF", "MS", "PP", "SBPS",
                                            "SBS", "SWB")){
        PSPL_data$SPECIES[i] <- "SW"
      }
    }
  }
}


temp <- siteage_data %>%
  left_join(PSPL_data[,.(SITE_IDENTIFIER, SPECIES, pspl_si)], by = c("SITE_IDENTIFIER", "SPECIES"))

# Match species
for (i in 1:nrow(temp)){
  
  if (is.na(temp$pspl_si[i])){
    
    # *for non matching interior spruce, assign one of three, mutually exclusive in the pspl;
    if (temp$SPECIES[i] %in% c('SW','SE')){
      
      sid <- temp$SITE_IDENTIFIER[i] 
      
      if(nrow(PSPL_data[PSPL_data$SITE_IDENTIFIER == sid & PSPL_data$SPECIES %in% c('SW','SE'),])>0){
        
        temp$pspl_si[i] <- PSPL_data[PSPL_data$SITE_IDENTIFIER == sid & PSPL_data$SPECIES %in% c('SW','SE'),]$pspl_si
        
      }
      
    }
    
    # *for non matching balsam, assign one of two, mutually exclusive in the pspl;
    if (temp$SPECIES[i] %in% c('BL','BA')){
      
      sid <- temp$SITE_IDENTIFIER[i] 
      
      if(nrow(PSPL_data[PSPL_data$SITE_IDENTIFIER == sid & PSPL_data$SPECIES %in% c('BL','BA'),])>0){
        
        temp$pspl_si[i] <- PSPL_data[PSPL_data$SITE_IDENTIFIER == sid & PSPL_data$SPECIES %in% c('BL','BA'),]$pspl_si
        
      }
      
    }
    
    # *for non matching hemlock, assign one of two, mutually exclusive in the pspl;
    if (temp$SPECIES[i] %in% c('HW','HM')){
      
      sid <- temp$SITE_IDENTIFIER[i] 
      
      if(nrow(PSPL_data[PSPL_data$SITE_IDENTIFIER == sid & PSPL_data$SPECIES %in% c('HW','HM'),])>0){
        
        temp$pspl_si[i] <- PSPL_data[PSPL_data$SITE_IDENTIFIER == sid & PSPL_data$SPECIES %in% c('HW','HM'),]$pspl_si
        
      }
      
    }
  }
}

SI_data <- temp %>%
  mutate(ratio = SI_M_TLSO / pspl_si,
         yrs_from_50 = abs(50-AGEB_TLSO))

saveRDS(SI_data, "//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/test/data3/SI_data.rds")


SI_data <- temp %>%
  filter(!is.na(ratio)) %>%
  group_by(SITE_IDENTIFIER, SPECIES) %>%
  slice(which.min(yrs_from_50))

