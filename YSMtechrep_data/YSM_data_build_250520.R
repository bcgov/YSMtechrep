################################################################################
### Load library & set static variables
################################################################################
library(data.table)
library(dplyr)
library(DT)
library(tibble)
library(tidyverse)
library(scales)
library(RODBC)
library(openxlsx)
library(readr)


### Deciduous species list
decidspc <- c('A','AC','ACT','ACB','AT',
              'DM','DR','E','EA','EP','EW',
              'MB','MV','KC','RA','V','VB','VP','VV',
              'W','WB','WP','WT','ZH','XH','XC')

### Set OAF values
OAF1 = 0.15
OAF2 = 0.05

################################################################################
### Import data
###  1) ISMC compiled ground sample data
###  2) MSYT projection table
###  3) VDYP projection table
###  4) TASS output
###  5) Other data (VRI, severity rating lookup table)
################################################################################

### Set data export location
savepath <- "/YSMtechrep/shiny_app/data"

### 1) Import ISMC compiled ground sample data
### Set data import location
folderloc <- "/Inventory/Compilation/ismc/forpublish"  # Published BC ground samples
### Pick a compilation date (ex. 20240619)
compdate <- ''
### Need some unpublished data
comp_path <- file.path(paste0("/Inventory/Compilation/ismc/Archive_nonPSP_", 
                              compdate))
indatapath <- file.path(folderloc, paste0("nonPSP_",compdate))

### Read ISMC compiled data (published)
faib_header <-fread(paste0(indatapath, "/faib_header.csv"))
faib_sample_byvisit <-fread(paste0(indatapath, "/faib_sample_byvisit.csv"))
faib_spcsmries <-fread(paste0(indatapath, "/faib_compiled_spcsmries.csv"))
faib_smeries <-fread(paste0(indatapath, "/faib_compiled_smeries.csv"))
faib_siteage <-fread(paste0(indatapath, "/faib_compiled_spcsmries_siteage.csv"))
faib_tree <-fread(paste0(indatapath, "/faib_tree_detail.csv"))
### Read ISMC compiled data (unpublished)
vi_d<-readRDS(paste0(comp_path, "/compilation_nonPSP_db/compiled_vi_d.rds"))
### Import IMSC damage agent for severity class lookup table
lookup_sev <-readRDS(paste0(comp_path, "/compilation_nonPSP_raw/ISMC_PROD_********_***_TreeDamageOccurrences.rds"))


### 2) Import MSYT projection table
msytfpath <- "/MSYT_Delivery/delivery2023"
### Input, output, and reference table
MSYT_current_input <- fread(paste0(msytfpath,"/msyt/MSYT_current_input.csv"))
MSYT_current_output <- fread(paste0(msytfpath,"/msyt/MSYT_current_output.csv"))
MSYT_reference <- fread(paste0(msytfpath,"/msyt/MSYT_reference.csv"))
### Make sure the variable names match
names(MSYT_reference) <- toupper(names(MSYT_reference))
names(MSYT_current_input) <- toupper(names(MSYT_current_input))
names(MSYT_current_output) <- toupper(names(MSYT_current_output))


### 3) Import VDYP projection table
vdyppath <- "/Inventory/Compilation/ismc/external_inputs"
### Input and output
VDYP_all <- fread(paste0(vdyppath, "/spatial_overlay/ISMC_VRI_Overlay/VDYP7/VDYP7_OUTPUT_YLDTBL_old.csv"))
VDYP_input <- fread(paste0(vdyppath,  "/spatial_overlay/ISMC_VRI_Overlay/VDYP7/VDYP7_INPUT_LAYER.csv"))


### 4) Import TASS output
tass_output <- readRDS("/Output/tass_output_combined.rds")


### 5) Other data
### Path to folder where external data locates
external_path <- "/Inventory/Compilation/ismc/external_inputs"

### Import VRI data
# Read VRI data - this version is based on the published 2022 vri feature_id;
vridatpath <- file.path(paste0(external_path, 
                               "/spatial_overlay/ISMC_VRI_Overlay/2_All_VRI_Attributes_2024Jun11.accdb"))
channel<-odbcConnectAccess2007(vridatpath)
vegcomp1<-sqlFetch(channel,"All_VRI_Attributes")

# *import crosswalk table for feature_id based on 2022 vegcomp, 
# needed to provide linkage for latest tsr msyt tables;
vegcomp2<- read.xlsx(paste0(external_path, 
                            "/spatial_overlay/ISMC_VRI_Overlay/2022VegCompR1_Overlay/1_Plot_Overlay_Out_2024Jun18.xlsx"))
### Make sure the variable names match
names(vegcomp2) <-c('SITE_IDENTIFIER', 'Alb_x', 'Alb_y', 'FEATURE_ID_2022', 'PROJ_AGE_1_2022', 'PROJECTED_DATE_2022')

### Import PSPL data
pspl <- fread(paste0(external_path, "/spatial_overlay/pspl_overlay/datasets/FME4_PSPL_Samples_Grid.csv"))

### Import forest health severity data
#* severity rating lookup table created by D.Rusch;
lookup_rush <- read.xlsx(paste0(external_path, 
                                "/severity_rating_lookup_table/Severity_lookup_table_2021mar15.xlsx"),
                         sheet = 'input1')

#* corrections to severity rating - unknown to correct severity ratings created by D.Rusch 2021mar10;
sev_rusch <- read.xlsx(paste0(external_path, 
                              "/forest_health/severity_rating_lookup_table/Unknown_severity_2021mar09.xlsx"))



################################################################################
### List of ground samples in YSM population
###  - Input: faib_sample_byvisit, faib_header, vegcomp2
###  - Output: sample_data
################################################################################

### Create list of YSM sample data
sample_data1 <- faib_sample_byvisit %>% 
  left_join(faib_header, by = c("SITE_IDENTIFIER", "SAMPLE_ESTABLISHMENT_TYPE")) %>%
  mutate(BEClabel = paste0(BEC_ZONE, BEC_SBZ, 
                           ifelse(!is.na(BEC_VAR), BEC_VAR, '')),
         BECsub = paste0(BEC_ZONE, BEC_SBZ),
         proj_id = sub("_.*", "", SAMPLE_SITE_NAME),
         OWN_SCHED = paste0(OWNER,"-",SCHEDULE)) %>%
  ### Join with VRI data to associate feature id for MSYT and VDYP projections
  left_join(vegcomp2 %>% select(SITE_IDENTIFIER, FEATURE_ID_2022, PROJ_AGE_1_2022),
            by = c("SITE_IDENTIFIER"), suffix = c("", "_vegcomp")) %>%
  mutate(sample_change_case = case_when(
    SAMPLE_ESTABLISHMENT_TYPE == "VRI" & VISIT_TYPE == "REP" ~ 'rep_in_vri',
    SAMPLE_ESTABLISHMENT_TYPE == "CMI" & VISIT_TYPE == "TMP" & SAMPLE_SITE_PURPOSE_TYPE_CODE == "A" ~ 'eysm_in_cmi',
    SAMPLE_ESTABLISHMENT_TYPE == "YSM" & VISIT_TYPE == "TMP" & SAMPLE_SITE_PURPOSE_TYPE_CODE == "A" ~ 'eysm_in_ysm',
    SAMPLE_ESTABLISHMENT_TYPE == "SUP" & VISIT_TYPE == "REP" & SAMPLE_SITE_PURPOSE_TYPE_CODE == "A" ~ 'ysm_in_sup',
    SAMPLE_ESTABLISHMENT_TYPE == "SUP" & VISIT_TYPE == "TMP" & SAMPLE_SITE_PURPOSE_TYPE_CODE == "Y" ~ 'eysm_in_sup',
    SAMPLE_ESTABLISHMENT_TYPE == "CNS" & VISIT_TYPE == "TMP" & SAMPLE_SITE_PURPOSE_TYPE_CODE %in% c("D", "O") ~ 'tmp_in_cns',
    SAMPLE_ESTABLISHMENT_TYPE == "YNS" & VISIT_TYPE == "TMP" & SAMPLE_SITE_PURPOSE_TYPE_CODE == "L" ~ 'tmp_in_yns',
    MGMT_UNIT == "TFL60_TaanForest" & SAMPLE_ESTABLISHMENT_TYPE == "CNS" & VISIT_TYPE == "REP" ~ 'pre_post_trt',
    # *in merritt tsa, YSM and CMI/NFI samples are on different grids, 
    # so only one sample type can be retained.  for ysm analysis drop cmi,nfi;
    # *for mature assessment analysis, drop ysm;
    MGMT_UNIT == "TSA18_Merritt" & SAMPLE_ESTABLISHMENT_TYPE == "CMI" ~ 'popn_conflict',
    YSM_MAIN_LM == "Y" & PROJ_AGE_ADJ < 15 ~ 'ysm_too_young',
    TRUE ~"")) 


### Define YSM population 
### This is an example for 2024 population. Need to modify as needed.
sample_data2 <- sample_data1 %>%
  # *delete visits outside of any mapped population (ie., proj_age_adj = missing;
  # *subset ysm cmi and nfi monitoring samples that fall in ysm target pop;
  filter(!is.na(PROJ_AGE_ADJ), SAMPLE_ESTABLISHMENT_TYPE %in% c('YSM','CMI','NFI')) %>%
  # *subset population of interest;
  filter(YSM_MAIN_FM == "Y" | YSM_MAIN_LM == "Y") %>%
  filter(sample_change_case == "") %>%
  # *drop parks / conservancy areas / private / IR ;
  filter(!(OWN_SCHED %in% c('63-N','50-N','51-N','53-N','54-N',
                            '67-N','64-N','60-N','40-N','41-N',
                            '52-N','72-A','77-A','79-A','80-N',
                            '99-N','81-U'))) %>%
  # *dont use age as a limitation for pilot ysm, since criteria were different for licensees;
  #filter(PROJ_AGE_ADJ >= 15) #%>%
  # *drop ysm samples shown as in popn, but determined outside, check email from cmulvihill 2024-02-05;
  ### '\Compilation_Sample_Check_CM Comments.xlsx
  filter(!(SITE_IDENTIFIER %in% c(1101001, 1314346, 1314351, 1314356,
                                  1334996, 2025067, 2066139, 2095091,
                                  2097096, 2101092, 2119129, 2120268, 2151194))) %>%
  # *the following are dropped only for the bec summaries to standardize grid sizes;
  # *haida gwaii tsa 25 and bec_subzone CWHvh were established as 5by5, 
  # need to standardize to 5by10 for bec summaries;
  mutate(BEC_filter = case_when(TSA == 25 & SITE_IDENTIFIER %in% 
                                  c(2106268, 2120273, 2122273, 2128276, 2130277) ~ "N",
                                TRUE ~ "Y")) %>% 
  # *the following are dropped only for the bec summaries to standardize grid sizes;
  # *merritt tsa 18 was established and remeasured on a provincial 4*4km grid, 
  # need to standardize to 4by8 for bec summaries;
  # *to make generally consistent with 5*10km intensification of the nfi grid;
  mutate(BEC_filter = case_when(TSA == 18 & SITE_IDENTIFIER %in% 
                                  c(3000692, 3000655, 3000649, 3000604, 3000509,
                                    3000485, 3000432, 3000430, 3000414, 3000376,
                                    3000354, 3000353, 3000344, 3000325, 3000324,
                                    3000323, 3000321, 3000317, 3000282, 3000281,
                                    3000276, 3000275, 3000274, 3000268, 3000229,
                                    3000228, 3000203, 3000201, 3000200, 3000192,
                                    3000164, 3000147, 3000115, 3000085, 3000083,
                                    3000082, 3000080, 3000078, 3000077, 3000048,
                                    3000047, 3000046, 3000044, 3000043, 3000040,
                                    3000039, 3000021, 3000003) ~ "N",
                                TRUE ~ BEC_filter)) %>%
  # *drop ysm plots that have excluded tsr yield tables;
  filter(!(FEATURE_ID_2022 %in% MSYT_reference[MSYT_reference$CURRENT_YIELD == "Excluded",]$FEATURE_ID)) %>%
  mutate(TSA_filter = case_when(TFL != "" ~ "N",
                                TRUE ~ "Y")) %>% 
  filter(TSA_filter == "Y" | BEC_filter == "Y")

### Get number of sites in each management unit
sample_data2 <- sample_data2 %>%
  group_by(TSA_DESC, TSA_filter) %>%
  mutate(nsite_bytsa = n_distinct(CLSTR_ID)) %>%
  ungroup() %>%
  group_by(BECsub, BEC_filter) %>%
  mutate(nsite_bybec = n_distinct(CLSTR_ID)) %>%
  ungroup() 

sample_data3 <- sample_data2 %>%
  ### Retain only nsite >= 10 for reporting purpose
  filter((nsite_bytsa >= 10 & TSA_filter =="Y") |(nsite_bybec >= 10 & BEC_filter =="Y")) 

sample_data3 <- sample_data3 %>%
  ### Define a now visit number as VISIT_NUMBER may not be consecutive
  group_by(SITE_IDENTIFIER) %>%
  arrange(VISIT_NUMBER) %>%
  mutate(visit_number_new = row_number()) %>%
  ungroup()

### Save data for application
saveRDS(sample_data3, paste0(savepath,"sample_data.rds"))



################################################################################
### Create species data, site-age data based on sample data
###  - Input: sample_data, faib_spcsmries, faib_siteage
###  - Output: spcs_data, siteage_data
################################################################################

### Species data
spcs_data <- faib_spcsmries %>%
  filter(CLSTR_ID %in% sample_data$CLSTR_ID)

### Site age data
siteage_data <- faib_siteage %>%
  filter(CLSTR_ID %in% sample_data$CLSTR_ID)

siteage_data <- siteage_data %>%
  left_join(sample_data[,.(SITE_IDENTIFIER, CLSTR_ID, VISIT_NUMBER, 
                           TSA, TSA_DESC, BEC_ZONE, BEC_SBZ, PROJ_AGE_ADJ)], by = ("CLSTR_ID"))


### Save data for application
saveRDS(spcs_data, paste0(savepath,"spcs_data.rds"))
saveRDS(siteage_data, paste0(savepath,"siteage_data.rds"))


################################################################################
### Create site index data
###  - Input: sample_data, siteage_data, pspl, faib_siteage, faib_tree
###  - Output: SI_data
################################################################################

### Filter YSM samples
pspl1 <- pspl %>%
  filter(SITE_IDENTIFIER %in% unique(sample_data$SITE_IDENTIFIER))

### Join BEC zone information
pspl2 <- pspl1 %>%
  left_join(sample_data[,.(SITE_IDENTIFIER, TSA, TSA_DESC, BEC_ZONE, BEC_SBZ, PROJ_AGE_ADJ)], 
            by = ("SITE_IDENTIFIER"))

### Reshape PSPL data - warning due to the data type
PSPL_data <- melt(pspl2,
                  id.vars = c('SITE_IDENTIFIER', 'BEC_ZONE', 'BEC_SBZ'),
                  measure.vars = patterns("_SI"),
                  variable.name = "SPECIES",
                  value.name = "pspl_si")

PSPL_data <- PSPL_data %>%
  filter(!is.na(pspl_si)) %>%
  mutate(SPECIES = substr(SPECIES, 1, 2))

### Correction on SX based on BEC zone
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
  left_join(PSPL_data[,.(SITE_IDENTIFIER, SPECIES, pspl_si)] %>% distinct(), 
            by = c("SITE_IDENTIFIER", "SPECIES"))

### Match species
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

### Compute age at breast height for the site trees only, 
### since compiled data includes all trees with age information
site_age <- faib_tree %>%
  mutate(SUIT_SI_edit = ifelse(SUIT_SI!="N" & SUIT_TR=="Y" & SUIT_HT =="Y", "Y", "N")) %>%
  filter(CLSTR_ID %in% sample_data$CLSTR_ID, SUIT_SI_edit == "Y",
         TH_TREE %in% c("T", "L", "S", "O"), RESIDUAL == "N",
         (AGE_BH >= 5 & AGE_BH <= 150), HEIGHT > 1.5)  %>% 
  group_by(CLSTR_ID, SPECIES) %>%
  summarize(meanage = mean(AGE_BH, na.rm=T),
            meansi = mean(SI_TREE, na.rm=T)) %>%
  ungroup()

SI_data <- temp %>%
  left_join(site_age, by = c("CLSTR_ID", "SPECIES")) %>%
  mutate(ratio = meansi / pspl_si,
         ### Need to join with site index closest to age 50
         yrs_from_50 = abs(50-meanage),
         standage_from_50 = abs(50-PROJ_AGE_ADJ))


### Save data for application
saveRDS(SI_data, paste0(savepath,"SI_data.rds"))


################################################################################
### Create vegcomp data
###  - Input: sample_data, vegcomp1, pspl
###  - Output: vegcomp_pspl_sample
################################################################################

# *merge samples with 2021 vegcomp spatial overlay, by visit number;
# *merge with spatial lookup table;
vegcomp_sample <- sample_data %>%
  left_join(vegcomp1, by = c("SITE_IDENTIFIER"),
            suffix = c("_ismc", "_veg"))

# *merge with pspl layer;
vegcomp_pspl_sample <- vegcomp_sample %>%
  left_join(pspl, by = "SITE_IDENTIFIER")

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


### Save data for application
saveRDS(vegcomp_pspl_sample, paste0(savepath,"vegcomp_pspl_sample.rds"))


################################################################################
### Create treelist data with health condition
###  - Input: sample_data, faib_tree, vi_d, lookup_sev, lookup_rush, sev_rusch
###  - Output: Tree_FH_data
################################################################################

### Tree list with forest health data
### Join damage information to tree data
tree_data <- faib_tree %>%
  filter(CLSTR_ID %in% sample_data$CLSTR_ID) %>%
  left_join(vi_d[, .(CLSTR_ID, PLOT, TREE_NO, SPECIES,
                     LOSS1_IN,LOSS2_IN,LOSS3_IN,LOSS4_IN,LOSS5_IN,
                     LOC1_FRO,LOC2_FRO,LOC3_FRO,LOC4_FRO,LOC5_FRO,
                     FREQ1,FREQ2,FREQ3,FREQ4,FREQ5)], 
            by = c('CLSTR_ID', 'PLOT', 'TREE_NO', 'SPECIES')) 

FH_dat <- tree_data %>%
  filter(!is.na(PHF_TREE)) %>%
  ### Force all NA to blank for further processing
  mutate(DAM_AGNA = ifelse(is.na(DAM_AGNA), '', DAM_AGNA),
         DAM_AGNB = ifelse(is.na(DAM_AGNB), '', DAM_AGNB),
         DAM_AGNC = ifelse(is.na(DAM_AGNC), '', DAM_AGNC),
         DAM_AGND = ifelse(is.na(DAM_AGND), '', DAM_AGND),
         DAM_AGNE = ifelse(is.na(DAM_AGNE), '', DAM_AGNE),
         ### DAM_AGNA should be counted
         DAM_AGNA = ifelse(DAM_AGNA == '', 'O', DAM_AGNA),
         ba_ha = BA_TREE * PHF_TREE,
         vol_ha = VOL_WSV * PHF_TREE) %>%
  left_join(sample_data %>% select(CLSTR_ID, MEAS_YR), by = "CLSTR_ID")

### Function for updating U trees based on loss information
#update_dam_loss <- function(dam_agnt, loss) {
#  case_when(
#    grepl('U', dam_agnt, fixed = TRUE) & loss %in% c('FRK') ~ 'UF',
#    grepl('U', dam_agnt, fixed = TRUE) & loss %in% c('CRO', 'CRK') ~ 'UCR',
#    grepl('U', dam_agnt, fixed = TRUE) & loss %in% c('BTP') ~ 'UBT',
#    grepl('U', dam_agnt, fixed = TRUE) & loss %in% c('DTP') ~ 'UDT',
#    TRUE ~ dam_agnt)
#}

update_dam_loss <- function(dam_agnt, loss) {
  case_when(
    dam_agnt == "U" & loss %in% c('FRK') ~ 'UF',
    dam_agnt == "U" & loss %in% c('CRO', 'CRK') ~ 'UCR',
    dam_agnt == "U" & loss %in% c('BTP') ~ 'UBT',
    dam_agnt == "U" & loss %in% c('DTP') ~ 'UDT',
    TRUE ~ dam_agnt)
}

#*redefine unknown fhf associated with specific stem form damage;
#*repeat for the up to 5 damage agents, and up to 5 associated loss indicators specific to the 5 damage agents;
#FH_dat <- FH_dat %>%
#  mutate(
#    DAM_AGNA = update_dam_loss(DAM_AGNA, LOSS1_IN),
#    DAM_AGNB = ifelse(DAM_AGNA == 'U' & !(LOSS2_IN %in% c(NA, '')) & DAM_AGNB %in% c(NA, ''), 
#                      update_dam_loss(DAM_AGNA, LOSS2_IN), DAM_AGNB),
#    DAM_AGNC = ifelse(DAM_AGNA == 'U' & (LOSS3_IN %in% c(NA, '')) & DAM_AGNC %in% c(NA, ''), 
#                      update_dam_loss(DAM_AGNA, LOSS3_IN), DAM_AGNC),
#    DAM_AGND = ifelse(DAM_AGNA == 'U' & (LOSS4_IN %in% c(NA, '')) & DAM_AGND %in% c(NA, ''), 
#                      update_dam_loss(DAM_AGNA, LOSS4_IN), DAM_AGND),
#    DAM_AGNE = ifelse(DAM_AGNA == 'U' & (LOSS5_IN %in% c(NA, '')) & DAM_AGNE %in% c(NA, ''), 
#                      update_dam_loss(DAM_AGNA, LOSS5_IN), DAM_AGNE)
#  )

FH_dat1 <- FH_dat %>%
  mutate(
    DAM_AGNA = update_dam_loss(DAM_AGNA, LOSS1_IN),
    DAM_AGNB = update_dam_loss(DAM_AGNB, LOSS2_IN),
    DAM_AGNC = update_dam_loss(DAM_AGNC, LOSS3_IN),
    DAM_AGND = update_dam_loss(DAM_AGND, LOSS4_IN),
    DAM_AGNE = update_dam_loss(DAM_AGNE, LOSS5_IN)
  )

# *ignore minor incidence of UF and UCR (ie,. where severity=N, as recorded in 2021 and later measurements);
FH_dat <- FH_dat %>%
  mutate(DAM_AGNA = ifelse(DAM_AGNA %in% c('UF', 'UCR') & SEV_A == "N", "O", DAM_AGNA),
         DAM_AGNB = ifelse(DAM_AGNB %in% c('UF', 'UCR') & SEV_B == "N", "O", DAM_AGNB),
         DAM_AGNC = ifelse(DAM_AGNC %in% c('UF', 'UCR') & SEV_C == "N", "O", DAM_AGNC),
         DAM_AGND = ifelse(DAM_AGND %in% c('UF', 'UCR') & SEV_D == "N", "O", DAM_AGND),
         DAM_AGNE = ifelse(DAM_AGNE %in% c('UF', 'UCR') & SEV_E == "N", "O", DAM_AGNE))

### If other damage agent exists, remove O
FH_dat <- FH_dat %>%
  mutate(DAM_AGNA = ifelse(DAM_AGNA == 'O' & !(DAM_AGNB %in% c(NA, '')), '', DAM_AGNA),
         
         DAM_AGNB = ifelse(DAM_AGNB == 'O' & !(DAM_AGNA %in% c('O', '', NA)), '', DAM_AGNB),
         DAM_AGNB = ifelse(DAM_AGNB == 'O' & !(DAM_AGNC %in% c('O', '', NA)), '', DAM_AGNB),
         DAM_AGNB = ifelse(DAM_AGNB == 'O' & !(DAM_AGND %in% c('O', '', NA)), '', DAM_AGNB),
         DAM_AGNB = ifelse(DAM_AGNB == 'O' & !(DAM_AGNE %in% c('O', '', NA)), '', DAM_AGNB),
         
         DAM_AGNC = ifelse(DAM_AGNC == 'O' & !(DAM_AGNA %in% c('O', '', NA)), '', DAM_AGNC),
         DAM_AGNC = ifelse(DAM_AGNC == 'O' & !(DAM_AGNB %in% c('O', '', NA)), '', DAM_AGNC),
         DAM_AGNC = ifelse(DAM_AGNC == 'O' & !(DAM_AGND %in% c('O', '', NA)), '', DAM_AGNC),
         DAM_AGNC = ifelse(DAM_AGNC == 'O' & !(DAM_AGNE %in% c('O', '', NA)), '', DAM_AGNC),
         
         DAM_AGND = ifelse(DAM_AGND == 'O' & !(DAM_AGNA %in% c('O', '', NA)), '', DAM_AGND),
         DAM_AGND = ifelse(DAM_AGND == 'O' & !(DAM_AGNB %in% c('O', '', NA)), '', DAM_AGND),
         DAM_AGND = ifelse(DAM_AGND == 'O' & !(DAM_AGNC %in% c('O', '', NA)), '', DAM_AGND),
         DAM_AGND = ifelse(DAM_AGND == 'O' & !(DAM_AGNE %in% c('O', '', NA)), '', DAM_AGND),
         
         DAM_AGNE = ifelse(DAM_AGNE == 'O' & !(DAM_AGNA %in% c('O', '', NA)), '', DAM_AGNE),
         DAM_AGNE = ifelse(DAM_AGNE == 'O' & !(DAM_AGNB %in% c('O', '', NA)), '', DAM_AGNE),
         DAM_AGNE = ifelse(DAM_AGNE == 'O' & !(DAM_AGNC %in% c('O', '', NA)), '', DAM_AGNE),
         DAM_AGNE = ifelse(DAM_AGNE == 'O' & !(DAM_AGND %in% c('O', '', NA)), '', DAM_AGNE))

#### If the DAM_AGNA is U and there are other loss indicator exists
#FH_dat <- FH_dat %>%
#  mutate(DAM_AGNA = ifelse(DAM_AGNA == 'U' & !is.na(DAM_AGNB) & grepl('U', DAM_AGNB), DAM_AGNB, DAM_AGNA))

#FH_dat <- FH_dat %>%
#  mutate(DAM_AGNB = ifelse(DAM_AGNA == DAM_AGNB & grepl('U', DAM_AGNA, fixed = TRUE), 'U', DAM_AGNB))

# *look at numeric values only in severity class;
FH_dat <- FH_dat %>%
  mutate(SEVPERC_A = as.numeric(gsub("[^\\d]+", "", SEV_A, perl = T)),
         SEVPERC_B = as.numeric(gsub("[^\\d]+", "", SEV_B, perl = T)),
         SEVPERC_C = as.numeric(gsub("[^\\d]+", "", SEV_C, perl = T)),
         SEVPERC_D = as.numeric(gsub("[^\\d]+", "", SEV_D, perl = T)),
         SEVPERC_E = as.numeric(gsub("[^\\d]+", "", SEV_E, perl = T))) %>%
  data.frame

## *only output unknown damage agents if leading damage agent only; - didnt seem to be applied
#update_undefined_dam <- function(dam_agna, dam_agnt) {
#  case_when(
#    grepl('U', dam_agna, fixed = TRUE) & dam_agnt %in% c(NA, '','UF','UCR','UBT','UDT','U') ~ '',
#    # *all other damage agents get output regardless of position;
#    TRUE ~ dam_agnt)
#}
#
#FH_dat <- FH_dat %>%
#  mutate(DAM_AGNB = update_undefined_dam(DAM_AGNA, DAM_AGNB),
#         DAM_AGNC = update_undefined_dam(DAM_AGNA, DAM_AGNC),
#         DAM_AGND = update_undefined_dam(DAM_AGNA, DAM_AGND),
#         DAM_AGNE = update_undefined_dam(DAM_AGNA, DAM_AGNE),
#         DAM_AGNC = update_undefined_dam(DAM_AGNB, DAM_AGNC),
#         DAM_AGND = update_undefined_dam(DAM_AGNB, DAM_AGND),
#         DAM_AGNE = update_undefined_dam(DAM_AGNB, DAM_AGNE),
#         DAM_AGND = update_undefined_dam(DAM_AGNC, DAM_AGND),
#         DAM_AGNE = update_undefined_dam(DAM_AGNC, DAM_AGNE),
#         DAM_AGNE = update_undefined_dam(DAM_AGND, DAM_AGNE))

# *expande tree data so up to 5 damage agents per tree each on their own record for tracking all agents per tree;
FH_dat1 <- melt(setDT(FH_dat),
                id.vars = c('SITE_IDENTIFIER', 'CLSTR_ID', 'VISIT_NUMBER', 'PLOT', 'MEAS_YR', 'MEAS_INTENSE',
                            'TREE_NO', 'RESIDUAL', 'TREE_WT', 'WALKTHRU_STATUS', 'TH_TREE', 
                            'DBH', 'HEIGHT', 'SPECIES', 'LV_D', 'S_F', 'COMP_CHG', 'PHF_TREE', 
                            'VOL_WSV', 'VOL_MER', 'VOL_NTWB', 'AGE_BH', 'AGE_TOT', 
                            'SI_TREE', "SUIT_TR", "SUIT_HT", "SUIT_SI",
                            'BA_TREE', 'ba_ha', 'vol_ha'),
                measure.vars = patterns("^DAM_AGN", "^SEV_", "^SEVPERC_"),
                variable.name = "DAM_NUM",
                value.name = c("AGN", "SEV", "SEVPERC"))

FH_dat1 <- FH_dat1 %>%
  mutate(SPC_GRP1 = substr(SPECIES,1,2)) %>%
  # *further grouping of multiple species labels, and for all deciduous;
  mutate(SPC_GRP1 = ifelse(SPECIES %in% decidspc, 'DE', SPC_GRP1))

FH_dat1 <- FH_dat1 %>%
  mutate(AGN = case_when(
    # *further corrections, error caught by H.Kope;
    #AGN %in% c('DBS') & SEV %in% c('BC', 'SC') ~ 'DSB',
    AGN %in% c('IAG') & SPC_GRP1 %in% c('DE') ~ '',
    AGN %in% c('DBS') & SPC_GRP1 %in% c('PW') ~ '',
    AGN %in% c('DFE') & SPC_GRP1 %in% c('FD') ~ '',
    AGN %in% c('DFL') & SPC_GRP1 %in% c('BL','FD') ~ '',
    AGN %in% c('DSB') & SPC_GRP1 %in% c('PL') ~ '',
    AGN %in% c('DSG') & SPC_GRP1 %in% c('BL') ~ '',
    AGN %in% c('IBM') & SPC_GRP1 %in% c('BL','SW') ~ '',
    AGN %in% c('IBS') & SPC_GRP1 %in% c('PL') ~ '',
    #AGN %in% c('IDE') & SPC_GRP1 %in% c('PL') ~ '',
    #AGN %in% c('DFB') & SPC_GRP1 %in% c('FD','SW') ~ '',
    #AGN %in% c('DM') & SPC_GRP1 %in% c('BL') ~ 'DBF',
    #AGN %in% c('DM') & SPC_GRP1 %in% c('PL') ~ 'DMP',
    #AGN %in% c('ISP') & SPC_GRP1 %in% c('BL') ~ '',
    # *edit corrections, error caught by T.Ebata on spruce budworm on Fd;
    AGN %in% c('IDE') & SPC_GRP1 %in% c('FD') & 
      CLSTR_ID %in% c(sample_data[sample_data$MGMT_UNIT %in% c('TSA11_Kamloops','TSA29_Williams_Lake'),]$CLSTR_ID) ~ 'IDW',
    TRUE ~ AGN))

#FH_dat1 <- FH_dat1 %>%
#  mutate(AGN = ifelse(DAM_NUM == 1 & AGN == "", "O", AGN))

FH_dat1_temp <- FH_dat1 %>%
  group_by(CLSTR_ID, PLOT, TREE_NO) %>%
  mutate(agn_all = paste(AGN, collapse = "")) %>%
  ungroup() %>%
  mutate(AGN = ifelse(agn_all == "" & DAM_NUM == 1, 'O', AGN))

# *for multiple occurrences of the same damage agent, only keep the lowest position 
# *(ie, lowest 1st digit in severity class);
FH_dat1_1_1 <- FH_dat1 %>%
  filter(!is.na(AGN), AGN != '') %>%
  arrange(CLSTR_ID, PLOT, TREE_NO, AGN, DAM_NUM) %>%
  group_by(CLSTR_ID, PLOT, TREE_NO, AGN) %>%
  slice(1)

### Assign new DAM_NUM
FH_dat1_1_1 <- FH_dat1_1_1 %>%
  ungroup() %>%
  mutate(DAM_NUM_old = DAM_NUM) %>%
  arrange(CLSTR_ID, PLOT, TREE_NO, DAM_NUM_old) %>%
  group_by(CLSTR_ID, PLOT, TREE_NO) %>%
  mutate(DAM_NUM = row_number(DAM_NUM_old))

#### If a tree has multiple records of same damage agent with different severity, leave the most severity one only
#FH_dat1_1 <- FH_dat1 %>%
#  filter(!is.na(AGN), AGN != '') %>%
#  group_by(CLSTR_ID, PLOT, TREE_NO, AGN) %>%
#  mutate(dups = ifelse(n()>1, "Yes", "No"),
#         selected = ifelse(SEVPERC == max(SEVPERC), "Yes", "No"),
#         selected = ifelse(is.na(selected), "Yes", selected))

#### If duplicated damage agent for a same tree has no severity information, leave only one
#FH_dat1_1_1 <- FH_dat1_1 %>%
#  group_by(CLSTR_ID, PLOT, TREE_NO, AGN) %>%
#  mutate(SEVPERC_new = ifelse(DAM_NUM == 1 & selected == "No", max(SEVPERC), SEVPERC),
#         selected = ifelse(SEVPERC_new == max(SEVPERC_new), "Yes", "No"),
#         selected = ifelse(is.na(selected), "Yes", selected)) %>%
#  filter(selected == "Yes") %>%
#  arrange(CLSTR_ID, PLOT, TREE_NO, AGN, DAM_NUM, desc(dups), desc(selected)) %>%
#  group_by(CLSTR_ID, PLOT, TREE_NO, AGN, dups, selected) %>%
#  slice(1)

# *start categorizing severity;
# *first import ismc damage agent to severity class lookup table;
lookup_sev1 <- lookup_sev %>%
  mutate(dam_3letter = DAMAGE_AGENT_CODE,
         severity_grp = DAMAGE_AGENT_SEVERITY_CODE) %>%
  select(dam_3letter, severity_grp) %>%
  distinct()

# *next, import severity rating lookup table created by D.Rush 2021-jan;
lookup_rush2 <- lookup_rush %>%
  mutate_at(vars(matches("dam")), trimws)

lookup_rush3 <- lookup_rush2 %>%
  pivot_longer(cols = starts_with("dam"), names_to = "dam", 
               values_to = "dam_3letter",values_drop_na = TRUE) %>%
  distinct()

lookup_sev2 <- lookup_sev1 %>%
  left_join(lookup_rush3, by = c("dam_3letter"))

### Hard coded
lookup_sev3 <- lookup_sev2[!(lookup_sev2$dam_3letter %in% c('NW', 'NWS', "NWT") & lookup_sev2$allowed == "1-100"),]

FH_dat1_2 <- FH_dat1_1_1 %>%
  left_join(lookup_sev3, by = c("AGN" = "dam_3letter"))

FH_dat1_2 <- FH_dat1_2 %>%
  ungroup() %>%
  # *get percent encirclement for stem rust severity collected since 2017;
  mutate(dam_severity_adj = gsub(' ', '', toupper(SEV)),
         sev_class = ifelse(dam_severity_adj == "", "UNKN", NA),
         # *look at numeric values only in severity class;
         sev_class_num = ifelse(grepl("[A-Za-z]", dam_severity_adj, perl = T) == F, 
                                as.numeric(dam_severity_adj), NA))

# *use only percent encirclemt for most recent stem rust severity ratings;
FH_dat1_2 <- FH_dat1_2 %>%
  mutate(dam_severity_adj = ifelse(severity_grp == "STEMRUST1", 
                                   substr(dam_severity_adj, 2, 2), dam_severity_adj)) %>%
  mutate(sev_class_num = ifelse(severity_grp == "STEMRUST1" & grepl("[A-Za-z]", dam_severity_adj, perl = T) == F, 
                                as.numeric(gsub("[A-Za-z]", "", dam_severity_adj)), sev_class_num))

FH_dat1_2 <- FH_dat1_2 %>%
  mutate(dam_severity_adj = ifelse(grepl("[A-Za-z]", dam_severity_adj, perl = T), 
                                   gsub("[0-9]", "", dam_severity_adj), dam_severity_adj)) 

FH_dat1_2 <- FH_dat1_2 %>%
  mutate(sev_class = ifelse(sev_class_num >= Low_min & sev_class_num <= Low_max, 'LOW', 
                            ifelse(sev_class_num >= Mod_min & sev_class_num <= Mod_max, 'MOD',
                                   ifelse(sev_class_num >= High_min, 'HIGH', sev_class))))

FH_dat1_2 <- FH_dat1_2 %>%
  mutate(sev_class = ifelse(sev_class %in% c("UNKN", NA) & dam_severity_adj %in% c(Low_class1, Low_class2), 'LOW',
                            ifelse(sev_class %in% c("UNKN", NA) & dam_severity_adj %in% c(Mod_class1, Mod_class2), 'MOD',
                                   ifelse(sev_class %in% c("UNKN", NA) & dam_severity_adj %in% c(High_class1, High_class2), 
                                          'HIGH', sev_class)
                            )))

# *default to severity class when no match;
FH_dat1_2 <- FH_dat1_2 %>%
  mutate(sev_class = ifelse(sev_class %in% c("", NA), "UNKN", sev_class))

FH_dat2 <- FH_dat1_2 %>%
  ungroup() %>%
  filter(!is.na(AGN), AGN != '') %>%
  mutate(n = n_distinct(SITE_IDENTIFIER),
         SPC_GRP1 = substr(SPECIES,1,2),
         SPC_GRP1 = ifelse(SPECIES %in% decidspc, 'DE', SPC_GRP1))

### Immediate vs. incremental mortality
# *assign a tree either as a current mortality (lethal agents), 
# or future mortality (assigned as an incremental 0.25%/yr mort rate;
FH_dat2 <- FH_dat2 %>%
  # *immediate / imminent mortality, at 90%;
  mutate(mort_code = case_when(AGN %in% c('AB','DRA','DRB','DRC','DRL','DRN','DRR','DRT','DSB', 
                                          'IB', 'IBB', 'IBI', 'IBM', 'IBP', 'IBS', 
                                          'IBW', 'ISW', 'ND', 'NF', 'NS', 'NW', 'NWS', 
                                          'NY', 'TC','DSC') ~ 1,
                               # *further assessment on fire damage (exclude FD);
                               # *immediate / imminent mortality, at 90%;
                               AGN == 'NB' & !(SPC_GRP1 %in% c('FD','LW')) ~ 1,
                               # *immediate only for the following fh agents greater than 80% severity;
                               AGN %in% c('DF','DFE','DFS') & SEVPERC >= 80 ~ 1,
                               # *incremental mortality, at 0.25%/yr for the following conifer diseases;
                               AGN %in% c('DB', 'DM', 'DMH', 'DMP', 'DSA', 'DSE', 'DSG','DSS') ~ 2,
                               #AGN %in% c('DSG','DSS') & MEAS_YR < 2017 ~ 2,
                               # *incremental growth loss, at 0.25%/yr for insects with percent of tree affected, 
                               # use 80%;
                               AGN %in% c('IAB', 'IDW', 'IDB', 'IDE', 'IDH', 'IDI', 'IDT') &
                                 SEVPERC >= 80 ~ 2,
                               # *incremental growth loss, at 0.25%/yr, for terminal weevil, 
                               # for all cases other than minor crooks / forks;
                               AGN %in% c('IWP', 'IWS') & grepl('N', SEV) == FALSE ~ 2,
                               TRUE ~ 3))

#FH_dat2 <- FH_dat2 %>%
#  mutate(mort_code = case_when(AGN %in% c('AB','DRA','DRB','DRC','DRL','DRN','DRR','DRT','DSB', 
#                                          'IB', 'IBB', 'IBI', 'IBM', 'IBP', 'IBS', 
#                                          'IBW', 'ISW', 'ND', 'NF', 'NS', 'NW', 'NWS', 
#                                          'NY', 'TC','DSC') ~ 1,
#                               AGN == 'NB' & !(SPC_GRP1 %in% c('FD','LW')) ~ 1,
#                               AGN %in% c('DF','DFE','DFS') & SEVPERC >= 80 ~ 1,
#                               AGN %in% c('DB', 'DM', 'DMH', 'DMP', 'DSA', 'DSE') ~ 2,
#                               AGN %in% c('DSG','DSS') & MEAS_YR < 2017 ~ 2,
#                               AGN %in% c('IAB', 'IDW', 'IDB', 'IDE', 'IDH', 'IDI', 'IDT') &
#                                 SEVPERC >= 80 ~ 2,
#                               AGN %in% c('IWP', 'IWS') & grepl('N', SEV) == FALSE ~ 2,
#                               TRUE ~ 3))

### If two or more mort_code appear on a same trees, use the most severe one.
FH_dat2 <- FH_dat2 %>%
  group_by(SITE_IDENTIFIER, CLSTR_ID, VISIT_NUMBER, PLOT, TREE_NO) %>%
  mutate(mort_flag = min(mort_code),
         AGN_new = ifelse(mort_flag != mort_code, AGN[which.min(mort_code)], AGN))

FH_dat2 <- FH_dat2 %>%
  # *create new resid flag, which combines different sources;
  mutate(RESID_GRP = ifelse(RESIDUAL %in% c('Y','R','V') | TH_TREE == 'V', 'Y', 'N'),
         # *create dbh classes;
         DBH_CLASS = round(DBH/5)*5)

FH_dat2 <- FH_dat2 %>%
  mutate(len_dam = nchar(AGN))  %>%
  rowwise() %>% 
  mutate(dam_1letter = toupper(substr(AGN, 1, 1)),
         dam_2letter = toupper(substr(AGN, 1, min(len_dam,2))),
         dam_3letter = toupper(substr(AGN, 1, min(len_dam,3))))

FH_dat2 <- FH_dat2 %>%
  mutate(dam_class = case_when(dam_1letter %in% c('O', '') ~ 'None',
                               dam_1letter == 'U' ~ 'Unknown',
                               dam_1letter == 'N' ~ 'Abiotic',
                               dam_1letter == 'D' ~ 'Disease',
                               dam_1letter == 'I' ~ 'Insect',
                               dam_1letter == 'T' ~ 'Treatment',
                               dam_1letter == 'A' ~ 'Animal',
                               dam_1letter == 'X' ~ 'Frk_Crk_Btp',
                               TRUE ~ ''))

### Restructure the severity rating data
sev_rusch1 <- sev_rusch %>%
  pivot_longer(
    cols = starts_with("unkn_sev"),
    names_to = "unkn_sev",
    names_prefix = 'unkn_sev_',
    values_to = "SEV",
    values_drop_na = TRUE
  )

# *merge corrections to severity classification;
FH_dat3 <- FH_dat2 %>%
  left_join(sev_rusch1[, c('dam_3letter', 'SEV', 'corr_sev')], 
            by = c('dam_3letter', 'SEV'))

# *replace unknown severity with corrected severity, where matches present;
FH_dat3 <- FH_dat3 %>%
  mutate(dam_severity = ifelse(is.na(corr_sev), SEV, corr_sev)) %>%
  data.table

FH_dat3 <- FH_dat3 %>%
  mutate(AGN = ifelse(AGN == '', 'O', AGN))

Tree_FH_data <- FH_dat3 %>%
  select(SITE_IDENTIFIER, CLSTR_ID, VISIT_NUMBER, PLOT, TREE_NO, RESIDUAL, MEAS_YR, MEAS_INTENSE,
         TREE_WT, TH_TREE, DBH, HEIGHT, SPECIES, SPC_GRP1, LV_D, S_F, COMP_CHG, WALKTHRU_STATUS, 
         PHF_TREE, VOL_WSV, VOL_MER, VOL_NTWB, AGE_BH, AGE_TOT,
         SI_TREE, SUIT_TR, SUIT_HT, SUIT_SI,
         BA_TREE, ba_ha, vol_ha, DAM_NUM, AGN, SEV, SEVPERC, 
         severity_grp, dam, dam_severity_adj, 
         sev_class, sev_class_num, n, mort_code, mort_flag, AGN_new,
         RESID_GRP, DBH_CLASS, dam_1letter, dam_2letter, dam_3letter,
         dam_class, corr_sev, dam_severity)

### if a tree is identified as a residual in any visit, then it is a residual across all visits.
Tree_FH_data <- Tree_FH_data %>%
  rename(RESIDUAL_old = RESIDUAL) %>%
  group_by(SITE_IDENTIFIER, TREE_NO) %>%
  mutate(RESIDUAL = case_when("Y" %in% RESIDUAL_old ~ "Y", 
                              TRUE ~ RESIDUAL_old)) %>%
  ungroup() %>%
  data.table

Tree_FH_data1 <- Tree_FH_data %>%
  left_join(sample_data %>% select(SITE_IDENTIFIER, VISIT_NUMBER, visit_number_new),
            by = c('SITE_IDENTIFIER', 'VISIT_NUMBER')) %>%
  mutate(tree_id = paste0(SITE_IDENTIFIER, "-", TREE_NO),
         phf_coc = PHF_TREE,
         resid_coc = RESIDUAL,
         comp_chg_coc = COMP_CHG,
         species_coc = SPECIES,
         lvd_coc = LV_D,
         sf_coc = S_F)

# *run checks across measurements;
### Takes a while..
for (i in unique(Tree_FH_data1$tree_id)){
  
  #max_meas <- max(Tree_FH_data1[Tree_FH_data1$tree_id == i, ]$visit_number_new)
  meas_no <- sort(unique(Tree_FH_data1[Tree_FH_data1$tree_id == i, ]$visit_number_new))
  
  if (length(meas_no) > 1){
    
    for (j in meas_no){
      
      a1 <- Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                            Tree_FH_data1$DAM_NUM == 1 &
                            Tree_FH_data1$visit_number_new == j, ]
      a2 <- Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                            Tree_FH_data1$DAM_NUM == 1 &
                            Tree_FH_data1$visit_number_new == j + 1, ]
      # *ingress trees;
      if (nrow(a1) == 0){
        Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                        Tree_FH_data1$visit_number_new == j + 1, ]$comp_chg_coc <- ifelse(a2$lvd_coc == "L", "I", "M")
      } else if (nrow(a2) == 0){
        ## *live at first msmt, missing at second msmt, assign as mortality, and assume dead fallen;
        #Tree_FH_data1[Tree_FH_data1$tree_id == i & 
        #                Tree_FH_data1$visit_number_new == j, ]$comp_chg_coc <- "M"
        #Tree_FH_data1[Tree_FH_data1$tree_id == i & 
        #                Tree_FH_data1$visit_number_new == j, ]$lvd_coc <- "D"
        #Tree_FH_data1[Tree_FH_data1$tree_id == i & 
        #                Tree_FH_data1$visit_number_new == j, ]$sf_coc <- "F"
      } else {
        # *where tree was previously recorded as dead, but subsequently recorded as live, 
        # then believe second measure;
        # *and redefine tree as alive at first measure;
        if (a1$lvd_coc == "D" & a2$lvd_coc == "L"){
          Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                          Tree_FH_data1$visit_number_new == j, ]$lvd_coc <- a2$lvd_coc
        }
        # *for components of change analysis, need to constrain phf to first measure;
        if (!is.na(a2$phf_coc) & a1$phf_coc != a2$phf_coc){
          Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                          Tree_FH_data1$visit_number_new == j + 1, ]$phf_coc <- a1$phf_coc
        }
        # *fill in residual classification if recorded at one measurement , but not the next;
        # *assign as residual across both measurements;
        if (a1$RESIDUAL != a2$RESIDUAL & (a1$RESIDUAL == "Y" | a2$RESIDUAL == "Y")){
          Tree_FH_data1[Tree_FH_data1$tree_id == i, ]$resid_coc <- "Y"
        }
        # *components of change;
        # *survivor trees;
        if (a1$lvd_coc == "L" & a2$lvd_coc == "L" & a2$sf_coc == "S"){
          Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                          Tree_FH_data1$visit_number_new == j + 1, ]$comp_chg_coc <- "S"
        }
        # *fallen live, assume this will become mortality;
        if (a1$lvd_coc == "L" & a2$lvd_coc == "L" & a2$sf_coc == "F"){
          Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                          Tree_FH_data1$visit_number_new == j + 1, ]$lvd_coc <- "D"
          Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                          Tree_FH_data1$visit_number_new == j + 1, ]$comp_chg_coc <- "M"
        }
        # *mortality : trees that died between measurements;
        if (a1$LV_D == "L" & a2$lvd_coc == "D"){
          Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                          Tree_FH_data1$visit_number_new == j + 1, ]$comp_chg_coc <- "M"
        }
        # *if second measure is unknown then use first measure species;
        if (a2$SPECIES == "XC"){
          Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                          Tree_FH_data1$visit_number_new == j, ]$species_coc <- a1$SPECIES
          # *otherwise resolve inconsistencies by believing second measure;
        } else if (a1$SPECIES != a2$SPECIES) {
          Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                          Tree_FH_data1$visit_number_new == j, ]$species_coc <- a2$SPECIES
        }
      }
    }
  }
  if (length(meas_no) == 1){
    if (unique(Tree_FH_data1[Tree_FH_data1$tree_id == i, ]$visit_number_new) == 1){
      Tree_FH_data1[Tree_FH_data1$tree_id == i, ]$comp_chg_coc <- ""
    } else {
      Tree_FH_data1[Tree_FH_data1$tree_id == i, ]$comp_chg_coc <- 
        ifelse(Tree_FH_data1[Tree_FH_data1$tree_id == i, ]$lvd_coc == "L", "I", 
               Tree_FH_data1[Tree_FH_data1$tree_id == i, ]$comp_chg_coc)
    }
  }
}


### Save data for application
saveRDS(Tree_FH_data1, paste0(savepath,"Tree_FH_data.rds"))


################################################################################
### Create regen data 
###  - Input: sample_data, MSYT_current_input, VDYP_input
###  - Output: regen_data
################################################################################

### Process MSYT input table
msyt_input1 <- MSYT_current_input %>%
  left_join(MSYT_reference, by = c("FEATURE_ID"), 
            suffix = c("", "_ref"))

msyt_input2 <- msyt_input1 %>%
  dplyr::select(FEATURE_ID, BEC_ZONE, BEC_SUBZONE, 
                PLANTED_SPECIES1:PLANTED_SPECIES5, 
                PLANTED_DENSITY1:PLANTED_DENSITY5, 
                GENETIC_WORTH1:GENETIC_WORTH5, 
                NATURAL_SPECIES1:NATURAL_SPECIES5,
                NATURAL_DENSITY1:NATURAL_DENSITY5) 

msyt_input3 <- tidyr::pivot_longer(msyt_input2, cols = matches("\\d{1}$"), 
                                   names_pattern = "(\\w+)(\\d{1})", 
                                   names_to = c(".value", "num")) %>% 
  filter(NATURAL_DENSITY > 0 | PLANTED_DENSITY > 0) %>% 
  mutate(regen_src = ifelse(PLANTED_DENSITY > 0, "P", ifelse(NATURAL_DENSITY > 0 , "N", NA)))

msyt_input3_1 <- msyt_input3 %>%  
  select(FEATURE_ID, BEC_ZONE, BEC_SUBZONE, 
         num, GENETIC_WORTH, ends_with("_SPECIES")) %>%
  tidyr::pivot_longer(cols = ends_with("_SPECIES"), 
                      names_to = 'regen_src', 
                      values_to = 'SPECIES')

msyt_input3_1 <- msyt_input3_1 %>%
  mutate(regen_src = substr(regen_src, 1, 1),
         SPECIES_recode = toupper(substr(SPECIES, 1, 2)),
         SPC_GRP1 =  ifelse(SPECIES_recode %in% decidspc, 'DE', SPECIES_recode))

msyt_input3_2 <- msyt_input3 %>%
  select(FEATURE_ID, BEC_ZONE, BEC_SUBZONE, 
         num, GENETIC_WORTH, ends_with("_DENSITY")) %>%
  tidyr::pivot_longer(cols = ends_with("_DENSITY"), 
                      names_to = 'regen_src', 
                      values_to = 'DENSITY')

msyt_input3_2 <- msyt_input3_2 %>%
  mutate(regen_src = substr(regen_src, 1, 1))

msyt_input4 <- merge(msyt_input3_1, msyt_input3_2, 
                     by =c('FEATURE_ID','num', 'GENETIC_WORTH', 'regen_src'), 
                     suffixes = c("","_den")) %>%
  distinct()

regen_data1 <- msyt_input4 %>%
  filter(SPECIES != "") %>%
  select(FEATURE_ID, num, SPECIES = SPECIES_recode, SPC_GRP1, DENSITY, regen_src)

regen_data2 <- regen_data1 %>%
  group_by(FEATURE_ID) %>%
  summarise(sum_density = sum(DENSITY, na.rm = T))

regen_data3 <- regen_data1 %>%
  left_join(regen_data2, by = "FEATURE_ID") %>%
  mutate(SPECIES_PCT = DENSITY/sum_density)

regen_data4 <- regen_data3 %>%
  group_by(FEATURE_ID, SPECIES, SPC_GRP1) %>%
  summarise(DENSITY = sum(DENSITY, na.rm = T),
            SPECIES_PCT = sum(SPECIES_PCT, na.rm = T)) %>%
  select(FEATURE_ID, SPECIES, SPC_GRP1, SPECIES_PCT) %>%
  ungroup()

### Process VDYP input table
vdyp_input1 <- VDYP_input %>%
  filter(VDYP7_LAYER_CD == "P") %>%
  select(FEATURE_ID, SPECIES_CD_1, SPECIES_PCT_1, SPECIES_CD_2, SPECIES_PCT_2,
         SPECIES_CD_3, SPECIES_PCT_3, SPECIES_CD_4, SPECIES_PCT_4,
         SPECIES_CD_5, SPECIES_PCT_5, SPECIES_CD_6, SPECIES_PCT_6)

vdyp_input2 <- tidyr::pivot_longer(vdyp_input1, cols = matches("\\d{1}$"), 
                                   names_pattern = "(\\w+)(\\d{1})", 
                                   names_to = c(".value", "num"))  %>%
  filter(SPECIES_CD_ != "") %>%
  mutate(SPECIES_recode = toupper(substr(SPECIES_CD_, 1, 2)),
         SPC_GRP1 =  ifelse(SPECIES_recode %in% decidspc, 'DE', SPECIES_recode))

vdyp_input3 <- vdyp_input2 %>%
  ### Only where the MSYT input is unavailable
  filter(!(FEATURE_ID %in% regen_data4$FEATURE_ID)) %>%
  select(FEATURE_ID, num, SPECIES = SPECIES_recode, SPC_GRP1, SPECIES_PCT = SPECIES_PCT_) %>%
  group_by(FEATURE_ID, SPECIES, SPC_GRP1) %>%
  summarise(SPECIES_PCT = sum(SPECIES_PCT, na.rm = T)/100) %>%
  ungroup()

### Combine MSYT input and VDYP input
regen_tsr_input <- rbind(regen_data4, vdyp_input3)

### Joint with sample data
regen_tsr_input1 <- sample_data %>%
  select(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, FEATURE_ID, FEATURE_ID_2022,
         BEC_ZONE, BEC_SBZ, BEC_VAR, FIRST_MSMT, LAST_MSMT, MEAS_YR,
         MGMT_UNIT, TSA_DESC) %>%
  left_join(regen_tsr_input, 
            by = c("FEATURE_ID_2022" = "FEATURE_ID"))


### Save data for application
saveRDS(regen_tsr_input1, paste0(savepath,"regen_data.rds"))


################################################################################
### Create current volume data 
###  - Input: sample_data, Tree_FH_data, vegcomp_pspl_sample, 
###           MSYT_reference, MSYT_current_input, 
###           VDYP_input, VDYP_all, tass_output
###  - Output: ysm_msyt_vdyp_volume, msyt_volproj
################################################################################

### First, compute YSM ground sample current volume
ysm_trees <- Tree_FH_data %>%
  ### Select live standing trees
  filter(S_F == "S", !is.na(PHF_TREE), !is.na(DBH), 
         LV_D == "L", DAM_NUM == 1) %>%
  # *add gross merch volume to summary outputs;
  mutate(vol_mer_ha = VOL_MER * PHF_TREE,
         vol_wsv_ha = VOL_WSV * PHF_TREE,
         vol_ntwb_ha = VOL_NTWB * PHF_TREE,
         SPC_GRP1 = substr(SPECIES,1,2),
         SPC_GRP1 = ifelse(SPECIES %in% decidspc, 'DE', SPC_GRP1))

### Additional columns for without residuals
ysm_trees_nores <- Tree_FH_data %>%
  filter(S_F == "S", !is.na(PHF_TREE), !is.na(DBH), 
         LV_D == "L", DAM_NUM == 1, RESIDUAL != "Y") %>%
  # *add gross merch volume to summary outputs;
  mutate(vol_mer_ha_nores = VOL_MER * PHF_TREE,
         vol_wsv_ha_nores = VOL_WSV * PHF_TREE,
         vol_ntwb_ha_nores = VOL_NTWB * PHF_TREE) %>%
  select(CLSTR_ID, TREE_NO, SPECIES, 
         vol_mer_ha_nores, vol_wsv_ha_nores, vol_ntwb_ha_nores) %>%
  distinct()

ysm_trees1 <- ysm_trees %>%
  left_join(ysm_trees_nores,
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


### Second, process MSYT projected volume data
### Get list of MSYT FEATURE_ID and associate it with SITE_IDENTIFIER
sample_data1 <- sample_data %>% 
  left_join(vegcomp_pspl_sample %>% select(CLSTR_ID, LAYER_ID),
            by = c("CLSTR_ID"))

msyt <- sample_data1 %>% 
  select(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, FEATURE_ID, FEATURE_ID_2022, 
         MGMT_UNIT, MEAS_YR, PROJ_AGE_ADJ) %>%
  left_join(MSYT_reference, 
            by = c("FEATURE_ID_2022" = "FEATURE_ID"), suffix = c("_YSM", ""))

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
  arrange(FEATURE_ID_2022, SITE_IDENTIFIER, CLSTR_ID)

### MSYT projected volume
msyt_vol <- MSYT_current_output %>%
  filter(FEATURE_ID %in% unique(msyt$FEATURE_ID_2022)) %>%
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

msyt_vol3_1 <- msyt_vol2 %>% 
  expand(FEATURE_ID, AGE = full_seq(AGE, 1))

msyt_vol4 <- msyt_vol3_1 %>%
  left_join(msyt_vol2, by = c("FEATURE_ID", "AGE"))

msyt_vol5 <- msyt_vol4 %>% 
  group_by(FEATURE_ID) %>% 
  mutate(netvol = zoo::na.approx(NETVOL, AGE),
         netvol_con = zoo::na.approx(NETVOL_CON, AGE)) %>% 
  ungroup()

msyt1 <- msyt %>%
  left_join(msyt_vol5, by = c("FEATURE_ID_2022" = "FEATURE_ID", "ref_age_adj" = "AGE")) %>%
  distinct


### Third, process VDYP current volume
VDYP_input <- VDYP_input %>%
  ### Remove residual
  filter(VDYP7_LAYER_CD == "P") 

VDYP_all <- VDYP_all %>%
  left_join(VDYP_input %>% select(FEATURE_ID, LAYER_ID = LAYER_LEVEL_CODE), 
            by = c("FEATURE_ID", "LAYER_ID"))

VDYP_proj <- VDYP_all %>%
  filter(FEATURE_ID %in% unique(sample_data$FEATURE_ID_2022), PRJ_TOTAL_AGE >= 10, PRJ_TOTAL_AGE <= 100)

vdyp1 <- VDYP_proj %>%
  select(FEATURE_ID, LAYER_ID, MAP_ID, PROJECTION_YEAR, PRJ_TOTAL_AGE, 
         PRJ_DOM_HT, PRJ_BA, PRJ_TPH, PRJ_VOL_WS, PRJ_VOL_CU, PRJ_VOL_DWB) %>%
  mutate_at(vars(PRJ_DOM_HT, PRJ_BA, PRJ_TPH, PRJ_VOL_WS, PRJ_VOL_CU, PRJ_VOL_DWB), ~replace(., is.na(.), 0))

vdyp2_1 <- vdyp1 %>% 
  expand(nesting(FEATURE_ID, LAYER_ID), PRJ_TOTAL_AGE = full_seq(PRJ_TOTAL_AGE, 1))

vdyp3 <- vdyp2_1 %>%
  left_join(vdyp1, by = c("FEATURE_ID", "LAYER_ID", "PRJ_TOTAL_AGE")) 

vdyp4 <- vdyp3 %>% 
  group_by(FEATURE_ID, LAYER_ID) %>% 
  mutate(vdyp_vol_dwb = zoo::na.approx(PRJ_VOL_DWB, PRJ_TOTAL_AGE),
         vdyp_year = zoo::na.approx(PROJECTION_YEAR, PRJ_TOTAL_AGE)) %>% 
  ungroup()


### Finally, TASS volume
tass_output1 <- tass_output %>%
  filter(SPECIES == "ALL", RESID %in% c("N", NA), CLSTR_ID %in% unique(sample_data$CLSTR_ID))

### Filter outputs with 0 projection
tass_output2_1 <- tass_output1 %>%
  arrange(SITE_IDENTIFIER, VISIT_NUMBER, rust, Age, desc(TASS_ver), desc(xy)) %>%
  group_by(SITE_IDENTIFIER, VISIT_NUMBER, rust, TASS_ver, xy) %>%
  filter(sum(Stems) > 0)

### Need reference age to compute OAF adjusted volume
tass_output3 <- tass_output2_1 %>%
  group_by(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, rust, TASS_ver, xy) %>%
  mutate(start_age = min(Age)) %>%
  ungroup() %>%
  left_join(sample_data %>% select(CLSTR_ID, PROJ_AGE_ADJ), 
            by = c("CLSTR_ID")) %>%
  left_join(msyt %>% select(CLSTR_ID, ref_age_adj), 
            by = c("CLSTR_ID"))

# *oaf adjust the tass2 curves with defaults, since they were generated without oafs;
# *for OAF1, increment from zero at starting age, linear increaseing up to 15% by age 80yrs, then maintain at 15%;
# *for OAF2, apply as SOP increasing to 5% by age 100, and continuing to increase beyond;
# *Age = current modeled age;
# *start_age = age at start of projection, based on RESULTS / VEGCOMP age adjusted to ground sampling year;
# *WSV = whole stem volume projected in TASS w/o oafs;
# *GMV = gross merch volume projected in TASS w/o oafs.  This is the whole stem volume, less 30cm stump ht, less 10cm top dib,;
# *for all trees >= 12.5cm DBH for PL and >= 17.5cm DBH for all other species;
# *WSV_adj = oaf1 / oaf2 adjusted TASS whole stem volume;
# *GMV_adj = oaf1 / oaf2 adjusted TASS net merchantable volume;
tass_output4_1 <- tass_output3 %>%
  ungroup() %>%
  mutate(WSV_adj = ifelse((Age-start_age) < (80-start_age),
                          WSV*(1-OAF1*((Age-start_age)/(80 - start_age)))*(1-OAF2*(Age/100)),
                          WSV*(1-OAF1)*(1-OAF2*(Age/100))),
         GMV_adj = ifelse((Age-start_age) < (80-start_age),
                          GMV*(1-OAF1*((Age-start_age)/(80 - start_age)))*(1-OAF2*(Age/100)),
                          GMV*(1-OAF1)*(1-OAF2*(Age/100))),
         age_adj = Age - PROJ_AGE_ADJ + ref_age_adj)

# *prep for linear interpolation;
# *proj_age_1 may have changed between original tass projection and subsequent vegcomp version.  
# *so need to standardize starting projage1; to tass msyt, then extrapolate;
age1 <- tass_output4_1 %>%
  group_by(xy, rust, TASS_ver, SITE_IDENTIFIER) %>%
  filter(VISIT_NUMBER == min(VISIT_NUMBER)) %>%
  arrange(xy, rust, TASS_ver, SITE_IDENTIFIER, age_adj) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(age_dif = age_adj - ref_age_adj) %>%
  select(xy, rust, TASS_ver, SITE_IDENTIFIER, age_dif)

tass_output4_2 <- tass_output4_1 %>%
  left_join(age1, by = c('xy', 'rust', 'TASS_ver', 'SITE_IDENTIFIER')) %>%
  mutate(new_age = age_adj - age_dif)

# *get starting and ending age for linear expansion;
tass_output4_3 <- tass_output4_2 %>%
  arrange(xy, rust, SITE_IDENTIFIER, TASS_ver, VISIT_NUMBER) %>% # Ensuring correct order
  group_by(xy, rust, SITE_IDENTIFIER, TASS_ver, VISIT_NUMBER, CLSTR_ID) %>%
  mutate(
    min_age = if_else(row_number() == 1, if_else(is.na(new_age), 0, new_age), NA_real_),
    max_age = if_else(row_number() == n(), if_else(is.na(new_age), 200, new_age), NA_real_)
  ) %>%
  fill(min_age, max_age, .direction = "downup") %>% # Filling retained values
  filter(row_number() == n()) %>% # Keep only last row per group
  ungroup() %>%
  select(xy, rust, TASS_ver, SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID,
         PROJ_AGE_ADJ, ref_age_adj, age_dif, min_age, max_age)

tass_output4_4 <- tass_output4_3 %>%
  rowwise() %>%
  do(data.frame(xy = .$xy, 
                rust = .$rust, 
                TASS_ver = .$TASS_ver,
                SITE_IDENTIFIER = .$SITE_IDENTIFIER, 
                VISIT_NUMBER = .$VISIT_NUMBER, 
                CLSTR_ID = .$CLSTR_ID, 
                PROJ_AGE_ADJ = .$PROJ_AGE_ADJ, 
                ref_age_adj = .$ref_age_adj, 
                agespan = seq(.$min_age, .$max_age, by = 1)))

# *merge back with source;
tass_output4_5 <- tass_output4_4 %>%
  left_join(tass_output4_2 %>% select(-PROJ_AGE_ADJ, -ref_age_adj), 
            by = c("xy", "rust", "TASS_ver", "SITE_IDENTIFIER", "VISIT_NUMBER", "CLSTR_ID", "agespan" = "new_age")) %>%
  mutate(GMV_adj = ifelse(agespan == 0 & is.na(GMV_adj), 0, GMV_adj))

# *create annual records from starting age to max age;
tass_output4_6 <- tass_output4_5 %>% 
  group_by(xy, rust, TASS_ver, SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID) %>% 
  mutate(GMV_approx = zoo::na.approx(GMV_adj, agespan, rule = 2),
         Age_adj_approx = zoo::na.approx(age_adj, agespan, rule = 2),
         Year_approx = zoo::na.approx(Year, agespan, rule = 2),
         Age_approx = zoo::na.approx(Age, agespan, rule = 2)) %>% 
  fill(xy, rust, TASS_VER, TASS_ver, SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, .direction = 'up') %>%
  ungroup()

# *next, only keep one tass projections, as the basis to compute a modeled pai;
# *by sorting by visit number, only the first visit is kept;
# *by sorting in descending xy_sub, then stem map is picked over not stem mapped, and rust is picked over norust;
# *this is consistent with the final selected tass run to use;
tass_output6 <- tass_output4_6 %>%
  select(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, xy, rust, TASS_ver, PROJ_AGE_ADJ, ref_age_adj, agespan,
         GMV_approx, Age_adj_approx, Year_approx, Age_approx)

### Leave only one rust and xy per sample, with first visit
tass_output7 <- tass_output6 %>% 
  arrange(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, desc(xy), desc(rust), desc(TASS_ver)) %>%
  group_by(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, Age_adj_approx) %>%
  slice(1)

tass_output7_1 <- tass_output7 %>%
  group_by(SITE_IDENTIFIER) %>%
  filter(VISIT_NUMBER == min(VISIT_NUMBER)) %>%
  ungroup()


### Combine all data
#### YSM current volume
currentvol <- sample_data1 %>%
  select(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, FEATURE_ID, FEATURE_ID_2022, LAYER_ID,
         MEAS_YR, PROJ_AGE_ADJ, MGMT_UNIT, BEC = BEC_ZONE, BEC_SBZ, TSA_DESC) %>%
  left_join(ysm_trees3, by = c("SITE_IDENTIFIER", "CLSTR_ID", "VISIT_NUMBER")) 

### Merge YSM - MSYT
### 1. TIPSY: Join with sample data 
currentvol1 <- currentvol %>%
  left_join(msyt1 %>% select(-SITE_IDENTIFIER, -VISIT_NUMBER, -FEATURE_ID, -MEAS_YR, -MGMT_UNIT, -PROJ_AGE_ADJ), 
            by = c("CLSTR_ID", "FEATURE_ID_2022" = "FEATURE_ID_2022")) 

### 2. VDYP: Merge YSM - VDYP
currentvol2 <- currentvol1 %>%
  mutate(LAYER_ID_c = as.character(LAYER_ID)) %>%
  left_join(vdyp4 %>% select(FEATURE_ID, LAYER_ID, PRJ_TOTAL_AGE, vdyp_vol_dwb, vdyp_year) %>% distinct(), 
            by = c("FEATURE_ID_2022" = "FEATURE_ID", "ref_age_adj" = "PRJ_TOTAL_AGE",
                   "LAYER_ID_c"  = "LAYER_ID"),
            suffix = c("_vegcomp", ""))

### Merge YSM - TASS
currentvol3 <- currentvol2 %>%
  left_join(tass_output7_1 %>% select(-CLSTR_ID, -VISIT_NUMBER, -PROJ_AGE_ADJ, -ref_age_adj),
            by = c("SITE_IDENTIFIER", "ref_age_adj" = "agespan")) 

### Final variable check
currentvol4 <- currentvol3 %>%
  ungroup() %>%
  mutate(
    vol_wsv_ha = ifelse(is.na(vol_wsv_ha), 0, vol_mer_ha),
    vol_wsv_ha_nores = ifelse(is.na(vol_wsv_ha_nores), 0, vol_mer_ha),
    vol_mer_ha = ifelse(is.na(vol_mer_ha), 0, vol_mer_ha),
    vol_mer_ha_nores = ifelse(is.na(vol_mer_ha_nores), 0, vol_mer_ha),
    vol_ntwb_ha = ifelse(is.na(vol_ntwb_ha), 0, vol_ntwb_ha),
    vol_ntwb_ha_nores = ifelse(is.na(vol_ntwb_ha_nores), 0, vol_ntwb_ha_nores),
    yt_source = ifelse(grepl('Managed:',CURRENT_YIELD), "Managed", 
                       ifelse(grepl('AGGREGATE', toupper(CURRENT_YIELD)), "AGGREGATE", CURRENT_YIELD)),
    ### If not available in MSYT but is in VDYP runs
    yt_source = ifelse(is.na(yt_source) & !is.na(vdyp_vol_dwb), "VDYP-fill_missed_tsr", yt_source),
    yt_source_f = factor(yt_source, 
                         levels = c("Managed", "AGGREGATE", "VDYP",  "VDYP-fill_missed_tsr", "Excluded"), 
                         ordered = T),
    volTSR = ifelse(yt_source %in% c("Managed","AGGREGATE"), netvol,   
                    ifelse(yt_source %in% c("VDYP", "VDYP-fill_missed_tsr"), vdyp_vol_dwb, NA)),
    grdnv = ifelse(is.na(vol_ntwb_ha_nores), 0, vol_ntwb_ha_nores),
    prednv = ifelse(is.na(volTSR), 0, volTSR),
    tassnv = ifelse(is.na(GMV_approx), 0, GMV_approx),
    voldiffTSR = prednv - grdnv,
    voldiffTASS = tassnv - grdnv)  

ysm_msyt_vdyp <- currentvol4 %>%
  select(SITE_IDENTIFIER, CLSTR_ID, VISIT_NUMBER, FEATURE_ID_2022,
         OPENING_ID, TASS_ver, xy, 
         MGMT_UNIT, TSA_DESC, BEC, BEC_SBZ, 
         MEAS_YR, PROJ_AGE_ADJ, ref_age_adj, ref_age_cd, PRJ_TOTAL_AGE = PROJ_AGE_ADJ, 
         vol_wsv_ha, vol_mer_ha, vol_ntwb_ha,
         vol_wsv_ha_nores, vol_mer_ha_nores,
         vol_ntwb_ha_nores, vdyp_vol_dwb, netvol, volTSR, GMV_approx,
         CURRENT_YIELD, yt_source, yt_source_f, 
         grdnv, prednv, tassnv, voldiffTSR, voldiffTASS)


### Save data for application
saveRDS(ysm_msyt_vdyp, paste0(savepath,"ysm_msyt_vdyp_volume.rds"))
saveRDS(msyt_vol2, paste0(savepath,"msyt_volproj.rds"))



################################################################################
### Create projected volume data 
###  - Input: sample_data, tass_output, msyt, msyt_volproj, MSYT_reference, 
###           vdyp2, msyt, MSYT_reference
###  - Output: tsr_tass_volproj, ysm_tass_proj
################################################################################

### TSR projected volume
tsr_volproj <- sample_data1 %>% 
  expand(nesting(CLSTR_ID, LAYER_ID), AGE = full_seq(c(30, 100), 10))

tsr_volproj1 <- tsr_volproj %>%
  left_join(sample_data1 %>% select(FEATURE_ID_2022, SITE_IDENTIFIER, CLSTR_ID, LAYER_ID, MEAS_YR, 
                                   MGMT_UNIT, PROJ_AGE_ADJ, TSA_DESC,
                                   BEC_ZONE_YSM = BEC_ZONE, BEC_SBZ_YSM = BEC_SBZ, BEClabel) %>% 
              distinct(), 
            by = c("CLSTR_ID", "LAYER_ID"))

tsr_volproj2 <- tsr_volproj1 %>%
  left_join(msyt_volproj, by = c("FEATURE_ID_2022" = "FEATURE_ID", "AGE"))

### Merge TASS - TSR projection
tsr_volproj3 <- tsr_volproj2 %>%
  mutate(LAYER_ID_c = as.character(LAYER_ID)) %>%
  left_join(vdyp1, 
            by = c("FEATURE_ID_2022" = "FEATURE_ID", "AGE" = "PRJ_TOTAL_AGE", "LAYER_ID_c" = "LAYER_ID"))

tsr_volproj3 <- tsr_volproj3 %>%
  filter(AGE %in% c(30, 40, 50, 60, 70, 80, 90, 100))

tsr_volproj4 <- tsr_volproj3 %>%
  left_join(MSYT_reference, by = c("FEATURE_ID_2022" = "FEATURE_ID"))

tsr_volproj4 <- tsr_volproj4 %>%
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
         yt_source = ifelse(grepl('Managed:',CURRENT_YIELD), "Managed", 
                            ifelse(grepl('AGGREGATE', toupper(CURRENT_YIELD)), "AGGREGATE", CURRENT_YIELD)),
         ### If not available in MSYT but is in VDYP runs
         yt_source = ifelse(is.na(yt_source) & !is.na(PRJ_VOL_DWB), "VDYP-fill_missed_tsr", yt_source),
         yt_source_f = factor(yt_source, levels = c("Managed", "AGGREGATE", "VDYP",  "VDYP-fill_missed_tsr", "Excluded"), ordered = T),
         
         volTSR = ifelse(yt_source %in% c("Managed","AGGREGATE"), NETVOL,       
                         ifelse(yt_source  %in% c("VDYP", "VDYP-fill_missed_tsr"), PRJ_VOL_DWB, NA)),
         volTSR = ifelse(is.na(volTSR), 0, volTSR))

### Join by adjusted age
tsr_volproj5_1 <- tsr_volproj4 %>%
  left_join(tass_output6 %>%  
              filter(agespan %in% c(30, 40, 50, 60, 70, 80, 90, 100)) %>%
              select(-PROJ_AGE_ADJ, -ref_age_adj),
            by = c("SITE_IDENTIFIER", "CLSTR_ID", "AGE" = "agespan")) %>%
  mutate(volTASS = ifelse(is.na(GMV_approx) & is.na(TASS_ver), 0, GMV_approx))


tsr_tass_volproj <- tsr_volproj5_1 %>%
  select(FEATURE_ID_2022, SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, MEAS_YR, PROJ_AGE_ADJ, 
         BEC_ZONE = BEC_ZONE_YSM, BEC_SBZ_YSM, MGMT_UNIT, 
         AGE, LAYER_ID, PROJECTION_YEAR, PRJ_BA, PRJ_TPH, PRJ_VOL_WS, PRJ_VOL_DWB, 
         OPENING_ID, RSLT_AGE, RSLT_REFERENCE_YEAR, VRI_AGE, CATEGORY, RSLT_AGE, RSLT_REFERENCE_YEAR, NETVOL,
         results_age, results_age_adj, ref_age_adj, ref_age_cd, CURRENT_YIELD, yt_source, yt_source_f, volTSR,
         xy, rust, TASS_ver, Year_approx, Age_approx, GMV_approx, volTASS) %>%
  distinct()


### Save data for application
### TASS projection 
saveRDS(tass_output6, paste0(savepath,"ysm_tass_proj.rds"))
### TASS - MSYT projection 
saveRDS(tsr_tass_volproj, paste0(savepath,"tsr_tass_volproj.rds"))


