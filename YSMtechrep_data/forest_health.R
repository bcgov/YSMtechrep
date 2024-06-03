library(data.table)
library(dplyr)
library(tidyverse)
library(openxlsx)

# Import sample and tree data
sample_data <- fread("//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/test/data2/sample_data_comp20240213.csv") 
tree_data <- fread("//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/test/data2/tree_data_comp20240213.csv") 

# Import ismc damage agent to severity class lookup table;
lookup_sev <-readRDS("//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/Compilation/ismc/Archive_nonPSP_20240213/compilation_nonPSP_raw/ISMC_PROD_20240213_11am_TreeDamageOccurrences.rds")

# Import severity rating lookup table created by D.Rusch;
lookup_rush <- read.xlsx("//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/ForestHealth/CopiedfromRdejong/forest_health/severity_rating_lookup_table/Severity_lookup_table_2021mar15.xlsx",
                          sheet = 'input1')

# Import corrections to severity rating (unknown to correct severity ratings created by D.Rusch 2021mar10;
sev_rusch <- read.xlsx("//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/ForestHealth/CopiedfromRdejong/forest_health/severity_rating_lookup_table/Unknown_severity_2021mar09.xlsx")

# Set variable
decidspc <- c('A','AC','ACT','ACB','AT',
              'DM','DR','E','EA','EP','EW',
              'MB','MV','KC','RA','V','VB','VP','VV',
              'W','WB','WP','WT','ZH','XH','XC')


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

update_dam_loss <- function(dam_agnt, loss) {
  case_when(
    grepl('U', dam_agnt, fixed = TRUE) & loss %in% c('FRK') ~ 'UF',
    grepl('U', dam_agnt, fixed = TRUE) & loss %in% c('CRO', 'CRK') ~ 'UCR',
    grepl('U', dam_agnt, fixed = TRUE) & loss %in% c('BTP') ~ 'UBT',
    grepl('U', dam_agnt, fixed = TRUE) & loss %in% c('DTP') ~ 'UDT',
    TRUE ~ dam_agnt)
}

FH_dat <- FH_dat %>%
  mutate(
    DAM_AGNA = update_dam_loss(DAM_AGNA, LOSS1_IN),
    DAM_AGNB = ifelse(DAM_AGNA == 'U' & !(LOSS2_IN %in% c(NA, '')) & DAM_AGNB %in% c(NA, ''), 
                      update_dam_loss(DAM_AGNA, LOSS2_IN), DAM_AGNB),
    DAM_AGNC = ifelse(DAM_AGNA == 'U' & (LOSS3_IN %in% c(NA, '')) & DAM_AGNC %in% c(NA, ''), 
                      update_dam_loss(DAM_AGNA, LOSS3_IN), DAM_AGNC),
    DAM_AGND = ifelse(DAM_AGNA == 'U' & (LOSS4_IN %in% c(NA, '')) & DAM_AGND %in% c(NA, ''), 
                      update_dam_loss(DAM_AGNA, LOSS4_IN), DAM_AGND),
    DAM_AGNE = ifelse(DAM_AGNA == 'U' & (LOSS5_IN %in% c(NA, '')) & DAM_AGNE %in% c(NA, ''), 
                      update_dam_loss(DAM_AGNA, LOSS5_IN), DAM_AGNE)
  )

# *ignore minor incidence of UF and UCR (ie,. where severity=N, as recorded in 2021 and later measurements);
FH_dat <- FH_dat %>%
  mutate(DAM_AGNA = ifelse(DAM_AGNA %in% c('UF', 'UCR') & SEV_A == "N", "O", DAM_AGNA),
         DAM_AGNB = ifelse(DAM_AGNB %in% c('UF', 'UCR') & SEV_B == "N", "O", DAM_AGNB),
         DAM_AGNC = ifelse(DAM_AGNC %in% c('UF', 'UCR') & SEV_C == "N", "O", DAM_AGNC),
         DAM_AGND = ifelse(DAM_AGND %in% c('UF', 'UCR') & SEV_D == "N", "O", DAM_AGND),
         DAM_AGNE = ifelse(DAM_AGNE %in% c('UF', 'UCR') & SEV_E == "N", "O", DAM_AGNE))

#### If the DAM_AGNA is U and there are other loss indicator exists
#FH_dat <- FH_dat %>%
#  mutate(DAM_AGNA = ifelse(DAM_AGNA == 'U' & !is.na(DAM_AGNB) & grepl('U', DAM_AGNB), DAM_AGNB, DAM_AGNA))
#
#FH_dat <- FH_dat %>%
#  mutate(DAM_AGNB = ifelse(DAM_AGNA == DAM_AGNB & grepl('U', DAM_AGNA, fixed = TRUE), 'U', DAM_AGNB))

FH_dat <- FH_dat %>%
  mutate(SEVPERC_A = as.numeric(gsub("[^\\d]+", "", SEV_A, perl = T)),
         SEVPERC_B = as.numeric(gsub("[^\\d]+", "", SEV_B, perl = T)),
         SEVPERC_C = as.numeric(gsub("[^\\d]+", "", SEV_C, perl = T)),
         SEVPERC_D = as.numeric(gsub("[^\\d]+", "", SEV_D, perl = T)),
         SEVPERC_E = as.numeric(gsub("[^\\d]+", "", SEV_E, perl = T))) %>%
  data.frame

update_undefined_dam <- function(dam_agna, dam_agnt) {
  case_when(
    grepl('U', dam_agna, fixed = TRUE) & dam_agnt %in% c(NA, '','UF','UCR','UBT','UDT','U') ~ '',
    # *all other damage agents get output regardless of position;
    TRUE ~ dam_agnt)
}

FH_dat <- FH_dat %>%
  mutate(DAM_AGNB = update_undefined_dam(DAM_AGNA, DAM_AGNB),
         DAM_AGNC = update_undefined_dam(DAM_AGNA, DAM_AGNC),
         DAM_AGND = update_undefined_dam(DAM_AGNA, DAM_AGND),
         DAM_AGNE = update_undefined_dam(DAM_AGNA, DAM_AGNE),
         DAM_AGNC = update_undefined_dam(DAM_AGNB, DAM_AGNC),
         DAM_AGND = update_undefined_dam(DAM_AGNB, DAM_AGND),
         DAM_AGNE = update_undefined_dam(DAM_AGNB, DAM_AGNE),
         DAM_AGND = update_undefined_dam(DAM_AGNC, DAM_AGND),
         DAM_AGNE = update_undefined_dam(DAM_AGNC, DAM_AGNE),
         DAM_AGNE = update_undefined_dam(DAM_AGND, DAM_AGNE))

FH_dat1 <- melt(setDT(FH_dat),
                id.vars = c('SITE_IDENTIFIER', 'CLSTR_ID', 'VISIT_NUMBER', 'PLOT', 'MEAS_YR', 'MEAS_INTENSE',
                            'TREE_NO', 'RESIDUAL', 'TH_TREE', 'DBH', 'HEIGHT', 'SPECIES', 
                            'LV_D', 'S_F', 'COMP_CHG', 'PHF_TREE', 
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
    AGN %in% c('DBS') & SEV %in% c('BC', 'SC') ~ 'DSB',
    AGN %in% c('IAG') & SPC_GRP1 %in% c('DE') ~ '',
    AGN %in% c('DBS') & SPC_GRP1 %in% c('PW') ~ '',
    AGN %in% c('DFE') & SPC_GRP1 %in% c('FD') ~ '',
    AGN %in% c('DFL') & SPC_GRP1 %in% c('BL','FD') ~ '',
    AGN %in% c('DSB') & SPC_GRP1 %in% c('PL') ~ '',
    AGN %in% c('DSG') & SPC_GRP1 %in% c('BL') ~ '',
    AGN %in% c('IBM') & SPC_GRP1 %in% c('BL','SW') ~ '',
    AGN %in% c('IBS') & SPC_GRP1 %in% c('PL') ~ '',
    AGN %in% c('IDE') & SPC_GRP1 %in% c('PL') ~ '',
    AGN %in% c('DFB') & SPC_GRP1 %in% c('FD','SW') ~ '',
    AGN %in% c('DM') & SPC_GRP1 %in% c('BL') ~ 'DBF',
    AGN %in% c('DM') & SPC_GRP1 %in% c('PL') ~ 'DMP',
    AGN %in% c('ISP') & SPC_GRP1 %in% c('BL') ~ '',
    TRUE ~ AGN))

FH_dat1 <- FH_dat1 %>%
  mutate(AGN = ifelse(DAM_NUM == 1 & AGN == "", "O", AGN))

### If a tree has multiple records of same damage agent with different severity, leave the most severity one only
FH_dat1_1 <- FH_dat1 %>%
  filter(!is.na(AGN), AGN != '') %>%
  group_by(CLSTR_ID, PLOT, TREE_NO, AGN) %>%
  mutate(dups = ifelse(n()>1, "Yes", "No"),
         selected = ifelse(SEVPERC == max(SEVPERC), "Yes", "No"),
         selected = ifelse(is.na(selected), "Yes", selected))

### If duplicated damage agent for a same tree has no severity information, leave only one
FH_dat1_1_1 <- FH_dat1_1 %>%
  group_by(CLSTR_ID, PLOT, TREE_NO, AGN) %>%
  mutate(SEVPERC_new = ifelse(DAM_NUM == 1 & selected == "No", max(SEVPERC), SEVPERC),
         selected = ifelse(SEVPERC_new == max(SEVPERC_new), "Yes", "No"),
         selected = ifelse(is.na(selected), "Yes", selected)) %>%
  filter(selected == "Yes") %>%
  arrange(CLSTR_ID, PLOT, TREE_NO, AGN, DAM_NUM, desc(dups), desc(selected)) %>%
  group_by(CLSTR_ID, PLOT, TREE_NO, AGN, dups, selected) %>%
  slice(1)

# *start categorizing severity;
lookup_sev1 <- lookup_sev %>%
  mutate(dam_3letter = DAMAGE_AGENT_CODE,
         severity_grp = DAMAGE_AGENT_SEVERITY_CODE) %>%
  select(dam_3letter, severity_grp) %>%
  distinct()

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
         sev_class_num = ifelse(grepl("[A-Za-z]", dam_severity_adj, perl = T) == F, 
                                as.numeric(dam_severity_adj), NA))

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
                                   ifelse(sev_class %in% c("UNKN", NA) & dam_severity_adj %in% c(High_class1, High_class2), 'HIGH', sev_class)
                            )))

FH_dat1_2 <- FH_dat1_2 %>%
  mutate(sev_class = ifelse(sev_class %in% c("", NA), "UNKN", sev_class))

FH_dat2 <- FH_dat1_2 %>%
  ungroup() %>%
  filter(!is.na(AGN), AGN != '') %>%
  mutate(n = n_distinct(SITE_IDENTIFIER),
         SPC_GRP1 = substr(SPECIES,1,2),
         SPC_GRP1 = ifelse(SPECIES %in% decidspc, 'DE', SPC_GRP1))

### Immediate vs. incremental mortality
FH_dat2 <- FH_dat2 %>%
  mutate(mort_code = case_when(AGN %in% c('AB','DRA','DRB','DRC','DRL','DRN','DRR','DRT','DSB', 
                                          'IB', 'IBB', 'IBI', 'IBM', 'IBP', 'IBS', 
                                          'IBW', 'ISW', 'ND', 'NF', 'NS', 'NW', 'NWS', 
                                          'NY', 'TC','DSC') ~ 1,
                               AGN == 'NB' & !(SPC_GRP1 %in% c('FD','LW')) ~ 1,
                               AGN %in% c('DF','DFE','DFS') & SEVPERC >= 80 ~ 1,
                               AGN %in% c('DB', 'DM', 'DMH', 'DMP', 'DSA', 'DSE') ~ 2,
                               AGN %in% c('DSG','DSS') & MEAS_YR < 2017 ~ 2,
                               AGN %in% c('IAB', 'IDW', 'IDB', 'IDE', 'IDH', 'IDI', 'IDT') &
                                 SEVPERC >= 80 ~ 2,
                               AGN %in% c('IWP', 'IWS') & grepl('N', SEV) == FALSE ~ 2,
                               TRUE ~ 3))

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
         TH_TREE, DBH, HEIGHT, SPECIES, SPC_GRP1, LV_D, S_F, COMP_CHG, 
         PHF_TREE, VOL_WSV, VOL_MER, VOL_NTWB, AGE_BH, AGE_TOT,
         SI_TREE, SUIT_TR, SUIT_HT, SUIT_SI,
         BA_TREE, ba_ha, vol_ha, DAM_NUM, AGN, SEV, SEVPERC, 
         dups, selected, severity_grp, dam, dam_severity_adj, 
         sev_class, sev_class_num, n, mort_code, mort_flag, AGN_new,
         RESID_GRP, DBH_CLASS, dam_1letter, dam_2letter, dam_3letter,
         dam_class, corr_sev, dam_severity)


write.csv(Tree_FH_data, "//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/YSMreport/test/data2/Tree_FH_data.csv")

