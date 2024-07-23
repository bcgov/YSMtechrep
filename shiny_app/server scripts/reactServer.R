## Server logic for reactive data ----

###############################################.
## Indicator definitions ----
###############################################.
# Define subsetting feature by domain 
title <- reactive({
  req(input$SelectCategory, input$SelectVar)
  title <- ifelse(input$SelectCategory == "TSA_DESC",
                  as.character(input$SelectVar),
                  paste0(input$SelectVar, " zone"))
  return(title)
})

site_id <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  site_id <- sample_data %>% 
    filter(TFL == "") %>% 
    filter(!!sym(input$SelectCategory) %in% input$SelectVar) %>%
    pull(SITE_IDENTIFIER)
  
  return(site_id)
  
})

clstr_id <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  clstr_id <- sample_data %>% 
    filter(TFL == "") %>% 
    filter(!!sym(input$SelectCategory) %in% input$SelectVar) %>%
    filter(LAST_MSMT == "Y") %>%
    pull(CLSTR_ID)
  
  return(clstr_id)
  
})

clstr_id_all <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  clstr_id_all <- sample_data %>% 
    filter(TFL == "") %>% 
    filter(!!sym(input$SelectCategory) %in% input$SelectVar) %>%
    pull(CLSTR_ID)
  
  return(clstr_id_all)
  
})


clstr_id_last2 <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  clstr_id_last2 <- sample_data %>% 
    filter(TFL == "") %>% 
    filter(!!sym(input$SelectCategory) %in% input$SelectVar) %>%
    group_by(SITE_IDENTIFIER) %>%
    filter(n() > 1) %>% 
    arrange(VISIT_NUMBER) %>% 
    slice_tail(n = 2) %>%
    pull(CLSTR_ID)
  
  return(clstr_id_last2)
  
})


# Subset data using the features

summary_data <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  summary_data<-spcs_data %>% 
    filter(CLSTR_ID %in% clstr_id(), UTIL == 4) %>%
    mutate(SPC_GRP1 = substr(SPECIES,1,2)) %>%
    mutate(SPC_GRP1 = ifelse(SPECIES %in% decidspc, 'DE', SPC_GRP1))%>%  
    mutate(BA_HA_L = BA_HA_LS + BA_HA_LF,
           BA_HA_D = BA_HA_DS + BA_HA_DF,
           STEMS_HA_L = STEMS_HA_LS + STEMS_HA_LF,
           STEMS_HA_D = STEMS_HA_DS + STEMS_HA_DF,
           VHA_WSV_L = VHA_WSV_LS + VHA_WSV_LF,
           VHA_WSV_D = VHA_WSV_DS + VHA_WSV_DF,
           n = length(unique(SITE_IDENTIFIER))) %>% 
    select(SITE_IDENTIFIER, CLSTR_ID, SPECIES, SPC_GRP1, 
           BA_HA_L, BA_HA_D, STEMS_HA_L, STEMS_HA_D, 
           VHA_WSV_L, VHA_WSV_D, n, 
           QMD_LS, QMD_DS)
  
  summary_mer<-spcs_data %>% 
    filter(CLSTR_ID %in% clstr_id(), UTIL == ifelse(SPECIES =="PL", 12.5, 17.5)) %>% 
    select(SITE_IDENTIFIER, CLSTR_ID, UTIL, SPECIES, 
           VHA_MER_LS, VHA_MER_LF, VHA_MER_DS, VHA_MER_DF) %>% 
    mutate(VHA_MER_L = VHA_MER_LS + VHA_MER_LF,
           VHA_MER_D = VHA_MER_DS + VHA_MER_DF) 
  
  summary_data <- summary_data %>%
    left_join(summary_mer, by = c('SITE_IDENTIFIER', 'CLSTR_ID', "SPECIES"))
  
  summary_data <- summary_data %>%
    left_join(SI_data, by = c('SITE_IDENTIFIER', 'CLSTR_ID', "SPECIES"))
  
  return(summary_data)
  
})


decid_vol <- reactive({
  
  summary_data <- summary_data()
  
  decid_vol <- summary_data %>%
    mutate(spc_dec_con = ifelse(SPC_GRP1 == "DE", "decid", "con")) %>%
    group_by(spc_dec_con) %>%
    summarize(VHA_WSV_L = sum(VHA_WSV_L, na.rm = T)) %>%
    ungroup() %>%
    mutate(vol = round(VHA_WSV_L/sum(VHA_WSV_L, na.rm = T)*100,0)) %>%
    filter(spc_dec_con == "decid") %>%
    pull(vol)
  
  return(decid_vol)
})



LD_dat <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  LD_dat <- spcs_data %>%
    filter(CLSTR_ID %in% clstr_id(), UTIL == 4) %>%
    group_by(SITE_IDENTIFIER) %>%
    arrange(desc(SP_PCT_BA_LS)) %>%
    slice_head(n = 1)%>%
    select(SITE_IDENTIFIER, CLSTR_ID, SPECIES, SP_PCT_BA_LS)
  
  LD_dat <- LD_dat %>%
    left_join(vegcomp_pspl_sample %>% select(CLSTR_ID, SPECIES_CD_1, SPECIES_PCT_1), 
              by = "CLSTR_ID") %>%
    mutate(SPC_GRP1 = substr(SPECIES,1,2),
           SPC_GRP1 = ifelse(SPECIES %in% decidspc, 'Decid', SPC_GRP1),
           SPC_GRP1 = ifelse(SPC_GRP1 =="", 'Nonstock', SPC_GRP1),
           SPC_GRP_VRI = substr(SPECIES_CD_1,1,2),
           SPC_GRP_VRI = ifelse(SPECIES_CD_1 %in% decidspc, 'Decid', SPC_GRP_VRI)) 
  
  return(LD_dat)
  
})


correct_ls <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  LD_dat <- LD_dat()
  
  correct <- round(sum(LD_dat$SPC_GRP1 == LD_dat$SPC_GRP_VRI)/nrow(LD_dat)*100, 0)
  
  return(correct)
  
})


Fig11_dat <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  ysm_spc <- spcs_data %>% 
    filter(CLSTR_ID %in% clstr_id(), UTIL == 4, !(SPECIES %in% c('', NA)))
  
  ### Add species percent information to sample_data
  ysm_spc1 <- sample_data %>%
    filter(CLSTR_ID %in% clstr_id()) %>%
    left_join(ysm_spc, by = c("CLSTR_ID", "SITE_IDENTIFIER", "VISIT_NUMBER" )) %>%
    filter(!is.na(SITE_IDENTIFIER))
  
  ### Compute number plot measurements by MGMT_UNIT
  ysm_spc2 <- ysm_spc1 %>%
    mutate(n_ci = length(unique(CLSTR_ID)),
           SPC_GRP1 = ifelse(SPECIES %in% decidspc, 'DE', SPECIES))
  
  ### Compute mean stems per ha by species
  ysm_spc3 <- ysm_spc2 %>%
    group_by(SPC_GRP1) %>%
    reframe(mean_STEMS_HA = sum(STEMS_HA_LS, na.rm = T)/n_ci) %>%
    distinct()
  
  ysm_spc4 <- ysm_spc3 %>%
    mutate(tot_STEMS_HA = sum(mean_STEMS_HA, na.rm = T),
           spcperc = mean_STEMS_HA/tot_STEMS_HA)
  
  ysm_spc6 <- ysm_spc4 %>%
    arrange(desc(spcperc)) %>%
    mutate(order = row_number(),
           SPC_GRP2 = ifelse(order <= 7, SPC_GRP1, 'Other'),
           SPC_GRP2 = ifelse(SPC_GRP2 == 'DE', 'Decid', SPC_GRP2))
  
  regen_natural <- regen_data %>%
    filter(CLSTR_ID %in% clstr_id(), regen_src == "N", SPECIES_recode != "") %>%
    mutate(n_si = length(unique(CLSTR_ID)),
           n_ft = length(unique(FEATURE_ID)))
  
  regen_natural <- regen_natural %>%
    group_by(SPC_GRP1) %>%
    reframe(mean_DENSITY = sum(DENSITY, na.rm = T)/n_ft) %>%
    distinct()
  
  regen_natural <- regen_natural %>%
    mutate(tot_DENSITY = sum(mean_DENSITY, na.rm = T),
           spcperc = mean_DENSITY/tot_DENSITY) %>%
    arrange(desc(spcperc)) %>%
    mutate(order = row_number(),
           SPC_GRP2 = ifelse(order <= 7, SPC_GRP1, 'Other'),
           SPC_GRP2 = ifelse(SPC_GRP2 == 'DE', 'Decid', SPC_GRP2))
  
  ### Data for figure
  ysm_spc_dat <- ysm_spc6 %>% 
    mutate(source="YSM") %>%
    select(SPC_GRP1, spcperc, order, SPC_GRP2, source)
  
  regen_natural <- regen_natural %>%
    mutate(source="TSR_INPUT") %>%
    select(SPC_GRP1, spcperc, order, SPC_GRP2, source)
  
  Fig11_dat <- rbind(ysm_spc_dat, regen_natural) 
  
  return(Fig11_dat)
  
})

percoverlap <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  Fig11_dat <- Fig11_dat()
  
  percoverlap <- Fig11_dat %>%
    #filter(MGMT_UNIT %in% mgmt_unit) %>%
    select(-order) %>%
    pivot_wider(names_from = source,
                names_sep = ".",
                values_from = c(spcperc)) %>%
    mutate(diff = abs(YSM - TSR_INPUT)) %>%
    summarize(perc_sum = sum(diff, na.rm = T)) %>%
    mutate(perc_over = 1- perc_sum) %>%
    pull(perc_over)
  
  return(percoverlap)
  
})


fig6_dat <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  fig6_dat <- tree_fh_data %>%
    filter(CLSTR_ID %in% clstr_id(), DAM_NUM==1, LV_D == "L") %>%
    mutate(VOL_WSV_HA = VOL_WSV*PHF_TREE,
           PERC_TOT_VOL_HA = VOL_WSV_HA/sum(VOL_WSV_HA, na.rm = T),
           DBH_CLASS = round(DBH/5)*5) 
  
  fig6_dat <- fig6_dat %>%
    mutate(DBH_CLASS_relevel = cut(DBH_CLASS, breaks = c(seq(-1, 59, 5), Inf), 
                                   labels = c(seq(0, 55, 5), "60+")),
           RESIDUAL_relevel = fct_recode(factor(RESIDUAL), "Managed"="N", "Residual"="Y"))
  
  return(fig6_dat)
  
})


fig6_max <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  fig6_dat <- fig6_dat()
  
  fig6_max <- fig6_dat %>%
    group_by(DBH_CLASS_relevel) %>%
    summarise(sum = sum(PERC_TOT_VOL_HA, na.rm = T)) %>%
    ungroup() %>%
    summarise(ymax = max(sum, na.rm = T)) %>%
    pull(ymax)
  
  return(fig6_max)
  
})


fig6_sum <- reactive({
  
  fig6_dat <- fig6_dat()
  
  fig6_sum <- fig6_dat %>%
    filter(RESIDUAL == "Y") %>%
    summarise(tot = sum(PERC_TOT_VOL_HA, na.rm = T)*100) %>%
    pull(tot)
  
  return(fig6_sum)
  
})



si_dat <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  si_dat <- SI_data %>%
    filter(CLSTR_ID %in% clstr_id_all(), !is.na(ratio)) %>%
    group_by(SITE_IDENTIFIER, SPECIES) %>%
    slice(which.min(yrs_from_50))
  
  si_dat1 <- si_dat %>%
    group_by(SPECIES) %>%
    summarise(n = n(),
              age_avg = mean(AGEB_TLSO),
              grd_avg = mean(SI_M_TLSO),
              prd_avg = mean(pspl_si))
  
  si_dat1 <- si_dat1 %>%
    ungroup() %>%
    mutate(ROM = grd_avg/prd_avg)
  
  si_dat2 <- si_dat %>%
    left_join(si_dat1, , by = "SPECIES")
  
  # *compute additional attribute to compute variance;
  si_dat2 <- si_dat2 %>%
    mutate(diffs_sqrd = (SI_M_TLSO - (ROM * pspl_si))**2)
  
  # *sum diffs squared;
  si_dat3 <- si_dat2 %>%
    group_by(SPECIES) %>%
    summarise(sum_diffs_sqrd = sum(diffs_sqrd))
  
  # *merge differences squared back to sample by species detail;
  si_dat4 <- si_dat2 %>%
    left_join(si_dat3, by = "SPECIES")
  
  # *compute variance and confidence interval, and test for significiant ratio different from 1.0;
  # *arbitrary set of miniumum number of obs per species to be 10;
  si_dat4 <- si_dat4 %>%
    rowwise() %>%
    mutate(var_rom = ifelse(n >=2, sum_diffs_sqrd / (n * (n-1) * prd_avg**2), NA),
           l95_rom  = ifelse(n >=2, ROM - qt(0.975,n-1) * sqrt(var_rom), NA),
           u95_rom  = ifelse(n >=2, ROM + qt(0.975,n-1) * sqrt(var_rom), NA),
           # *standard signficance test;
           sig_rom = ifelse(!is.na(l95_rom) & l95_rom < 1.0 & !is.na(u95_rom) & u95_rom > 1.0,
                            "N", "Y"),
           # *following review by p.ott 2021mar, recommend to use ROPE TO DEFINE ZONE OF PRACTICAL SIGNIFICANCE;
           # *suggested to use 0.95 to 1.05 range of ratio to establish zone, rationale is that 5% rom is a 1m SI difference from a known;
           # *SI of 20m, and results in a 10% change in culmination of MAI for a PL MSYT using TIPSY 4.4;
           
           # *95% confidence interval completely outside rope;
           sig_rope = case_when((!is.na(l95_rom) & l95_rom > 1.05) | (!is.na(u95_rom) & u95_rom < 0.95) ~ "Y",
                                # *95% confidence interval completely inside rope;
                                (!is.na(l95_rom) & l95_rom > 0.95) & (!is.na(u95_rom) & u95_rom < 1.05) ~ "N",
                                # *one 95% confidence limit inside rope, but the other outside rope, then inconclusive;
                                (!is.na(l95_rom) & l95_rom < 1.05) & (!is.na(u95_rom) & u95_rom > 1.05) ~ "I",
                                (!is.na(l95_rom) & l95_rom < 0.95) & (!is.na(u95_rom) & u95_rom > 0.95) ~ "I",
                                # *both 95% confidence limits outside rope, then inconclusive;
                                (!is.na(l95_rom) & l95_rom < 0.95) & (!is.na(u95_rom) & u95_rom > 1.05) ~ "I"
           )
    )
  
  si_dat5 <- si_dat4 %>%
    ungroup() %>%
    select(SPECIES, n, age_avg, grd_avg, prd_avg, ROM, 
           l95_rom, u95_rom, sig_rope) %>%
    distinct() %>%
    mutate(age_avg = round(age_avg, 0),
           grd_avg = round(grd_avg, 1),
           prd_avg = round(prd_avg, 1),
           ROM = round(ROM, 2),
           l95_rom = round(l95_rom, 2),
           u95_rom = round(u95_rom, 2),
           l95_rom = ifelse(n >= 10, l95_rom, "-"),
           u95_rom = ifelse(n >= 10, u95_rom, "-"),
           sig_rope = ifelse(n >= 10, sig_rope, "-")) %>%
    arrange(SPECIES)
  
  return(si_dat5)
  
})


si_bias <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  si_dat <- si_dat()
  
  si_bias <- if (sum(si_dat$sig_rope == "Y") > 0){
    
    si_dat %>%
      filter(sig_rope == "Y") %>%
      mutate(bias = paste0(SPECIES, "(",  round((grd_avg-prd_avg)/prd_avg*100, 0), "%)")) %>%
      pull(bias)
    
  } else NULL
  
  si_bias1 <- ifelse(is.null(si_bias), "None", paste(si_bias, collapse=" "))
  
  return(si_bias1)
  
})



total_remeas_plot <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  total_remeas_plot <- sample_data %>% 
    filter(SITE_IDENTIFIER %in% site_id()) %>%
    group_by(SITE_IDENTIFIER) %>%
    filter(VISIT_NUMBER == max(VISIT_NUMBER), VISIT_NUMBER != 1) %>%
    ungroup() %>%
    summarize(nreplot = length(unique(SITE_IDENTIFIER))) %>%
    pull(nreplot)
  
  return(total_remeas_plot)
  
})



fig10_dat_final <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  # *only interested in remeasured samples;
  FH_dat_coc <- tree_fh_data %>%
    filter(CLSTR_ID %in% clstr_id_last2(), !is.na(PHF_TREE), S_F == "S") %>%
    group_by(SITE_IDENTIFIER) %>%
    mutate(new_visit_number = ifelse(VISIT_NUMBER == max(VISIT_NUMBER), 1, 2))
  
  FH_dat_coc <- FH_dat_coc %>%
    ungroup() %>% 
    mutate(COMP_CHG_new = COMP_CHG,
           COMP_CHG_new = ifelse(new_visit_number == 2 & LV_D == "D", "D", COMP_CHG_new),
           COMP_CHG_new = ifelse(new_visit_number == 2 & LV_D == "L", "E", COMP_CHG_new)) %>%
    data.table
  
  FH_dat_coc1 <- FH_dat_coc %>%
    filter(!is.na(ba_ha)) %>%
    ungroup() %>%
    mutate(n = n_distinct(SITE_IDENTIFIER))
  
  # *compute incidence by visit and by live dead, then merge and average all samples per mu;
  # *compute totals for common denominator between measurements;
  FH_dat_coc2 <- FH_dat_coc1 %>%
    filter(DAM_NUM == 1)  %>%
    group_by(n, SITE_IDENTIFIER, new_visit_number, COMP_CHG_new) %>%
    summarize(tot_ba_comp = sum(ba_ha, na.rm = T),
              tot_stems_comp = sum(PHF_TREE, na.rm = T),
              n_tree = n())  %>%
    #ungroup() %>%
    data.table()
  
  if (nrow(FH_dat_coc2) > 1){
    FH_dat_coc2_1 <- FH_dat_coc2 %>%
      dcast(n + SITE_IDENTIFIER + new_visit_number ~ COMP_CHG_new,
            value.var = "tot_stems_comp", drop=FALSE, fill=0, sep = "_") %>%
      mutate(totsph_comdem = E + M + S)
  } else {
    FH_dat_coc2_1 <- data.frame()
  }
  
  # *recompute incidence based on common demoninator between measurements;
  # *summarize totals by damage agent for each coc;
  FH_dat_coc3 <- FH_dat_coc1 %>%
    filter(DAM_NUM == 1)  %>%
    group_by(n, SITE_IDENTIFIER, new_visit_number, AGN, COMP_CHG_new) %>%
    summarize(tot_ba_dam_comp = sum(ba_ha, na.rm = T),
              tot_stems_dam_comp = sum(PHF_TREE, na.rm = T),
              n_tree = n())  %>%
    ungroup() %>%
    data.table()
  
  # *sum the totals so to compare live standing at first measure, to those same trees at second measure, wheher still alive;
  # *of if they died during that period;
  if (nrow(FH_dat_coc3) > 1){
    FH_dat_coc3_1 <- FH_dat_coc3 %>%
      dcast(n + SITE_IDENTIFIER + new_visit_number + AGN ~ COMP_CHG_new,
            value.var = "tot_stems_dam_comp", drop=FALSE, fill=0, sep = "_") %>%
      mutate(damsph_comdem = E + M + S)
    
    FH_dat_coc4 <- FH_dat_coc3_1 %>%
      left_join(FH_dat_coc2_1, 
                by = c("n", "SITE_IDENTIFIER", "new_visit_number"),
                suffix = c(".dam", ".comp"))
    
    FH_dat_coc5 <- FH_dat_coc4 %>%
      ungroup() %>%
      group_by(n, new_visit_number, AGN) %>%
      summarise_all(mean, .names = "mean_{.col}")
    
    FH_dat_coc5 <- FH_dat_coc5 %>%
      ungroup() %>%
      mutate(incid_stems = damsph_comdem/totsph_comdem,
             perc_mort = M.dam/damsph_comdem,
             prob_get_and_die = incid_stems*perc_mort)
    
    fig10_dat_final <- FH_dat_coc5 %>%
      mutate(dam_1letter = toupper(substr(AGN, 1, 1)),
             dam_class = case_when(dam_1letter %in% c('O', '') ~ 'None',
                                   dam_1letter == 'U' ~ 'Unknown',
                                   dam_1letter == 'N' ~ 'Abiotic',
                                   dam_1letter == 'D' ~ 'Disease',
                                   dam_1letter == 'I' ~ 'Insect',
                                   dam_1letter == 'T' ~ 'Trt',
                                   dam_1letter == 'A' ~ 'Animal',
                                   dam_1letter == 'X' ~ 'Frk_Crk_Btp',
                                   TRUE ~ '')) %>%
      mutate(dam_class = fct_reorder(dam_class, -incid_stems)) 
  } else {
    fig10_dat_final <- data.frame()
  }
  
  return(fig10_dat_final)
})



risk_vol <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  volsum_num <- tree_fh_data %>%
    filter(CLSTR_ID %in% clstr_id(), DAM_NUM==1, LV_D=="L", S_F == "S") %>%
    reframe(volsum = sum(vol_ha, na.rm = T)) %>%
    pull(volsum)
  
  meanage = ysm_msyt_vdyp_volume %>%
    filter(CLSTR_ID %in% clstr_id()) %>%
    summarize(meanage = mean(ref_age_adj)) %>%
    pull(meanage)
  
  risk_vol <- tree_fh_data %>%
    filter(CLSTR_ID %in% clstr_id(), DAM_NUM==1, LV_D=="L", 
           mort_flag %in% c(1,2), !(AGN_new %in% c('DSC', 'DSG', 'DSS'))) %>%
    group_by(mort_flag, AGN_new) %>%
    reframe(volsum = sum(vol_ha, na.rm = T))  %>%
    arrange(mort_flag, desc(volsum)) %>%
    mutate(volperc = ifelse(mort_flag == 1, volsum /volsum_num*0.9, volsum /volsum_num),
           meanyear = meanage,
           year60 = ifelse(mort_flag==2, volperc*0.0025*(60-meanyear), volperc),
           year80 = ifelse(mort_flag==2, volperc*0.0025*(80-meanyear), volperc),
           year100 = ifelse(mort_flag==2, volperc*0.0025*(100-meanyear), volperc))
  
  
  return(risk_vol)
  
})



year100_immed <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  risk_vol <- risk_vol()
  
  year100_immed <- round(sum(risk_vol[risk_vol$mort_flag==1,]$year100*100),1)
  
  return(year100_immed)
  
})



year100_inc <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  risk_vol <- risk_vol()
  
  year100_inc<- round(sum(risk_vol[risk_vol$mort_flag==2,]$year100)*100,1)
  
  return(year100_inc)
  
})


year100_comb <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  risk_vol <- risk_vol()
  year100_immed <- year100_immed()
  year100_inc <- year100_inc()
  
  year100_comb<- round((1-(1-year100_immed/100)*(1-year100_inc/100))*100,1)
  
  return(year100_comb)
  
  
})



meanage <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  meanage = ysm_msyt_vdyp_volume %>%
    filter(CLSTR_ID %in% clstr_id()) %>%
    summarize(meanage = mean(ref_age_adj)) %>%
    pull(meanage)
  
  return(meanage)
  
})


volproj <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  risk_vol <- risk_vol()
  meanage <- meanage()
  
  year100_immed <- year100_immed()
  year100_inc<- year100_inc()
  year100_comb<- year100_comb()
  
  volproj <- tsr_tass_volproj %>%
    filter(CLSTR_ID %in% clstr_id()) %>%
    mutate(GMV_adj1 = GMV_adj*(1- year100_immed/100)*
             (1-max(AGE - meanage, 0)*year100_inc/100*sum(risk_vol[risk_vol$mort_flag==2,]$volperc)),
           n_si = length(unique(SITE_IDENTIFIER)))
  
  volproj1 <- volproj %>%
    #filter(!is.na(rust)) %>%
    group_by(rust, AGE) %>%
    summarize(meanvol_tsr = mean(volTSR, na.rm = T),
              sd_tsr = sd(volTSR , na.rm = T),
              meanvol_tass = mean(GMV_adj1, na.rm = T),
              sd_tass = sd(GMV_adj1 , na.rm = T),
              n = n()) %>%
    ungroup() %>%
    mutate(se_tsr = sd_tsr/sqrt(n),
           se_tass = sd_tass/sqrt(n),
           tstat = ifelse(n >1, qt(0.975, n-1), NA),
           u95_tsr = meanvol_tsr + tstat*se_tsr,
           l95_tsr = meanvol_tsr - tstat*se_tsr,
           mai_tsr = meanvol_tsr/AGE,
           u95_tass = meanvol_tass + tstat*se_tass,
           l95_tass = meanvol_tass - tstat*se_tass,
           mai_tass = meanvol_tass/AGE)
  
  return(volproj1)
})


projectiontable <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  risk_vol <- risk_vol()
  meanage <- meanage()
  
  year100_immed <- year100_immed()
  year100_inc<- year100_inc()
  year100_comb<- year100_comb()
  
  projectiontable <- tsr_tass_volproj %>%
    filter(CLSTR_ID %in% clstr_id(), AGE %in% c(60, 70, 80, 90, 100), rust == "Y") %>%
    mutate(GMV_adj1 = GMV_adj*(1- year100_immed/100)*
             (1-max(AGE - meanage, 0)*year100_inc/100*sum(risk_vol[risk_vol$mort_flag==2,]$volperc)),
           voldiff = volTSR - GMV_adj1) %>%
    group_by(AGE) %>%
    summarize(n_samples = n(),
              meanvol_tsr = round(mean(volTSR, na.rm = T), 0),
              meanvol_tass = round(mean(GMV_adj1, na.rm = T), 0),
              meanvoldiff = round(mean(voldiff, na.rm = T), 0),
              sdvoldiff = sd(voldiff, na.rm = T)) %>%
    ungroup() %>%
    mutate(se_voldiff = sdvoldiff/sqrt(n_samples),
           pval = round(2*pt(abs(meanvoldiff)/se_voldiff, n_samples-1, lower.tail = F), 3),
           percvoldiff = paste0(round(meanvoldiff/meanvol_tass*100, 0), "%"))
  
  return(projectiontable)
  
})



Fig14_dat <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  Fig14_dat <- ysm_msyt_vdyp_volume %>%
    filter(CLSTR_ID %in% clstr_id()) %>%
    mutate(agediff = PROJ_AGE_ADJ - ref_age_adj) 
  
  temp <- SI_data %>%
    filter(CLSTR_ID %in% clstr_id()) %>%
    group_by(CLSTR_ID)%>%
    summarize(meanage = mean(AGET_TLSO, na.rm = T))
  
  Fig14_dat <- Fig14_dat %>%
    left_join(temp, by = "CLSTR_ID")
  
  Fig14_dat <- Fig14_dat %>%
    ungroup() %>%
    mutate(aget_diff = ref_age_adj - meanage) %>%
    filter(!is.na(aget_diff)) %>%
    select(CLSTR_ID, meanage, ref_age_adj, aget_diff)
  
  return(Fig14_dat)
  
})



age_p <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  Fig14_dat <- Fig14_dat()
  
  age_p <- t.test(Fig14_dat$aget_diff)$p.value
  
  return(age_p)
  
})



Fig15_dat <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  comp_dat <- ysm_msyt_vdyp_volume %>%
    filter(SITE_IDENTIFIER %in% site_id()) 
  
  comp_dat <- comp_dat %>%
    mutate(grdnv = ifelse(is.na(grdnv), 0, grdnv),
           prednv = ifelse(is.na(prednv), 0, prednv),
           tassnv = ifelse(is.na(tassnv), 0, tassnv),
           voldiffTASS = ifelse(is.na(voldiffTASS), 0, voldiffTASS),
           voldiffTSR = ifelse(is.na(voldiffTSR), 0, voldiffTSR)
    )  
  
  setDT(comp_dat)[, year_dff := MEAS_YR - lag(MEAS_YR), by = SITE_IDENTIFIER]
  setDT(comp_dat)[, grdnv_diff := grdnv - lag(grdnv), by = SITE_IDENTIFIER]
  setDT(comp_dat)[, prednv_diff := prednv - lag(prednv), by = SITE_IDENTIFIER]
  setDT(comp_dat)[, tassnv_diff := tassnv - lag(tassnv), by = SITE_IDENTIFIER]
  
  Fig15_dat <- comp_dat %>%
    select(SITE_IDENTIFIER, year_dff, grdnv_diff, prednv_diff, tassnv_diff) %>%
    filter(!is.na(year_dff)) %>%
    mutate(grdnv_pai = grdnv_diff/year_dff,
           prednv_pai = prednv_diff/year_dff,
           tass_pai = tassnv_diff/year_dff)
  
  return(Fig15_dat)
  
})



test1 <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  Fig15_dat <- Fig15_dat()
  
  if(nrow(Fig15_dat) > 0 & length(Fig15_dat$prednv_pai - Fig15_dat$grdnv_pai) > 1){
    test1<-t.test(Fig15_dat$prednv_pai - Fig15_dat$grdnv_pai)
    test1result <- test1$estimate
  }
  else test1result <- NA
  
  return(test1result)
  
})

test2 <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  Fig15_dat <- Fig15_dat()
  
  if(nrow(Fig15_dat) > 0 & length(Fig15_dat$tass_pai - Fig15_dat$grdnv_pai) > 1){
    test2<-t.test(Fig15_dat$tass_pai - Fig15_dat$grdnv_pai)
    test2result <- test2$estimate
  }
  else test2result <- NA
  
  return(test2result)
  
})
