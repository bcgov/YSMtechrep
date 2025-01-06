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
  
  if (input$SelectCategory == "TSA_DESC"){
    site_id <- sample_data %>% 
      filter(TSA_filter == "Y") %>% 
      filter(!!sym(input$SelectCategory) %in% input$SelectVar) %>%
      pull(SITE_IDENTIFIER)
  } else if (input$SelectCategory == "BECsub"){
    site_id <- sample_data %>% 
      filter(BEC_filter == "Y") %>% 
      filter(!!sym(input$SelectCategory) %in% input$SelectVar) %>%
      pull(SITE_IDENTIFIER)
  }
  
  return(site_id)
  
})

clstr_id <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  if (input$SelectCategory == "TSA_DESC"){
    clstr_id <- sample_data %>% 
      filter(TSA_filter == "Y") %>% 
      filter(!!sym(input$SelectCategory) %in% input$SelectVar) %>%
      filter(LAST_MSMT == "Y") %>%
      pull(CLSTR_ID)
  } else if (input$SelectCategory == "BECsub"){
    clstr_id <- sample_data %>% 
      filter(BEC_filter == "Y") %>% 
      filter(!!sym(input$SelectCategory) %in% input$SelectVar) %>%
      filter(LAST_MSMT == "Y") %>%
      pull(CLSTR_ID)
  }

  return(clstr_id)
  
})

clstr_id_all <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  if (input$SelectCategory == "TSA_DESC"){
    clstr_id_all <- sample_data %>% 
      filter(TSA_filter == "Y") %>% 
      filter(!!sym(input$SelectCategory) %in% input$SelectVar) %>%
      pull(CLSTR_ID)
  } else if (input$SelectCategory == "BECsub"){
    clstr_id_all <- sample_data %>% 
      filter(BEC_filter == "Y") %>% 
      filter(!!sym(input$SelectCategory) %in% input$SelectVar) %>%
      pull(CLSTR_ID)
  }
  
  return(clstr_id_all)
  
})


clstr_id_last2 <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  if (input$SelectCategory == "TSA_DESC"){
    clstr_id_last2 <- sample_data %>% 
      filter(TSA_filter == "Y") %>% 
      filter(!!sym(input$SelectCategory) %in% input$SelectVar) %>%
      group_by(SITE_IDENTIFIER) %>%
      filter(n() > 1) %>% 
      arrange(VISIT_NUMBER) %>% 
      slice_tail(n = 2) %>%
      pull(CLSTR_ID)
  } else if (input$SelectCategory == "BECsub"){
    clstr_id_last2 <- sample_data %>% 
      filter(BEC_filter == "Y") %>% 
      filter(!!sym(input$SelectCategory) %in% input$SelectVar) %>%
      group_by(SITE_IDENTIFIER) %>%
      filter(n() > 1) %>% 
      arrange(VISIT_NUMBER) %>% 
      slice_tail(n = 2) %>%
      pull(CLSTR_ID)
  }
  
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
    group_by(CLSTR_ID) %>% 
    select(SITE_IDENTIFIER, CLSTR_ID, SPECIES, SPC_GRP1, 
           BA_HA_LS, BA_HA_DS, STEMS_HA_LS, STEMS_HA_DS, VHA_WSV_LS, VHA_WSV_DS,
           QMD_LS, QMD_DS) 
  
  summary_mer<-spcs_data %>% 
    filter(CLSTR_ID %in% clstr_id(), UTIL == ifelse(SPECIES =="PL", 12.5, 17.5)) %>% 
    select(SITE_IDENTIFIER, CLSTR_ID, UTIL, SPECIES, 
           VHA_MER_LS, VHA_MER_DS)
  
  summary_data <- summary_data %>%
    left_join(summary_mer, by = c('SITE_IDENTIFIER', 'CLSTR_ID', "SPECIES")) %>%
    data.table()
  
  return(summary_data)
  
})

summary_si <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  summary_si <- spcs_data %>% 
    filter(CLSTR_ID %in% clstr_id(), UTIL == 4) %>%
    mutate(SPC_GRP1 = substr(SPECIES,1,2)) %>%
    mutate(SPC_GRP1 = ifelse(SPECIES %in% decidspc, 'DE', SPC_GRP1)) %>%
    arrange(SITE_IDENTIFIER, CLSTR_ID, desc(BA_HA_LS)) %>%
    group_by(SITE_IDENTIFIER, CLSTR_ID) %>%
    slice(1) 
  
  summary_si <- summary_si %>%
    left_join(SI_data, by = c('SITE_IDENTIFIER', 'CLSTR_ID', "SPECIES")) %>%
    ungroup() %>%
    summarise(n = sum(!is.na(AGET_TLSO)),
              Avg = ifelse(n>0, mean(AGET_TLSO, na.rm = T), NA),
              Min = ifelse(n>0, min(AGET_TLSO, na.rm = T), NA),
              Max = ifelse(n>0, max(AGET_TLSO, na.rm = T), NA)) %>%
    as.data.table()
  
  return(summary_si)
})


decid_vol <- reactive({
  
  summary_data <- summary_data()
  
  decid_vol <- summary_data %>%
    mutate(spc_dec_con = ifelse(SPC_GRP1 == "DE", "decid", "con")) %>%
    group_by(spc_dec_con) %>%
    summarize(VHA_WSV_LS = sum(VHA_WSV_LS, na.rm = T)) %>%
    ungroup() %>%
    mutate(vol = round(VHA_WSV_LS/sum(VHA_WSV_LS, na.rm = T)*100,0)) %>%
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
           SPC_GRP1 = ifelse(is.na(SP_PCT_BA_LS), 'Nonstock', SPC_GRP1),
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
  ysm_spc2_1 <- ysm_spc1 %>%
    filter(!(SPECIES %in% c("", NA))) %>%
    mutate(n_ci = length(unique(CLSTR_ID)),
           SPC_GRP1 = ifelse(SPECIES %in% decidspc, 'DE', SPECIES))
  
  ### Compute mean stems per ha by species
  # Ground samples
  ysm_spc3_1 <- ysm_spc2_1 %>%
    group_by(SPC_GRP1) %>%
    reframe(mean_BA_PC = sum(SP_PCT_BA_LS, na.rm = T)/n_ci) %>%
    distinct()
  
  ysm_spc6 <- ysm_spc3_1 %>%
    arrange(desc(mean_BA_PC)) %>%
    mutate(order = row_number(),
           SPC_GRP2 = ifelse(order <= 7, SPC_GRP1, 'Other'),
           SPC_GRP2 = ifelse(SPC_GRP2 == 'DE', 'Decid', SPC_GRP2))
  
 # MSYT and VDYP inputs
  regen1 <- regen_data %>%
    filter(CLSTR_ID %in% clstr_id()) %>%
    filter(!(SPECIES %in% c("", NA))) %>%
    mutate(n_si = length(unique(CLSTR_ID)),
           n_ft = length(unique(FEATURE_ID)))
  
  regen2 <- regen1 %>%
    group_by(SPC_GRP1) %>%
    reframe(mean_PC = sum(SPECIES_PCT, na.rm = T)/n_ft) %>%
    distinct()
  
  regen3 <- regen2 %>%
    arrange(desc(mean_PC)) %>%
    mutate(order = row_number(),
           SPC_GRP2 = ifelse(order <= 7, SPC_GRP1, 'Other'),
           SPC_GRP2 = ifelse(SPC_GRP2 == 'DE', 'Decid', SPC_GRP2))
  
  ### Data for figure
  ysm_spc_dat <- ysm_spc6 %>% 
    mutate(source="YSM") %>%
    select(SPC_GRP1, spcperc = mean_BA_PC, order, SPC_GRP2, source)
  
  regen_dat <- regen3 %>%
    mutate(source="TSR_INPUT",
           spcperc = mean_PC*100) %>%
    select(SPC_GRP1, spcperc, order, SPC_GRP2, source)
  
  Fig11_dat <- rbind(ysm_spc_dat, regen_dat) 
  
  return(Fig11_dat)
  
})

percoverlap <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  Fig11_dat <- Fig11_dat()
  
  spccoverlap <- Fig11_dat %>%
    select(-order) %>%
    pivot_wider(names_from = source,
                names_sep = ".",
                values_from = c(spcperc),
                values_fill = 0) %>%
    rowwise() %>%
    mutate(diff = abs(YSM - TSR_INPUT),
           overlap = min(YSM, TSR_INPUT),
           overlapmax = max(YSM, TSR_INPUT)) %>%
    summarize(diff_sum = sum(diff, na.rm = T),
              over_sum = sum(overlap, na.rm = T),
              max_sum = sum(overlapmax, na.rm = T))
  
  percoverlap <- round(sum(spccoverlap$over_sum, na.rm = T)/sum(spccoverlap$max_sum, na.rm = T)*100, 0)
  
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
  
  si_dat_1 <- SI_data %>%
    filter(CLSTR_ID %in% clstr_id(), !is.na(ratio)) %>%
    group_by(SITE_IDENTIFIER, SPECIES) %>%
    slice(which.min(yrs_from_50))
  
  si_dat1 <- si_dat_1 %>%
    group_by(SPECIES) %>%
    summarise(n = n(),
              age_avg = mean(meanage, na.rm = T),
              grd_avg = mean(meansi, na.rm = T),
              prd_avg = mean(pspl_si, na.rm = T))
  
  si_dat1 <- si_dat1 %>%
    ungroup() %>%
    mutate(ROM = grd_avg/prd_avg)
  
  si_dat2 <- si_dat_1 %>%
    left_join(si_dat1, , by = "SPECIES")
  
  # *compute additional attribute to compute variance;
  si_dat2 <- si_dat2 %>%
    mutate(diffs_sqrd = (meansi - (ROM * pspl_si))**2)
  
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
  
  si_dat6 <- si_dat5 %>%
    filter(n >1)
  
  return(si_dat6)
  
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
  
  si_bias1 <- ifelse(is.null(si_bias), "No bias, or sample size too small", paste(si_bias, collapse=" "))
  
  return(si_bias1)
  
})


remeas_plot <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
remeas_plot <- sample_data %>% 
  filter(SITE_IDENTIFIER %in% site_id()) %>%
  group_by(SITE_IDENTIFIER) %>%
  arrange(VISIT_NUMBER) %>%
  mutate(meas_no = row_number()) %>%
  filter(meas_no == max(meas_no), meas_no != 1) %>%
  ungroup() %>%
  #summarize(nreplot = length(unique(SITE_IDENTIFIER))) %>%
  pull(CLSTR_ID)

return(remeas_plot)

})


total_remeas_plot <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  remeas_plot <- remeas_plot()
  
  total_remeas_plot <- length(remeas_plot)
  
  return(total_remeas_plot)
  
})



fig10_dat_final <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  fig8_dat <- fig8_dat()
  
  # *prep file to compare incidence over time for subset of remeasured samples;
  # *get last two measurements for analysis;
  FH_dat_coc <- fig8_dat %>%
    # *only interested in remeasured samples, and only the last two measurements to compare change;
    filter(CLSTR_ID %in% clstr_id_last2()) %>%
    mutate(n_si = n_distinct(substr(clstr_id_last2(), 1, 7))) %>%
    group_by(SITE_IDENTIFIER) %>%
    # *rename first_last FP to P for reporting purposes, as this change analysis is based on the last two visits;
    mutate(new_visit_number = ifelse(VISIT_NUMBER == min(VISIT_NUMBER), 'First', 'Last')) %>%
    ungroup()
  
  # *when previous visit is also the first visit, no components of change.  
  # *to make damage agent change comparison between the last two visits;
  # *need to fill in components of change for first visit to "E" as a default (establishment);
  FH_dat_coc <- FH_dat_coc %>%
    rowwise() %>%
    mutate(#COMP_CHG_new = COMP_CHG,
      COMP_CHG_new = comp_chg_coc,
      COMP_CHG_new = ifelse(new_visit_number == 'First' & LV_D == "D", "D", COMP_CHG_new),
      COMP_CHG_new = ifelse(new_visit_number == 'First' & LV_D == "L", "E", COMP_CHG_new)) %>%
    data.table
  
  FH_dat_coc1 <- FH_dat_coc %>%
    #mutate(n = n_distinct(SITE_IDENTIFIER)) %>%
    filter(!is.na(BA_TREE), !is.na(phf_coc)) %>%
    ungroup()
  
  # *compute incidence by visit and by live dead, then merge and average all samples per mu;
  # *compute totals for common denominator between measurements;
  FH_dat_coc2 <- FH_dat_coc1 %>%
    filter(DAM_NUM == 1)  %>%
    group_by(n_si, SITE_IDENTIFIER, new_visit_number, COMP_CHG_new) %>%
    summarize(tot_ba_comp = sum(phf_coc*BA_TREE, na.rm = T),
              tot_stems_comp = sum(phf_coc, na.rm = T),
              n_tree = n())  %>%
    ungroup() %>%
    data.table()
  
  # *need to fill in missing records of each forest health pest by sample id , with zeros;
  if (nrow(FH_dat_coc2) > 1){
    FH_dat_coc2_1 <- FH_dat_coc2 %>%
      dcast(n_si + SITE_IDENTIFIER + new_visit_number ~ COMP_CHG_new,
            value.var = "tot_stems_comp", drop=FALSE, fill=0, sep = "_") %>%
      mutate(totsph_comdem = E + M + S)
  } else {
    FH_dat_coc2_1 <- data.frame()
  }
  
  # *recompute incidence based on common demoninator between measurements;
  # *summarize totals by damage agent for each coc;
  FH_dat_coc3 <- FH_dat_coc1 %>%
    filter(DAM_NUM == 1)  %>%
    group_by(n_si, SITE_IDENTIFIER, new_visit_number, AGN, COMP_CHG_new) %>%
    summarize(tot_ba_dam_comp = sum(phf_coc*BA_TREE, na.rm = T),
              tot_stems_dam_comp = sum(phf_coc, na.rm = T),
              n_tree = n())  %>%
    ungroup() %>%
    data.table()
  
  # *sum the totals so to compare live standing at first measure, to those same trees at second measure, wheher still alive;
  # *of if they died during that period;
  if (nrow(FH_dat_coc3) > 1){
    FH_dat_coc3_1 <- FH_dat_coc3 %>%
      # *creates full join of all fh damage agents per sample;
      dcast(n_si + SITE_IDENTIFIER + new_visit_number + AGN ~ COMP_CHG_new,
            value.var = "tot_stems_dam_comp", drop=FALSE, fill=0, sep = "_") %>%
      mutate(damsph_comdem = E + M + S)
    
    FH_dat_coc4 <- FH_dat_coc3_1 %>%
      left_join(FH_dat_coc2_1, 
                by = c("n_si", "SITE_IDENTIFIER", "new_visit_number"),
                suffix = c(".dam", ".comp"))
    
    FH_dat_coc5 <- FH_dat_coc4 %>%
      ungroup() %>%
      group_by(n_si, new_visit_number, AGN) %>%
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
    filter(CLSTR_ID %in% clstr_id(), DAM_NUM==1, LV_D=="L", S_F == "S",
           mort_flag %in% c(1,2)) %>%
    rowwise() %>%
    filter(if(MEAS_YR >= 2017) !(AGN_new %in% c('DSC', 'DSG', 'DSS')) else TRUE) %>%
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


max_measyear <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  maxyear <- sample_data %>%
    filter(CLSTR_ID %in% clstr_id_all()) %>%
    summarize(maxyear = max(MEAS_YR)) %>%
    pull(maxyear)
  
  return(maxyear)
  
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
  
  volproj1 <- setDT(tsr_tass_volproj) %>%
    filter(CLSTR_ID %in% clstr_id()) %>%
    arrange(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, desc(xy), desc(rust), desc(TASS_ver)) %>%
    group_by(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, rust, AGE) %>%
    slice(1) %>%
    mutate(volTASS_adj = volTASS*(1- year100_immed/100)*
             (1-max(AGE - meanage, 0)*0.25/100*sum(risk_vol[risk_vol$mort_flag==2,]$volperc)),
           n_si = length(unique(SITE_IDENTIFIER)),
           n_ci = length(unique(CLSTR_ID)))
  
  #volproj2 <-volproj1 %>%
  #  group_by(AGE, rust) %>%
  #  summarize(meanvol_tsr = mean(volTSR, na.rm = T),
  #            sd_tsr = sd(volTSR , na.rm = T),
  #            meanvol_tass = mean(volTASS_adj, na.rm = T),
  #            sd_tass = sd(volTASS_adj , na.rm = T),
  #            n = n()) %>%
  #  ungroup() %>%
  #  mutate(se_tsr = sd_tsr/sqrt(n),
  #         se_tass = sd_tass/sqrt(n),
  #         tstat = ifelse(n >1, qt(0.975, n-1), NA),
  #         u95_tsr = meanvol_tsr + tstat*se_tsr,
  #         l95_tsr = meanvol_tsr - tstat*se_tsr,
  #         mai_tsr = meanvol_tsr/AGE,
  #         u95_tass = meanvol_tass + tstat*se_tass,
  #         l95_tass = meanvol_tass - tstat*se_tass,
  #         mai_tass = meanvol_tass/AGE)
  
  return(volproj1)
})


stemrustimpact <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  volproj <- volproj()
  
  stemrustimpact <- volproj %>%
    arrange(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, desc(xy), desc(rust)) %>%
    group_by(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, rust, AGE) %>%
    slice(1) %>%
    group_by(AGE, rust) %>%
    summarize(meanvol_tsr = mean(volTSR, na.rm = T),
              sd_tsr = sd(volTSR , na.rm = T),
              meanvol_tass = mean(volTASS_adj, na.rm = T),
              sd_tass = sd(volTASS_adj , na.rm = T),
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
           mai_tass = meanvol_tass/AGE) %>%
    filter(AGE %in% c(60, 70, 80, 90, 100)) %>%
    pivot_wider(id_cols = AGE,
                names_from = rust, values_from = meanvol_tass) %>%
    mutate(rustimpact = paste0(round((N-Y)/N*100,1), "%")) 
  
  return(stemrustimpact)
})


projectiontable <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  #risk_vol <- risk_vol()
  #meanage <- meanage()
  #
  #year100_immed <- year100_immed()
  #year100_inc<- year100_inc()
  #year100_comb<- year100_comb()
  
  volproj <- volproj()
  
  #projectiontable <- tsr_tass_volproj %>%
  #  filter(CLSTR_ID %in% clstr_id(), AGE %in% c(60, 70, 80, 90, 100)) %>%
  #  mutate(GMV_adj1 = GMV_approx*(1- year100_immed/100)*
  #           (1-max(AGE - meanage, 0)*0.25/100*sum(risk_vol[risk_vol$mort_flag==2,]$volperc)),
  #         voldiff = volTSR - GMV_adj1) %>%
  #  arrange(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, desc(xy), desc(rust)) %>%
  #  group_by(SITE_IDENTIFIER, AGE) %>%
  #  slice(1) %>%
  #  ungroup() %>%
  #  group_by(AGE) %>%
  #  summarize(n_samples = n(),
  #            meanvol_tsr = round(mean(volTSR, na.rm = T), 0),
  #            meanvol_tass = round(mean(GMV_adj1, na.rm = T), 0),
  #            meanvoldiff = round(mean(voldiff, na.rm = T), 0),
  #            sdvoldiff = sd(voldiff, na.rm = T)) %>%
  #  ungroup() %>%
  #  mutate(se_voldiff = sdvoldiff/sqrt(n_samples),
  #         pval = round(2*pt(abs(meanvoldiff)/se_voldiff, n_samples-1, lower.tail = F), 3),
  #         percvoldiff = paste0(round(meanvoldiff/meanvol_tass*100, 0), "%"))
  
  projectiontable <- volproj %>%
    filter(AGE %in% c(60, 70, 80, 90, 100)) %>%
    arrange(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, desc(xy), desc(rust)) %>%
    group_by(SITE_IDENTIFIER, AGE) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(voldiff = volTSR - volTASS_adj) %>%
    group_by(AGE) %>%
    summarize(meanvol_tsr = round(mean(volTSR, na.rm = T), 0),
              meanvol_tass = round(mean(volTASS_adj, na.rm = T), 0),
              n = n(),
              meanvoldiff = round(mean(voldiff, na.rm = T), 0),
              sdvoldiff = sd(voldiff, na.rm = T)) %>%
    ungroup() %>%
    mutate(se_voldiff = sdvoldiff/sqrt(n),
           pval = round(2*pt(abs(meanvoldiff)/se_voldiff, n-1, lower.tail = F), 3),
           percvoldiff = paste0(round(meanvoldiff/meanvol_tass*100, 0), "%"))
  
  return(projectiontable)
  
})



Fig14_dat <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  Fig14_dat <- ysm_msyt_vdyp_volume %>%
    filter(CLSTR_ID %in% clstr_id()) %>%
    select(CLSTR_ID, ref_age_adj) 
  
  temp <- spcs_data %>%
    filter(CLSTR_ID %in% clstr_id(), UTIL == 4) %>%
    group_by(SITE_IDENTIFIER) %>%
    arrange(desc(SP_PCT_BA_LS)) %>%
    slice_head(n = 1)%>%
    select(SITE_IDENTIFIER, CLSTR_ID, SPECIES)
  
  Fig14_dat1 <- Fig14_dat %>%
    left_join(temp, by = 'CLSTR_ID') %>%
    left_join(siteage_data %>% select(CLSTR_ID, SPECIES, AGET_TLSO),
              by = c('CLSTR_ID', 'SPECIES')) %>%
    mutate(age_diff = ref_age_adj - AGET_TLSO) %>%
    filter(complete.cases(age_diff))
  
  return(Fig14_dat1)
  
})



age_p <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  Fig14_dat <- Fig14_dat()
  
  age_p <- t.test(Fig14_dat$age_diff)$p.value
  
  return(age_p)
  
})



Fig15_dat <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  comp_dat <- ysm_msyt_vdyp_volume %>%
    filter(SITE_IDENTIFIER %in% site_id()) %>%
    arrange(SITE_IDENTIFIER, VISIT_NUMBER) %>%
    group_by(SITE_IDENTIFIER) %>%
    filter(VISIT_NUMBER == min(VISIT_NUMBER) | VISIT_NUMBER == max(VISIT_NUMBER))
  
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
  
  if(nrow(Fig15_dat) > 0 & all(!is.na(Fig15_dat$grdnv_pai)) & 
     length(Fig15_dat$prednv_pai - Fig15_dat$grdnv_pai) > 1){
    test1<-t.test(Fig15_dat$prednv_pai - Fig15_dat$grdnv_pai)
    test1est <- test1$estimate
    test1p <- test1$p.value
    test1result <- c(test1est, test1p)
  } else test1result <- c(NA, NA)
  
  return(test1result)
  
})

test2 <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  Fig15_dat <- Fig15_dat()
  
  if(nrow(Fig15_dat) > 0 & all(!is.na(Fig15_dat$grdnv_pai)) & 
     length(Fig15_dat$tass_pai - Fig15_dat$grdnv_pai) > 1){
    test2<-t.test(Fig15_dat$tass_pai - Fig15_dat$grdnv_pai)
    test2est <- test2$estimate
    test2p <- test2$p.value
    test2result <- c(test2est, test2p)
  } else test2result <- c(NA, NA)
  
  return(test2result)
  
})

test1_comment <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  if (total_remeas_plot() > 0){
    
  test1 <- test1()
  
  if (!is.null(test1) & !is.na(test1[2]) & test1[2] < 0.05){
    test1_comment <- paste0("TSR is ", "<b>",
                            ifelse(!is.na(test1[1]) & test1[1] > 0, "over", "under"), "</b>",
                            "-estimating actual growth by ", "<b>", 
                            ifelse(!is.na(test1[1]), round(abs(test1[1]), 1), "-"), 
                            "</b>"," m<sup>3</sup>/ha/yr.</br>")
  } else if (!is.null(test1) & !is.na(test1[2]) & test1[2] >= 0.05){
    test1_comment <- "no significant difference between TSR and YSM."
  } else if (is.null(test1) | all(is.na(test1))){
    test1_comment <- "(insufficient remeasured data)"
  }} else {
    test1_comment <- "No re-measred YSM samples"
  }
  
  return(test1_comment)
  
})

test2_comment <- reactive({
  
  req(input$SelectCategory, input$SelectVar)
  input$genearate
  
  if (total_remeas_plot() > 0){
  test2 <- test2()
  
  if (!is.null(test2) & !is.na(test2[2])  & test2[2] < 0.05){
    test2_comment <- paste0("TASS is ", "<b>",
                             ifelse(!is.na(test2[1]) & test2[1] > 0, "over", "under"), "</b>",
                             "-estimating actual growth by ", "<b>", 
                            ifelse(!is.na(test2[1]), round(abs(test2[1]), 1), "-"), 
                            "</b>"," m<sup>3</sup>/ha/yr.</br>")
  } else if (!is.null(test2) & !is.na(test2[2]) & test2[2] >= 0.05){
    test2_comment <- "no significant difference between TASS and YSM."
  } else if (is.null(test2) | all(is.na(test2))){
    test2_comment <- "(insufficient remeasured data)"
  }} else {
    test2_comment <- "No re-measred YSM samples"
  }
  return(test2_comment)
})


fig8_dat <- reactive({
  
  fig8_dat <- tree_fh_data %>%
    filter(CLSTR_ID %in% clstr_id_last2(), DAM_NUM==1) %>%
    group_by(SITE_IDENTIFIER, PLOT, TREE_NO) %>%
    arrange(VISIT_NUMBER) %>%
    mutate(meas_no = row_number())  %>%
    ungroup() %>%
    mutate(tree_id = paste0(SITE_IDENTIFIER, "-", TREE_NO))
  
  fig8_dat$phf_coc <- fig8_dat$PHF_TREE
  fig8_dat$resid_coc <- fig8_dat$RESIDUAL
  fig8_dat$comp_chg_coc <- fig8_dat$COMP_CHG
  fig8_dat$species_coc <- fig8_dat$SPECIES
  
  for (i in unique(fig8_dat$tree_id)){
    
    max_meas <- max(fig8_dat[fig8_dat$tree_id == i, ]$meas_no)
    
    if (max_meas > 1){
      
      for (j in 1:(max_meas-1)){
        
        a1 <- fig8_dat[fig8_dat$tree_id == i & fig8_dat$meas_no == j, ]
        a2 <- fig8_dat[fig8_dat$tree_id == i & fig8_dat$meas_no == j + 1, ]
        
        # *for components of change analysis, need to constrain phf to first measure;
        if (!is.na(a2$PHF_TREE) & a1$PHF_TREE != a2$PHF_TREE){
          fig8_dat[fig8_dat$tree_id == i & fig8_dat$meas_no == j + 1, ]$phf_coc <- a1$PHF_TREE
        }
        # *fill in residual classification if recorded at one measurement , but not the next;
        # *assign as residual across both measurements;
        if (a1$RESIDUAL != a2$RESIDUAL & (a1$RESIDUAL == "Y" | a2$RESIDUAL == "Y")){
          fig8_dat[fig8_dat$tree_id == i, ]$resid_coc <- "Y"
        }
        
        if (a1$LV_D == "L" & a2$LV_D == "L" & a2$S_F == "F"){
          
          fig8_dat[fig8_dat$tree_id == i & fig8_dat$meas_no == j + 1, ]$comp_chg_coc <- "M"
        }
        
        if (a2$SPECIES == "XC"){
          
          fig8_dat[fig8_dat$tree_id == i & fig8_dat$meas_no == j, ]$species_coc <- a1$SPECIES
        } else if (a1$SPECIES != a2$SPECIES) {
          
          fig8_dat[fig8_dat$tree_id == i & fig8_dat$meas_no == j, ]$species_coc <- a2$SPECIES
        }
      }
      # *components of change;
      # *fallen live, assume this will become mortality;
      if (a1$LV_D == "L" & a2$LV_D == "L" & a2$S_F == "F"){
        fig8_dat[fig8_dat$tree_id == i & fig8_dat$meas_no == j + 1, ]$comp_chg_coc <- "M"
      }
      # *mortality : trees that died between measurements;
      if (a1$LV_D == "L" & a2$LV_D == "D"){
        fig8_dat[fig8_dat$tree_id == i & fig8_dat$meas_no == j + 1, ]$comp_chg_coc <- "M"
      }
      
    }
    else if (max_meas == 1){
      
      c <- fig8_dat[fig8_dat$tree_id == i, ]$meas_no
      
      if (c == 1){
        
        b <- fig8_dat[fig8_dat$tree_id == i, ]
        # *live at first msmt, missing at second msmt, assign as mortality, and assume dead fallen;
        if (b$LV_D == "L"){
          
          new_cid <- paste0(substr(b$CLSTR_ID, 1, 9), 2)
          b$CLSTR_ID <- new_cid
          b$VISIT_NUMBER <- 2
          #b$MEAS_YR <- sample_data[sample_data$CLSTR_ID == new_cid, ]$MEAS_YR
          b$LV_D <- "D"
          b$S_F <- "F"
          b$comp_chg_coc <- "M"
          
          #fig8_dat <- rbind(fig8_dat, b)  ## not appear on Rene's data
        }
      }
      # *ingress trees;
      else if (c == 2){
        fig8_dat[fig8_dat$tree_id == i, ]$comp_chg_coc <- "I"
      }
    }
  }
  return(fig8_dat)
  
})
