select_sitetrees <- function(tree, type = "TLSO", suit = "Y") {
  
  th <- switch(type,
               TLSO  = c("T","L","S","O"),
               TLSOX = c("T","L","S","O","X"))
  
  st <- switch(suit,
               Y = c("Y"),
               N = c("N",NA, "", " "))
  tree %>%
    filter(
      TH_TREE %in% th,
      AGE_BH >= 5, AGE_BH <= 150,
      HEIGHT > 1.5,
      LV_D == "L",
      !(S_F == "F"),
      DBH >= 4,
      SUIT_HT == "Y",
      SUIT_TR == "Y",
      SUIT_SI %in% st
    )
}

tass_average_SI <- function(sitetreedata, 
                            esttyp_iter,
                            esttyp_dire,
                            agetyp_tage,
                            agetyp_bage){
  
  library(SIndexR)
  grd_si3 <- sitetreedata %>%
    mutate(
      si_sp = 0,    
      si_si = 0,
      si_gi = 0,
      si_new = 0,
      y2bh = 0
    ) %>%
    mutate(
      BEC_I_C = ifelse(BEC_ZONE %in% c('CWH','CDF','MH'), 'C', 'I'),
      si_sp = SIndexR_SpecRemap(SPECIES, BEC_I_C),
      site_curve = SIndexR_DefCurve(si_sp),
      grth_curve = SIndexR_DefGICurve(si_sp)
    ) %>%
    mutate(
      si_si = SIndexR_HtAgeToSI(site_curve, AGE_BH, agetyp_bage, HEIGHT, esttyp_dire)[[1]],
      siErr = SIndexR_HtAgeToSI(site_curve, AGE_BH, agetyp_bage, HEIGHT, esttyp_dire)[[2]],
      si_gi = ifelse(AGE_BH <= 50, 
                     SIndexR_HtAgeToSI(grth_curve, AGE_BH, agetyp_bage, HEIGHT, esttyp_dire)[[1]], NA),
      giErr = ifelse(AGE_BH <= 50, 
                     SIndexR_HtAgeToSI(grth_curve, AGE_BH, agetyp_bage, HEIGHT, esttyp_dire)[[2]], NA)
    ) %>%
    mutate(si_new = ifelse(!is.na(giErr) & giErr == 0, si_gi, ifelse(siErr == 0, si_si, NA))) %>%
    mutate(y2bh = SIndexR_Y2BH(site_curve, si_new)[[1]],
           ybhErr = SIndexR_Y2BH(site_curve, si_new)[[2]],
           age_tot_new = AGE_BH + y2bh) %>%
    arrange(MGMT_UNIT, SITE_IDENTIFIER, VISIT_NUMBER, SPECIES, BEC_ZONE, BEC_SBZ)
  
  a <- grd_si3 %>%
    mutate(diff = round(SI_TREE, 1) - round(si_new, 1)) %>%
    filter(abs(diff) > 0.11) %>%
    select(SITE_IDENTIFIER, VISIT_NUMBER, BEC_ZONE, BEC_SBZ, SPECIES, #SPECIES_orig,
           TREE_NO, DBH, HEIGHT, AGE_BH, SI_TREE, si_new, diff, 
           si_si, si_gi, site_curve, grth_curve, BEC_I_C)
  
  grd_si4 <- grd_si3 %>%
    summarise(
      si_o = mean(SI_TREE, na.rm = T),
      bhage_avg = mean(AGE_BH, na.rm = T),
      tage_avg = mean(AGE_TOT, na.rm = T),
      si_n = mean(si_new, na.rm = T),
      tage_n = mean(age_tot_new, na.rm = T),
      y2bh_n = mean(y2bh, na.rm = T),
      htop_avg = mean(HEIGHT, na.rm = T),
      FREQ = n(),
      .by = c("MGMT_UNIT", "SITE_IDENTIFIER", "VISIT_NUMBER", "SPECIES", "BEC_ZONE")
    )
  
  if (nrow(a) > 0){
    warning(cat("Some site trees show large difference from computation"))
    
  } 
  return(grd_si4)
  rm(grd_si3)
}

compute_ground_si <- function(tree, label){
  
  SI_by_species <- tass_average_SI(tree,
                                   esttyp_iter = 0,
                                   esttyp_dire = 1,
                                   agetyp_tage = 0,
                                   agetyp_bage = 1)
  
  SI_by_species %>%
    mutate(
      si_grd = si_n,
      si_src = label
    )
}

apply_conv <- function(dat, label){
  
  dat %>%
    mutate(bec_i_c = ifelse(BEC_ZONE %in% c("CWH","CDF","MH"), "C", "I")) %>%
    group_by(CLSTR_ID) %>%
    group_modify(~process_cluster(.x, src_label = label)) %>%
    ungroup()
}


spec_match <- c("BA" = "BG", "BA" = "BL", "BL" = "BA", "BL" = "BG", "BG" = "BA",
                "DR" = "FD", "PY" = "FD", "SS" = "PW", "LT" = "LW", "LW" = "LT", 
                "HM" = "HW", "HW" = "HM", "PA" = "PL", "PL" = "PA",
                "SW" = "SX", "SW" = "SE", "SE" = "SW",
                "AT" = "AC", "AC" = "AT")

apply_species_substitution <- function(dat){
  
  # convert spec_match to lookup table (preserves duplicates)
  spec_tbl <- enframe(spec_match, name = "SPECIES", value = "SPECIES_SUB")%>%
    group_by(SPECIES) %>%
    slice(1) %>%   # keep first match (or define priority here)
    ungroup()
  
  dat <- dat %>% 
    group_by(CLSTR_ID) %>%
    mutate(
      # group conditions
      has_DR_na = any(SPECIES == "DR" & is.na(si_grd)),
      has_FD_si = any(SPECIES == "FD" & !is.na(si_grd)),
      
      # get FD SI within group
      si_fd = si_grd[SPECIES == "FD"][!is.na(si_grd[SPECIES == "FD"])][1],
      
      # compute substitution only where needed
      si_sub = case_when(
        SPECIES == "DR" & is.na(si_grd) & has_DR_na & has_FD_si &
          BEC_ZONE %in% c("CWH","CDF","MH") & BEC_SBZ %in% c("ds","db","xm") ~ si_fd * 0.55,
        
        SPECIES == "DR" & is.na(si_grd) & has_DR_na & has_FD_si ~ si_fd * 0.73,
        
        TRUE ~ NA_real_
      ),
      
      si_grd = coalesce(si_grd, si_sub),
      
      si_src = if_else(
        is.na(si_src) & !is.na(si_sub),
        "CONV_EQN",
        si_src
      )
    ) %>%
    ungroup() %>%
    select(-has_DR_na, -has_FD_si, -si_fd, -si_sub) %>% data.table()
  
  dat %>%
    # attach substitution species
    left_join(spec_tbl, by = "SPECIES") %>%
    # bring SI from substitution species within same CLSTR_ID
    left_join(
      dat %>%
        select(CLSTR_ID, SPECIES, si_grd) %>%
        rename(SPECIES_SUB = SPECIES,
               si_sub = si_grd),
      by = c("CLSTR_ID", "SPECIES_SUB")
    ) %>%
    mutate(
      si_grd = coalesce(si_grd, si_sub),
      
      si_src = if_else(
        is.na(si_src) & !is.na(si_sub),
        "CONV_EQN_SUB",
        si_src
      )
    ) %>%
    select(-SPECIES_SUB, -si_sub) %>%
    distinct()
}

fill_pspl <- function(dat, pspl){  
  
  pspl_long <- pspl %>%
    pivot_longer(
      cols = ends_with("_SI"),
      names_to = c("SPECIES","tmp"),
      names_sep = "_",
      values_to = "PSPL_SI",
      values_drop_na = TRUE
    ) %>%
    select(-tmp)
  
  dat %>%
    # exact match
    left_join(pspl_long,
              by = c("SITE_IDENTIFIER","SPECIES")) %>%
    # substitution species
    mutate(SPECIES_SUB = spec_match[SPECIES]) %>%
    left_join(pspl_long %>%
                rename(SPECIES_SUB = SPECIES,
                       PSPL_SI_SUB = PSPL_SI),
              by = c("SITE_IDENTIFIER","SPECIES_SUB")) %>%
    mutate(
      si_grd = coalesce(si_grd, PSPL_SI, PSPL_SI_SUB),
      si_src = case_when(
        is.na(si_src) & !is.na(PSPL_SI) ~ "PSPL_SI",
        is.na(si_src) & is.na(PSPL_SI) & !is.na(PSPL_SI_SUB) ~ "PSPL_SI_SUB",
        TRUE ~ si_src
      )
    ) %>%
    select(-SPECIES_SUB, -PSPL_SI_SUB)
}

apply_lead_species_rule <- function(dat){
  
  dat %>%
    group_by(SITE_IDENTIFIER, VISIT_NUMBER) %>%
    arrange(desc(SP_PCT_BA_LS)) %>%
    mutate(
      lead_si = first(na.omit(si_grd)),
      si_grd = coalesce(si_grd, lead_si),
      si_src = ifelse(is.na(si_src),"LEAD_SPECIES",si_src)
    ) %>%
    ungroup()
}

apply_default_rule <- function(dat){
  
  dat %>%
    mutate(
      si_src = ifelse(is.na(si_grd),"DEFAULT15",si_src),
      si_grd = ifelse(is.na(si_grd),15,si_grd)
    ) %>%
    ungroup()
}

get_conv <- function(target_sp, source_sp, source_si, bec = NULL, use_bec = TRUE) {
  row <- if (use_bec && !is.null(bec) && target_sp %in% c("CW", "FD", "HW")) {
    si_conv_eqn %>%
      filter(s1_sp == target_sp, s2 == source_sp, bec_i_c == bec)
  } else {
    si_conv_eqn %>%
      filter(s1_sp == target_sp, s2 == source_sp)
  }
  
  if (nrow(row) > 0) {
    return(as.numeric(row$b0[1] + row$b1[1] * source_si))  # Take first if multiple rows
  }
  return(NA_real_)
}

process_cluster <- function(dat, src_label = "CONV_EQN") {
  dat <- dat %>% arrange(desc(SP_PCT_BA_LS))
  
  # Fallback logic for 1st row NA
  if (is.na(dat$si_grd[1]) && !is.na(dat$si_grd[2])) {
    source_sp <- dat$SPECIES[2]
    source_si <- dat$si_grd[2]
    target_sp <- dat$SPECIES[1]
    bec_zone <- dat$bec_i_c[1]
    
    conv_si <- get_conv(target_sp, source_sp, source_si, bec_zone)
    
    if (is.na(conv_si) && target_sp %in% names(spec_match)) {
      conv_si <- get_conv(spec_match[[target_sp]], source_sp, source_si, bec_zone)
    }
    if (is.na(conv_si) && source_sp %in% names(spec_match)) {
      alt_s2 <- spec_match[[source_sp]]
      conv_si <- get_conv(target_sp, alt_s2, source_si, bec_zone, use_bec = FALSE)
      if (is.na(conv_si)) conv_si <- get_conv(target_sp, alt_s2, source_si, bec_zone)
    }
    if (is.na(conv_si) && all(c(target_sp, source_sp) %in% names(spec_match))) {
      conv_si <- get_conv(spec_match[[target_sp]], spec_match[[source_sp]], source_si, bec_zone, use_bec = FALSE)
      if (is.na(conv_si)) conv_si <- get_conv(target_sp, spec_match[[source_sp]], source_si, bec_zone)
    }
    
    if (!is.na(conv_si)) {
      dat$si_grd[1] <- conv_si
      dat$si_src[1] <- src_label
    }
  }
  
  # Loop for the rest
  for (j in 2:nrow(dat)) {
    if (is.na(dat$si_grd[j]) && !is.na(dat$si_grd[1])) {
      source_sp <- dat$SPECIES[1]
      source_si <- dat$si_grd[1]
      target_sp <- dat$SPECIES[j]
      bec_zone <- dat$bec_i_c[j]
      
      conv_si <- get_conv(target_sp, source_sp, source_si, bec_zone)
      
      if (is.na(conv_si) && target_sp %in% names(spec_match)) {
        conv_si <- get_conv(spec_match[[target_sp]], source_sp, source_si, bec_zone)
      }
      if (is.na(conv_si) && source_sp %in% names(spec_match)) {
        alt_s2 <- spec_match[[source_sp]]
        conv_si <- get_conv(target_sp, alt_s2, source_si, bec_zone, use_bec = FALSE)
        if (is.na(conv_si)) conv_si <- get_conv(target_sp, alt_s2, source_si, bec_zone)
      }
      if (is.na(conv_si) && all(c(target_sp, source_sp) %in% names(spec_match))) {
        conv_si <- get_conv(spec_match[[target_sp]], spec_match[[source_sp]], source_si, bec_zone, use_bec = FALSE)
        if (is.na(conv_si)) conv_si <- get_conv(target_sp, spec_match[[source_sp]], source_si, bec_zone)
      }
      
      if (!is.na(conv_si)) {
        dat$si_grd[j] <- conv_si
        dat$si_src[j] <- src_label
      }
    }
  }
  return(dat)
}
