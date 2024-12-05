## Server logic for Site Index tab ----

###############################################.
## Indicator definitions ----
###############################################.
#Subsetting by domain 

output$site_index_pspl <- renderUI({
  
  HTML("Site index (SI) is an estimate of a stand's potential productivity by species. 
Predicted SI is from the Provincial site productivity layer (PSPL v7.0) that uses 
either ecological mapping plus SIBEC tables or SI biophysical model where no 
ecological mapping is available. YSM ground SI is from up to four site trees 
per species. The average SI and ratio of means (ROM) between ground and 
predicted SI are compared at the time of latest YSM measurement (table 
below). For those species with a least 10 paired observations, a region of 
practical equivalence (ROPE) assesses for practical SI differences, defined 
here as 5% ROPE (ie., ratio limits of 0.95 to 1.05). A practical difference (Y) is 
when the ROM confidence interval (CI) is entirely outside the ROPE, no 
practical difference (N) is when the ROM CI is entirely within the ROPE; all 
other situations are inconclusive (I).")
  
})


#si_dat <- reactive({
#  
#  req(input$SelectCategory, input$SelectVar)
#  input$genearate
#  
#  si_dat <- SI_data %>%
#    filter(CLSTR_ID %in% clstr_id_all(), !is.na(ratio)) %>%
#    group_by(SITE_IDENTIFIER, SPECIES) %>%
#    slice(which.min(yrs_from_50))
#  
#  si_dat1 <- si_dat %>%
#    group_by(SPECIES) %>%
#    summarise(n = n(),
#              age_avg = mean(AGEB_TLSO),
#              grd_avg = mean(SI_M_TLSO),
#              prd_avg = mean(pspl_si))
#  
#  si_dat1 <- si_dat1 %>%
#    ungroup() %>%
#    mutate(ROM = grd_avg/prd_avg)
#  
#  si_dat2 <- si_dat %>%
#    left_join(si_dat1, , by = "SPECIES")
#  
#  # *compute additional attribute to compute variance;
#  si_dat2 <- si_dat2 %>%
#    mutate(diffs_sqrd = (SI_M_TLSO - (ROM * pspl_si))**2)
#  
#  # *sum diffs squared;
#  si_dat3 <- si_dat2 %>%
#    group_by(SPECIES) %>%
#    summarise(sum_diffs_sqrd = sum(diffs_sqrd))
#  
#  # *merge differences squared back to sample by species detail;
#  si_dat4 <- si_dat2 %>%
#    left_join(si_dat3, by = "SPECIES")
#  
#  # *compute variance and confidence interval, and test for significiant ratio different from 1.0;
#  # *arbitrary set of miniumum number of obs per species to be 10;
#  si_dat4 <- si_dat4 %>%
#    rowwise() %>%
#    mutate(var_rom = ifelse(n >=2, sum_diffs_sqrd / (n * (n-1) * prd_avg**2), NA),
#           l95_rom  = ifelse(n >=2, ROM - qt(0.975,n-1) * sqrt(var_rom), NA),
#           u95_rom  = ifelse(n >=2, ROM + qt(0.975,n-1) * sqrt(var_rom), NA),
#           # *standard signficance test;
#           sig_rom = ifelse(!is.na(l95_rom) & l95_rom < 1.0 & !is.na(u95_rom) & u95_rom > 1.0,
#                            "N", "Y"),
#           # *following review by p.ott 2021mar, recommend to use ROPE TO DEFINE ZONE OF PRACTICAL SIGNIFICANCE;
#           # *suggested to use 0.95 to 1.05 range of ratio to establish zone, rationale is that 5% rom is a 1m SI difference from a known;
#           # *SI of 20m, and results in a 10% change in culmination of MAI for a PL MSYT using TIPSY 4.4;
#           
#           # *95% confidence interval completely outside rope;
#           sig_rope = case_when((!is.na(l95_rom) & l95_rom > 1.05) | (!is.na(u95_rom) & u95_rom < 0.95) ~ "Y",
#                                # *95% confidence interval completely inside rope;
#                                (!is.na(l95_rom) & l95_rom > 0.95) & (!is.na(u95_rom) & u95_rom < 1.05) ~ "N",
#                                # *one 95% confidence limit inside rope, but the other outside rope, then inconclusive;
#                                (!is.na(l95_rom) & l95_rom < 1.05) & (!is.na(u95_rom) & u95_rom > 1.05) ~ "I",
#                                (!is.na(l95_rom) & l95_rom < 0.95) & (!is.na(u95_rom) & u95_rom > 0.95) ~ "I",
#                                # *both 95% confidence limits outside rope, then inconclusive;
#                                (!is.na(l95_rom) & l95_rom < 0.95) & (!is.na(u95_rom) & u95_rom > 1.05) ~ "I"
#           )
#    )
#  
#  si_dat5 <- si_dat4 %>%
#    ungroup() %>%
#    select(SPECIES, n, age_avg, grd_avg, prd_avg, ROM, 
#           l95_rom, u95_rom, sig_rope) %>%
#    distinct() %>%
#    mutate(age_avg = round(age_avg, 0),
#           grd_avg = round(grd_avg, 1),
#           prd_avg = round(prd_avg, 1),
#           ROM = round(ROM, 2),
#           l95_rom = round(l95_rom, 2),
#           u95_rom = round(u95_rom, 2),
#           l95_rom = ifelse(n >= 10, l95_rom, "-"),
#           u95_rom = ifelse(n >= 10, u95_rom, "-"),
#           sig_rope = ifelse(n >= 10, sig_rope, "-")) %>%
#    arrange(SPECIES)
#  
#  return(si_dat5)
#  
#})
#
#
#si_bias <- reactive({
#  
#  req(input$SelectCategory, input$SelectVar)
#  input$genearate
#  
#  si_dat <- si_dat()
#  
#  si_bias <- if (sum(si_dat$sig_rope == "Y") > 0){
#    
#    si_dat %>%
#    filter(sig_rope == "Y") %>%
#    mutate(bias = paste0(SPECIES, "(",  round((grd_avg-prd_avg)/prd_avg*100, 0), "%)")) %>%
#    pull(bias)
#    
#  } else NULL
#  
#  si_bias1 <- ifelse(is.null(si_bias), "None", paste(si_bias, collapse=" "))
#  
#  return(si_bias1)
#  
#})


output$si_pspl_flex <- renderUI({
  
  si_dat <- si_dat()
  
  si_flex <- flextable(si_dat) %>% 
    bold(j = c(4,5,9), bold = TRUE, part = "all") %>%
    color(i = ~ sig_rope == "Y", j = 9, color = 'red', part = "body") %>%
    set_header_labels(values = c("Spc", "n", "BHAge", "YSM", "PSPL", "ROM", "L95%", "U95%", "test")) %>% 
    add_header_row(top = TRUE, values = c(" ", "#Pairs", "YSM", "SI (m)", "Ratio of Means", "ROPE"), 
                   colwidths = c(1,1,1,2,3,1)) %>%
    align(align = "center", part = "all") %>%
    merge_v(part = "header") 
  
  return(si_flex %>%
           htmltools_value())   
})


output$trend_si <- renderUI({
  
  HTML("The average site index vs. breast height age from all valid site trees 
       is plotted for each species and sample measurement (graph below; joined 
       dots are for repeated measurements). Subsequent measurements may not 
       always include the same site trees (due to changes in site tree suitability, 
       or height by DBH crossovers), site index and breast height age may also 
       change over time. The most reliable site index estimate for a given 
       species and YSM plot is the measurement closest to breast height age 50. 
       The number above each line denotes the number of plots used to average the site index.")
  
})



output$si_trend <- renderPlot({
  
  si_dat <- si_dat()
  
  fig7_dat <- SI_data %>%
    filter(CLSTR_ID %in% clstr_id_all(), SPECIES %in% si_dat$SPECIES) %>%
    mutate(meas_no = ifelse(CLSTR_ID %in% clstr_id(), 2, 1)) %>%
    filter(!is.na(meansi), !is.na(meanage)) 
  
  fig7_dat <- fig7_dat %>%
    group_by(SPECIES, meas_no) %>%
    summarize(mean_agebh = mean(meanage, na.rm = T),
              mean_si = mean(meansi, na.rm = T),
              nobs = n()) %>%
    ungroup() 
  
  fig7_lab <- fig7_dat %>% 
    group_by(SPECIES) %>%
    summarize(mean_agebh = mean(mean_agebh, na.rm = T),
              mean_si = mean(mean_si, na.rm = T),
              nobs = min(nobs))
  
  p <- ggplot(fig7_dat, 
                 aes(x = mean_agebh, y = mean_si, 
                     group =SPECIES, color = SPECIES)) + 
    geom_point(size = 4) + geom_line(linewidth = 1.5)  +
    scale_color_brewer(name = "", palette = "Set2") +
    geom_text(data = fig7_lab, aes(label = nobs, col = SPECIES),
              vjust = -1, 
              position = position_dodge(0.9), show.legend = FALSE) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, max(round(fig7_dat$mean_si+5,-1))),
                       breaks = seq(0, max(round(fig7_dat$mean_si+5,-1)), by = 10),
                       minor_breaks = seq(0, max(round(fig7_dat$mean_si+5,-1)), by = 2)) + 
    scale_x_continuous(expand = c(0, 0), limits = c(0, max(fig7_dat$mean_agebh)+5)) +
    labs(x = "Breast Height Age (yrs)", y = "Ground Site Index (m)") +
    #theme_bw() + 
    theme(axis.line = element_line(colour="darkgray"), 
          panel.grid.major.y = element_line(color = 'darkgray'), 
          #panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          rect = element_blank()
    )  
  p
  
})
