## Server logic for Leading Species tab ----

###############################################.
## Indicator definitions ----
###############################################.
#Subsetting by domain 

#LD_dat <- reactive({
#  
#  req(input$SelectCategory, input$SelectVar)
#  input$genearate
#  
#  LD_dat <- spcs_data %>%
#    filter(CLSTR_ID %in% clstr_id(), UTIL == 4) %>%
#    group_by(SITE_IDENTIFIER) %>%
#    arrange(desc(SP_PCT_BA_LS)) %>%
#    slice_head(n = 1)%>%
#    select(SITE_IDENTIFIER, CLSTR_ID, SPECIES, SP_PCT_BA_LS)
#  
#  LD_dat <- LD_dat %>%
#    left_join(vegcomp_pspl_sample %>% select(CLSTR_ID, SPECIES_CD_1, SPECIES_PCT_1), 
#              by = "CLSTR_ID") %>%
#    mutate(SPC_GRP1 = substr(SPECIES,1,2),
#           SPC_GRP1 = ifelse(SPECIES %in% decidspc, 'Decid', SPC_GRP1),
#           SPC_GRP1 = ifelse(SPC_GRP1 =="", 'Nonstock', SPC_GRP1),
#           SPC_GRP_VRI = substr(SPECIES_CD_1,1,2),
#           SPC_GRP_VRI = ifelse(SPECIES_CD_1 %in% decidspc, 'Decid', SPC_GRP_VRI)) 
#  
#  return(LD_dat)
#  
#})
#
#
#correct_ls <- reactive({
#  
#  req(input$SelectCategory, input$SelectVar)
#  input$genearate
#  
#  LD_dat <- LD_dat()
#  
#  correct <- round(sum(LD_dat$SPC_GRP1 == LD_dat$SPC_GRP_VRI)/nrow(LD_dat)*100, 0)
#  
#  return(correct)
#  
#})
#
#
#Fig11_dat <- reactive({
#  
#  req(input$SelectCategory, input$SelectVar)
#  input$genearate
#  
#  ysm_spc <- spcs_data %>% 
#    filter(CLSTR_ID %in% clstr_id(), UTIL == 4, !(SPECIES %in% c('', NA)))
#  
#  ### Add species percent information to sample_data
#  ysm_spc1 <- sample_data %>%
#    filter(CLSTR_ID %in% clstr_id()) %>%
#    left_join(ysm_spc, by = c("CLSTR_ID", "SITE_IDENTIFIER", "VISIT_NUMBER" )) %>%
#    filter(!is.na(SITE_IDENTIFIER))
#  
#  ### Compute number plot measurements by MGMT_UNIT
#  ysm_spc2 <- ysm_spc1 %>%
#    mutate(n_ci = length(unique(CLSTR_ID)),
#           SPC_GRP1 = ifelse(SPECIES %in% decidspc, 'DE', SPECIES))
#  
#  ### Compute mean stems per ha by species
#  ysm_spc3 <- ysm_spc2 %>%
#    group_by(SPC_GRP1) %>%
#    reframe(mean_STEMS_HA = sum(STEMS_HA_LS, na.rm = T)/n_ci) %>%
#    distinct()
#  
#  ysm_spc4 <- ysm_spc3 %>%
#    mutate(tot_STEMS_HA = sum(mean_STEMS_HA, na.rm = T),
#           spcperc = mean_STEMS_HA/tot_STEMS_HA)
#  
#  ysm_spc6 <- ysm_spc4 %>%
#    arrange(desc(spcperc)) %>%
#    mutate(order = row_number(),
#           SPC_GRP2 = ifelse(order <= 7, SPC_GRP1, 'Other'),
#           SPC_GRP2 = ifelse(SPC_GRP2 == 'DE', 'Decid', SPC_GRP2))
#  
#  regen_natural <- regen_data %>%
#    filter(CLSTR_ID %in% clstr_id(), regen_src == "N", SPECIES_recode != "") %>%
#    mutate(n_si = length(unique(CLSTR_ID)),
#           n_ft = length(unique(FEATURE_ID)))
#  
#  regen_natural <- regen_natural %>%
#    group_by(SPC_GRP1) %>%
#    reframe(mean_DENSITY = sum(DENSITY, na.rm = T)/n_ft) %>%
#    distinct()
#  
#  regen_natural <- regen_natural %>%
#    mutate(tot_DENSITY = sum(mean_DENSITY, na.rm = T),
#           spcperc = mean_DENSITY/tot_DENSITY) %>%
#    arrange(desc(spcperc)) %>%
#    mutate(order = row_number(),
#           SPC_GRP2 = ifelse(order <= 7, SPC_GRP1, 'Other'),
#           SPC_GRP2 = ifelse(SPC_GRP2 == 'DE', 'Decid', SPC_GRP2))
#  
#  ### Data for figure
#  ysm_spc_dat <- ysm_spc6 %>% 
#    mutate(source="YSM") %>%
#    select(SPC_GRP1, spcperc, order, SPC_GRP2, source)
#  
#  regen_natural <- regen_natural %>%
#    mutate(source="TSR_INPUT") %>%
#    select(SPC_GRP1, spcperc, order, SPC_GRP2, source)
#  
#  Fig11_dat <- rbind(ysm_spc_dat, regen_natural) 
#  
#  return(Fig11_dat)
#  
#})
#
#percoverlap <- reactive({
#  
#  req(input$SelectCategory, input$SelectVar)
#  input$genearate
#  
#  Fig11_dat <- Fig11_dat()
#  
#  percoverlap <- Fig11_dat %>%
#  #filter(MGMT_UNIT %in% mgmt_unit) %>%
#  select(-order) %>%
#  pivot_wider(names_from = source,
#              names_sep = ".",
#              values_from = c(spcperc)) %>%
#  mutate(diff = abs(YSM - TSR_INPUT)) %>%
#  summarize(perc_sum = sum(diff, na.rm = T)) %>%
#  mutate(perc_over = 1- perc_sum) %>%
#  pull(perc_over)
#  
#  return(percoverlap)
#  
#})


output$leading_sp <- renderUI({
  
  HTML("Leading species (by basal area) is compared between YSM & VRI where the 
       <i>‘correct leading species classification rate’</i> is a percent of all YSM samples 
       with matching inventory leading species at latest measurement (table below). 
       A separate assessment of overall species composition is also compared 
       between YSM and TSR modeled regeneration inputs (figure below). 
       Overall species composition overlap is a rough index, and expressed as 
       the ratio between the minimum in common relative to the maximum in common 
       that could have been. The planted & natural densities used in TSR inputs 
       are combined as part of the overall species composition comparison.")

})


output$leading_sp_flex <- renderUI({
  
  if(!is.null(clstr_id())){
    
    LD_dat <- LD_dat()
    
    LD_table <- proc_freq(LD_dat, "SPC_GRP_VRI", "SPC_GRP1",
                          #main = "Leading species cross-table comparison (sum of the # of YSM plots)",
                          include.row_total = T,
                          include.row_percent = F,
                          include.column_total = T,
                          include.column_percent = F,
                          include.table_percent = F) 
    
    LD_table <- set_caption(LD_table, caption = as_paragraph(
      as_chunk("Leading species cross-table comparison (sum of the # of YSM plots)", 
               props = fp_text_default(bold = TRUE))),
      align_with_table = FALSE,
      word_stylename = "Table Caption") %>%
      add_footer_lines(value = as_paragraph(paste0("Correct Leading Species Classification Rate = ", correct_ls(), "%")))
    
    LD_table <- labelizor(x = LD_table, 
                          part = "header", 
                          labels = c("SPC_GRP_VRI" = "VRI", 
                                     "SPC_GRP1" = "YSM"))  %>%
      align(align = "left", part = "header") %>%
      autofit()
    
    return(LD_table %>%
             htmltools_value())      
  }
  
})

output$spc_comp <- renderPlot({
  
  Fig11_dat <- Fig11_dat()
  
  percoverlap <- percoverlap()
  
  p <- Fig11_dat %>%
    filter(spcperc >=0.001) %>%
    ggplot(aes(x=SPC_GRP1, y=spcperc, fill=source)) + 
    geom_bar(stat="identity", position = position_dodge2(preserve = "single"), width=0.7) +
    scale_fill_manual(values = c("#B4464B", "steelblue"), name = NULL, labels = c("TSR-INPUT", "YSM")) +
    scale_x_discrete(drop=FALSE) +
    #scale_y_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
    labs(x = "Species", y = "% of total Stems/ha",
         title = "Overall Species Composition Comparison",
         caption=paste0("Overall Species Composition Overlap = ", round(percoverlap*100, 0), "%")) +
    #theme_bw() + 
    theme(
      #axis.line = element_line(colour="darkgray"), 
      panel.grid.major.y = element_line(color = 'darkgray'), 
      plot.caption = element_text(hjust=0, size=rel(1.2)),
      legend.position="top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      rect = element_blank()
    ) 
  
  p
})
