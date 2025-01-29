## Server logic for Forest Health tab ----

###############################################.
## Indicator definitions ----
###############################################.

coctext <- reactive({
  req(input$SelectCategory, input$SelectVar)
  Fig15_dat <- Fig15_dat()
  
  coctext <- HTML(paste0("<p>Growth and mortality of the n=", "<b>", total_remeas_plot(), 
               "</b>", " re-measured YSM ground samples are summarized into components of change for
all tagged trees, separately for each measurement period. </p>", 
               "<p>The components of change across only the last two measurements are shown 
  (figure below), representing an average of ", round(mean(Fig15_dat$year_dff),0),
               " years. The components of change include:</p>",
               "</br><ul><li><b><i>Survivor</i></b> Trees that are alive at both measurements </li>",
               "<li><b><i>Mortality</i></b> Trees that died between measurements </li>",
               "<li><b><i>Ingrowth</i></b> New trees that grow into the minimum tagging limit </li>",
               "<li><b><i>Dead</i></b> Trees that are dead standing at both measurements </li></br>"))
  return(coctext)
})



output$quant_coc <- renderUI({
  
  coctext()
  
})

cocplot <- reactive({
  req(input$SelectCategory, input$SelectVar)
  fig8_dat <- fig8_dat()
  remeas_plot <- remeas_plot()
  total_remeas_plot <- total_remeas_plot()
  
  fig8 <- fig8_dat %>%
    filter(CLSTR_ID %in% remeas_plot) %>%
    mutate(baha = BA_TREE*phf_coc) %>%
    group_by(comp_chg_coc) %>%
    summarize(BA = sum(baha, na.rm = T)/total_remeas_plot,
              stem = sum(phf_coc, na.rm = T)/total_remeas_plot) %>%
    data.table
  
  ratio = round(max(fig8$BA)/max(fig8$stem), 2)
  
  fig8 <- melt(fig8, measure.vars = c( "stem","BA"),
               variable.name = "variable", value.name = "value")
  
  fig8 <- fig8 %>%
    mutate(value_adj = ifelse(variable == "BA", value/ratio, value))
  
  p <- if (nrow(fig8) > 1){ ggplot(fig8, aes(x = comp_chg_coc)) +
      geom_bar(aes(y = value_adj, fill = variable, group = variable),
               stat = "identity", position = position_dodge2(), width = 0.7)  +
      labs(x = "", title = "Components of Change") + 
      scale_fill_manual(values = c("steelblue", "#B4464B"), name = NULL, labels = c("Stems/ha", "BA/ha")) +
      #scale_fill_discrete(name = "", labels = c("Stems/ha", "BA/ha")) +
      scale_x_discrete("", labels = c("D" = "Dead", "I" = "Ingrowth", "M" = "Mortality", "S" = "Survivor")) +
      scale_y_continuous(name = "Stems (#/ha)", expand = c(0, 0), 
                         limits = c(0,max(fig8$value_adj)*1.1), 
                         sec.axis = sec_axis( trans=~.*ratio, 
                                              name=expression("Basal Area ("~m^{2}~"/ha)"))) +
      geom_text(mapping = aes(label = round(value, 1), 
                              x = comp_chg_coc, y = value_adj+max(value_adj)*0.05, 
                              group = variable,
                              colour = variable), 
                position = position_dodge(width = .9), show.legend  = FALSE) + 
      scale_color_manual(values = c("steelblue", "#B4464B")) +
      #theme_bw() + 
      theme(
        axis.title.y = element_text(color="steelblue"),
        axis.title.y.right = element_text(colour = "#B4464B"),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.position = c(0.15, 0.85),
        legend.title = element_blank(),
        axis.line = element_line(colour="darkgray"), 
        panel.grid.major.y = element_line(color = 'darkgray'), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        rect = element_blank()
      ) 
  } else {
    ggplot() + 
      theme_void() +
      geom_text(aes(0,0,label='N/A')) +
      xlab(NULL)
  }
  
  return(p)
})


output$coc_chart <- renderPlot({
  
  cocplot()
})


curfhtext <- reactive({
  req(input$SelectCategory, input$SelectVar)
  curfhtext <- HTML("All tagged trees are assessed for up to five forest health damage agents
per tree. The mean incidence and 95% confidence intervals by damage
agent (expressed as a percent of total live stems/ha of all damage
agents recorded per tree) are computed at the latest measurement. Note
that up to the 2020 field season, all fork and crook occurrences were
recorded regardless of size and severity. Starting in the 2021 field
season, those forks and crooks with very small (<10%) offsets in stem
diameter (expected to have negligible/no impact on stem form) are no
longer recorded; therefore, earlier recorded incidences of forks and
crooks prior to 2021 were likely over-estimated. In addition fork and
crook severity is now further classified into minor (<50%) vs. major
(>=50%) diameter offsets. A full list of recorded damage agents is under
General Notes.")
  return(curfhtext)
})


output$health_inci <- renderUI({
  
  curfhtext()
  
})


curfhplot <- reactive({
  req(input$SelectCategory, input$SelectVar)
  # *compute incidence percent by agent by sample;
  ### Total stems/ha within a plot & average stems/ha of all plots
  FH_dat <- tree_fh_data  %>%
    filter(CLSTR_ID %in% clstr_id(), S_F == "S", RESIDUAL != "Y") 
  
  FH_dat1 <- FH_dat  %>%
    filter(DAM_NUM == 1)  %>%
    group_by(CLSTR_ID, n, LV_D) %>%
    summarize(tot_ba_plot = sum(ba_ha, na.rm = T),
              tot_stems_plot = sum(PHF_TREE, na.rm = T),
              tot_vol_plot = sum(vol_ha, na.rm = T),
              n_tree = n()) %>%
    group_by(n, LV_D) %>%
    mutate(avg_ba_allplot = sum(tot_ba_plot)/n,
           avg_stems_allplot = sum(tot_stems_plot)/n,
           avg_vol_allplot = sum(tot_vol_plot)/n)
  
  #*totals based on un expanded tree list (ie., sum of percent incidence can be 
  #*over 100 since multiple damage agents per tree);
  ### Total stems/ha of a damage agent in a plot
  FH_dat2 <- FH_dat %>%
    group_by(CLSTR_ID, n, LV_D, AGN) %>%
    summarize(tot_ba_dam_plot = sum(ba_ha, na.rm = T),
              tot_stems_dam_plot = sum(PHF_TREE, na.rm = T),
              tot_vol_dam_plot = sum(vol_ha, na.rm = T),
              n_agn = n())   ### n_agn: # of each damage within a plot
  
  ### Proportion of damaged stems within a plot
  FH_dat3 <- FH_dat1 %>%
    full_join(FH_dat2, by = c("CLSTR_ID", "n", "LV_D" )) %>%
    mutate(incid_dam_ba_plot = tot_ba_dam_plot/tot_ba_plot,
           incid_dam_stems_plot = tot_stems_dam_plot/tot_stems_plot,
           incid_dam_vol_plot = tot_vol_dam_plot/tot_vol_plot)
  
  ### Total stems/ha of a damage agent across plots
  FH_dat4 <- FH_dat3 %>%
    group_by(n, LV_D, AGN) %>%
    summarize(tot_ba_dam_allplot = sum(tot_ba_dam_plot),
              tot_stems_dam_allplot = sum(tot_stems_dam_plot),
              tot_vol_dam_allplot = sum(tot_vol_dam_plot),
              n_agn_plot = n()) %>%    #n_agn_all: # plots that has the damage agent
    mutate(avg_ba_dam_allplot = tot_ba_dam_allplot/n,
           avg_stems_dam_allplot = tot_stems_dam_allplot/n,
           avg_vol_dam_allplot = tot_vol_dam_allplot/n)
  
  FH_dat5 <- FH_dat4 %>%
    left_join(FH_dat3 %>% ungroup() %>% distinct(n, LV_D, avg_ba_allplot, avg_stems_allplot, avg_vol_allplot), 
              by = c("n", "LV_D")) %>%
    mutate(incid_stems_allplot = avg_stems_dam_allplot/avg_stems_allplot,
           incid_vol_allplot = avg_vol_dam_allplot/avg_vol_allplot)
  
  FH_dat6 <- FH_dat3 %>%
    left_join(FH_dat5, by = c("n", "LV_D", "AGN", "avg_ba_allplot", "avg_stems_allplot"))
  
  FH_dat6 <- FH_dat6 %>%
    mutate(incid_diffs_sqrd = (tot_stems_dam_plot - avg_stems_dam_allplot)^2)
  
  FH_dat7 <- FH_dat6 %>%
    group_by(n, LV_D, AGN) %>%
    summarize(sum_incid_diffs_sqrd = sum(incid_diffs_sqrd))
  
  FH_dat8 <- FH_dat5 %>%
    left_join(FH_dat7, by = c("n", "LV_D", "AGN")) %>%
    mutate(var_stems_incid = sum_incid_diffs_sqrd / (n*(n-1)*avg_stems_allplot^2),
           l95_stems = ifelse(n >1, incid_stems_allplot - qt(0.975, n-1) * sqrt(var_stems_incid), NA),
           u95_stems = ifelse(n >1, incid_stems_allplot + qt(0.975, n-1) * sqrt(var_stems_incid), NA))
  
  FH_dat8 <- FH_dat8 %>%
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
    mutate(dam_class = fct_reorder(dam_class, -incid_stems_allplot))
  
  FH_dat_final <- FH_dat8 %>% ungroup()
  
  p <- ggplot(FH_dat_final %>% filter(LV_D == "L", AGN != "O")) + 
    geom_bar(stat = "identity", aes(x = AGN, y = incid_stems_allplot, fill = dam_class)) + 
    geom_errorbar(aes(x = AGN, ymin = l95_stems, ymax = u95_stems), width = .2,
                  position = position_dodge(.9)) +
    geom_linerange(aes(x = AGN, ymin = incid_stems_allplot, ymax = u95_stems)) +
    scale_x_discrete(drop = FALSE) +
    scale_fill_brewer(name = NULL, palette = "Set2") +
    scale_y_continuous(labels = scales::percent, expand = c(0, 0),
                       limits = c(0, max(round(FH_dat_final[FH_dat_final$AGN!="O" & 
                                                              FH_dat_final$LV_D == "L",]$u95_stems+0.05,1)))) + 
    facet_grid(. ~ reorder(dam_class, -incid_stems_allplot, min), scales="free_x", space="free_x") +
    labs(x = "", y = "Incidence (%)",
         title = "Current Incidence",  
         subtitle = "(% of total live stems/ha of up to 5 damage agents per tree)") +
    #theme_bw() + 
    theme(
      axis.line = element_line(colour="darkgray"), 
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      panel.grid.major.y = element_line(color = 'darkgray'), 
      #panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      rect = element_blank(),
      legend.box.background = element_rect(fill = "white", color = "lightgray"),
      legend.position = "none",
      legend.title = element_blank(),
      plot.caption = element_text(hjust=0, size=rel(1.2))
    )  
  
  return(p)
})



output$curr_fh_inci <- renderPlot({
  
  curfhplot()
})


fhcoctext1 <- reactive({
  req(input$SelectCategory, input$SelectVar)
  Fig15_dat <- Fig15_dat()
  
  fhcoctext1 <- HTML(paste0("Change in forest health incidence of the (n=","<b>",
              total_remeas_plot(),"</b>",") re-measured YSM samples are compared across 
  the last measurement period averaging ", "<b>", 
              round(mean(Fig15_dat$year_dff),0), "</b>", " years. Incidence across both 
  measurements is relative to the primary damage agent (first recorded and 
  most significant) on the same trees that were alive at the beginning of the 
  period (graph below)."))
  return(fhcoctext1)
})


output$comp_coc <- renderUI({
  
  fhcoctext1()
  
})


fhcocplot <- reactive({
  req(input$SelectCategory, input$SelectVar)
  fig10_dat_final <- fig10_dat_final()
  
  p <- if (nrow(fig10_dat_final) > 0) {
    ggplot(fig10_dat_final %>% filter(!(AGN %in% c("O", "")))) + 
      geom_bar(stat = "identity", aes(x = AGN, y = incid_stems, fill = factor(new_visit_number)), 
               position = position_dodge(), width=0.7) + 
      scale_fill_manual(values = c("steelblue", "#B4464B"), name = NULL, 
                        labels = c("Initial Msmt", "Latest Msmt")) +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(labels = scales::percent, expand = c(0, 0),
                         limits = c(0, ceiling(max(fig10_dat_final[fig10_dat_final$AGN!="O",]$incid_stems)*20) / 20),
                         minor_breaks = seq(0, 
                                            ceiling(max(fig10_dat_final[fig10_dat_final$AGN!="O",]$incid_stems)*20) / 20, 
                                            by = 0.01)) + 
      facet_grid(. ~ reorder(dam_class, -incid_stems, min), scales="free_x", space="free_x") +
      labs(x = "", y = "Incidence (%)",
           title = "Change in primary damage agent incidence",
           subtitle = "(% of live trees)") +
      theme(
        axis.line = element_line(colour="darkgray"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.y = element_line(color = 'darkgray'), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        rect = element_blank(),
        legend.box.background = element_rect(fill = "white", color = "lightgray"),
        legend.position = c(0.9, 0.8),
        legend.title = element_blank(),
        plot.caption = element_text(hjust=0, size=rel(1.2))
      )    
  } else {
    ggplot() + 
      theme_void() +
      geom_text(aes(0,0,label='N/A')) +
      xlab(NULL)
  }
  
  return(p)
})




output$change_dam <- renderPlot({
  
  fhcocplot()
})

fhcoctext2 <- reactive({
  req(input$SelectCategory, input$SelectVar)
  
  fhcoctext2 <- HTML("All trees that were alive at the beginning of the period, have either
survived or died between measurements. The table below summarizes
the components of change for those trees affected by 
primary damage agents with highest percent incidence. In addition, the 
probability of trees getting infected by a given damage agent and subsequently 
dying from it, is also calculated.")
  return(fhcoctext2)
})



output$fh_trees <- renderUI({
  
  fhcoctext2()
  
})


fhcocflex <- reactive({
  req(input$SelectCategory, input$SelectVar)
  fig10_dat_final <- fig10_dat_final()
  
  if (nrow(fig10_dat_final) > 0){
    tot_tree_alive <- round(unique(fig10_dat_final[fig10_dat_final$new_visit_number=='Last',]$totsph_comdem),0)
    
    table6_dat <- fig10_dat_final %>%
      arrange(desc(incid_stems)) %>%
      filter(new_visit_number == 'Last') %>%
      mutate(rank = row_number()) %>%
      mutate(rank_new = ifelse(rank>15, 16, rank),
             AGN_rank = ifelse(rank>15, "Rest", AGN)) %>%
      group_by(AGN_rank, rank_new) %>%
      summarise(incid_stems = sum(incid_stems, na.rm = T)*100,
                S.dam = sum(S.dam, na.rm = T),
                M.dam = sum(M.dam, na.rm = T),
                damsph_comdem = sum(damsph_comdem, na.rm = T),
                perc_mort = mean(perc_mort, na.rm = T)*100,
                prob_get_and_die = sum(prob_get_and_die, na.rm = T)*100) %>%
      ungroup() %>%
      arrange(rank_new) %>%
      mutate(total = "", 
             PDA = AGN_rank,
             Inci = round(incid_stems, 1),
             S = round(S.dam, 0),
             M = round(M.dam, 0), 
             Tot = round(damsph_comdem, 0),
             PM = round(perc_mort, 1),
             Prob = round(prob_get_and_die, 1)) %>%
      select(total, PDA, Inci, S, M, Tot, PM, Prob) %>%
      mutate(across(everything(), .fns = function(x) ifelse(x == 0, "", x)))
      #slice_max(incid_stems, n =15) %>%
      #mutate(total = "", 
      #       PDA = AGN,
      #       Inci = round(incid_stems*100, 1),
      #       S = round(S.dam, 0),
      #       M = round(M.dam, 0), 
      #       Tot = round(damsph_comdem, 0),
      #       PM = round(perc_mort*100, 1),
      #       Prob = round(prob_get_and_die*100, 1)) %>%
      #ungroup() %>%
      #select(total, PDA, Inci, S, M, Tot, PM, Prob) %>%
      #mutate(across(everything(), .fns = function(x) ifelse(x == 0, "", x)))
    
    flextable3 <- flextable(table6_dat) 
    
    flextable3 <- add_header_row(flextable3, top = TRUE, colwidths = c(3,4,1),
                                 values = c("", "Number of Affected Trees by Primary Damage Agent", "")) %>%
      align(align = "center", part = "all") %>%
      merge_v(j = c(1:3,8), part = "header") 
    
    flextable3 <- labelizor(
      x = flextable3, 
      part = "header", 
      labels = c("total" = 'Live trees at\n period start\n [a]\n (#/ha)', 
                 "PDA" = "Primary\n Damage\n Agent",
                 "Inci" = "Incidence\n [b]=e/a d\n (%)",
                 "S" = "Survivor trees\n [c]\n (#/ha)",
                 "M" = "Mortality trees\n [d]\n (#/ha)",
                 "Tot" = "Total affected\n [e]=c+d (#/ha)",
                 "PM" = "Mortality\n [f]=d/e\n (%)",
                 "Prob" = "Prob. getting\n Infected &\n Dying [g]=b*f\n (%)")) %>%
      autofit()
    
    flextable3 <- merge_v(flextable3, j = 1:2, part = "header") 
    
    flextable3 <- flextable::compose(flextable3,
                                     i = 1, j = 1, 
                                     value = as_paragraph(round(tot_tree_alive, 0)))
    
  } else {
    flextable3 <- flextable(data.frame(matrix(rep("-", 8), ncol=8,nrow=1)))
    flextable3 <- add_header_row(flextable3, top = TRUE, colwidths = c(3,4,1),
                                 values = c("", "Number of Affected Trees by Primary Damage Agent", "")) %>%
      align(align = "center", part = "all")
    
    flextable3 <- labelizor(
      x = flextable3, 
      part = "header", 
      labels = c("X1" = 'Live trees at\n period start\n [a]\n (#/ha)', 
                 "X2" = "Primary\n Damage\n Agent",
                 "X3" = "Incidence\n [b]=e/a d\n (%)",
                 "X4" = "Survivor trees\n [c]\n (#/ha)",
                 "X5" = "Mortality trees\n [d]\n (#/ha)",
                 "X6" = "Total affected\n [e]=c+d (#/ha)",
                 "X7" = "Mortality\n [f]=d/e\n (%)",
                 "X8" = "Prob. getting\n Infected &\n Dying [g]=b*f\n (%)")) %>%
      autofit()
    
    flextable3 <- merge_v(flextable3, j = 1:2, part = "header") 
    
  }
  
  return(flextable3)   
})



output$fh_trees_flex <- renderUI({
  
  htmltools_value(fhcocflex())
  
})


fufhtext <- reactive({
  req(input$SelectCategory, input$SelectVar)
  
  year100_immed <- year100_immed()
  year100_inc <- year100_inc()
  year100_comb <- year100_comb()
  
  max_measyear <- max_measyear()
  stemrustimpact <- stemrustimpact()
  
  phrase <- ifelse(max_measyear >= 2017, 
                   paste0("The GRIM/CRIME forest health modules (affecting DSC,DSG,DSS) resulted in a <b>",
                          stemrustimpact %>% tail(1) %>% pull(rustimpact), "</b> volume impact on YSM TASS 
         projections by age <b>", stemrustimpact %>% tail(1) %>% pull(AGE), "</b>. In addition, 
         the interim forest health factors resulted in a further ","<b>", year100_comb, 
                          "</b>","% volume impact on YSM TASS projections by age 100."),
                   paste0("The GRIM/CRIME forest health modules could not be included since the 
         latest sample visit was completed prior to 2017.  Therefore, interim 
         forest health factors also account for (DSC,DSG,DSS) stem rusts and 
         resulted in an additional <b>", year100_comb, " %</b> volume impact on YSM 
         TASS projections by age 100."))
  
  
  fufhtext <- HTML(paste0("<p>Forest health impacts are currently modeled in TASS using 
  the forest health modules GRIM & CRIME that quantify volume impacts of 
  a specific group of stem rusts (DSC, DSG, DSS). YSM sample measurements 
  collected since 2017 include the necessary tree detail information to 
  run these modules. However, to address forest health risks from all other 
  damage agents (and for those sample measurements collected before 2017), 
  an interim simplistic approach is applied to estimate future impacts of 
  all known forest health agents. This involves creating two groups of 
  damage agents: 1) those expected to result in immediate mortality vs. 
  2) those causing incremental mortality or growth loss through to rotation. 
  For the first group, 90% of the current measured incidence is assumed to 
  cause immediate mortality (left graph). For the second group, the impact 
  is modeled as a product of the current measured incidence times a mortality 
  rate of 2.5% volume loss per decade (right graph).
  Their combined impact is applied as an additional reduction factor to 
  each YSM TASS projection up to rotation age.</p>", "<p>", phrase, "</p>"))
  return(fufhtext)
})



output$future_fh <- renderUI({
  
  fufhtext()
  
})



fufhplot1 <- reactive({
  req(input$SelectCategory, input$SelectVar)
  risk_vol <- risk_vol()
  
  fig16 <- if (sum(risk_vol$mort_flag == 1) > 0) {
    risk_vol %>%
      ungroup() %>%
      filter(mort_flag == 1) %>%
      mutate(AGN_new = fct_reorder(AGN_new, volperc)) %>%
      ggplot(aes(x = "",  y = volperc, fill = AGN_new)) +
      geom_bar(stat="identity", width = 0.5) +
      scale_fill_brewer(name = NULL, palette = "Set2", direction = -1) +
      scale_y_continuous(labels=scales::percent) +
      labs(x = "Current Age (yrs)", y = "% volume impact",
           title = "Damage Agents",
           subtitle = "Immediate Impact") +
      theme(
        axis.ticks.x=element_blank(), 
        panel.grid.major.y = element_line(color = 'darkgray'), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        rect = element_blank(),
        legend.title = element_blank()
      )  
  } else {
    ggplot() + 
      theme_bw() + 
      geom_bar(aes(x = "",  y = 0), stat="identity", width = 0.5) +
      #geom_text(aes(0,0,label='N/A')) +
      labs(x = "Current Age (yrs)", y = "% volume impact",
           title = "Damage Agents",
           subtitle = "Immediate Impact")+ 
      theme(
        axis.ticks.x=element_blank(), 
        panel.grid.major.y = element_line(color = 'darkgray'), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        rect = element_blank(),
        legend.title = element_blank())
  }
  return(fig16)
  
})
  

fufhplot2 <- reactive({
  req(input$SelectCategory, input$SelectVar)
  risk_vol <- risk_vol()
  
  fig17 <- if (sum(risk_vol$mort_flag == 2) > 0) {
    risk_vol %>%
      ungroup() %>%
      filter(mort_flag == 2) %>%
      mutate(AGN_new = fct_reorder(AGN_new, volperc)) %>%
      ggplot() +
      geom_bar(aes(x = factor(1),  y = year60, fill = AGN_new), stat="identity", width = 0.5) +
      geom_bar(aes(x = factor(2),  y = year80, fill = AGN_new), stat="identity", width = 0.5) +
      geom_bar(aes(x = factor(3),  y = year100, fill = AGN_new), stat="identity", width = 0.5) +
      scale_fill_brewer(name = NULL, palette = "Set2", direction = -1) +
      scale_y_continuous(labels=scales::percent) +
      scale_x_discrete(breaks =1:3, labels=c('60', '80', '100')) +
      labs(x = "Age (yrs)", y = "% volume impact",
           title = "Damage Agents",
           subtitle = "Incremental Impact") +
      theme(
        axis.ticks.x=element_blank(), 
        panel.grid.major.y = element_line(color = 'darkgray'), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        rect = element_blank(),
        legend.title = element_blank()
      )  
  } else {
    ggplot() + 
      geom_bar(aes(x = factor(c(60, 80, 100)),  y = c(0,0,0)), stat="identity", width = 0.5) +
      geom_text(aes(2,1,label='N/A')) +
      labs(x = "Age (yrs)", y = "% volume impact",
           title = "Damage Agents",
           subtitle = "Incremental Impact")+ 
      theme(
        axis.ticks.x=element_blank(), 
        panel.grid.major.y = element_line(color = 'darkgray'), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        rect = element_blank(),
        legend.title = element_blank())
  }
  
  return(fig17)
})


output$dam_immed <- renderPlot({
  
  grid.arrange(fufhplot1(), fufhplot2(), ncol = 2)
  
  
})