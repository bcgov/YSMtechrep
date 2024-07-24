## Server logic for Young Stand Description tab ----

###############################################.
## Indicator definitions ----
###############################################.
#Subsetting by domain 
  
#summary_data <- reactive({
#  
#  req(input$SelectCategory, input$SelectVar)
#  input$genearate
#  
#  summary_data<-spcs_data %>% 
#    filter(CLSTR_ID %in% clstr_id(), UTIL == 4) %>%
#    mutate(SPC_GRP1 = substr(SPECIES,1,2)) %>%
#    mutate(SPC_GRP1 = ifelse(SPECIES %in% decidspc, 'DE', SPC_GRP1))%>%  
#    mutate(BA_HA_L = BA_HA_LS + BA_HA_LF,
#           BA_HA_D = BA_HA_DS + BA_HA_DF,
#           STEMS_HA_L = STEMS_HA_LS + STEMS_HA_LF,
#           STEMS_HA_D = STEMS_HA_DS + STEMS_HA_DF,
#           VHA_WSV_L = VHA_WSV_LS + VHA_WSV_LF,
#           VHA_WSV_D = VHA_WSV_DS + VHA_WSV_DF,
#           n = length(unique(SITE_IDENTIFIER))) %>% 
#    select(SITE_IDENTIFIER, CLSTR_ID, SPECIES, SPC_GRP1, 
#           BA_HA_L, BA_HA_D, STEMS_HA_L, STEMS_HA_D, 
#           VHA_WSV_L, VHA_WSV_D, n, 
#           QMD_LS, QMD_DS)
#  
#  summary_mer<-spcs_data %>% 
#    filter(CLSTR_ID %in% clstr_id(), UTIL == ifelse(SPECIES =="PL", 12.5, 17.5)) %>% 
#    select(SITE_IDENTIFIER, CLSTR_ID, UTIL, SPECIES, 
#           VHA_MER_LS, VHA_MER_LF, VHA_MER_DS, VHA_MER_DF) %>% 
#    mutate(VHA_MER_L = VHA_MER_LS + VHA_MER_LF,
#           VHA_MER_D = VHA_MER_DS + VHA_MER_DF) 
#  
#  summary_data <- summary_data %>%
#    left_join(summary_mer, by = c('SITE_IDENTIFIER', 'CLSTR_ID', "SPECIES"))
#  
#  summary_data <- summary_data %>%
#    left_join(SI_data, by = c('SITE_IDENTIFIER', 'CLSTR_ID', "SPECIES"))
#  
#  return(summary_data)
#  
#})
#
#
#decid_vol <- reactive({
#  
#  summary_data <- summary_data()
#  
#  decid_vol <- summary_data %>%
#    mutate(spc_dec_con = ifelse(SPC_GRP1 == "DE", "decid", "con")) %>%
#    group_by(spc_dec_con) %>%
#    summarize(VHA_WSV_L = sum(VHA_WSV_L, na.rm = T)) %>%
#    ungroup() %>%
#    mutate(vol = round(VHA_WSV_L/sum(VHA_WSV_L, na.rm = T)*100,0)) %>%
#    filter(spc_dec_con == "decid") %>%
#    pull(vol)
#  
#  return(decid_vol)
#})


output$young_stand_description <- renderUI({
  
  HTML("<p>Stand summaries (all species combined) are compiled and summarized for 
       all samples in the target population at the time of the latest measurement. 
       Compilations are for all standing trees >=4cm DBH, except net merchantable 
       volume (i.e., PL >= 12.5cm DBH, & all other species >=17.5cm DBH, 
       excluding 30cm stump height, 10cm top diameter, & decay). 
       Species code names are listed on <b> General Notes </b> tab.<p/>"
  )
  
})

output$stand_summary_flex <- renderUI({
  
  if(!is.null(clstr_id())){
    
    summary_data <- summary_data()
    
    n_sid = unique(summary_data$n)
    
    sum_cols <- c("BA_HA_L", "BA_HA_D",
                  "STEMS_HA_L", "STEMS_HA_D",
                  "VHA_WSV_L", "VHA_WSV_D",
                  "VHA_MER_L", "VHA_MER_D")
    
    summary_table <- summary_data[, lapply(.SD, sum, na.rm=TRUE), by = SITE_IDENTIFIER, .SDcols=sum_cols] 
    
    Avg <- summary_table[, lapply(.SD, mean, na.rm=TRUE), .SDcols=sum_cols]
    Min <- summary_table[, lapply(.SD, min, na.rm=TRUE), .SDcols=sum_cols]
    Max <- summary_table[, lapply(.SD, max, na.rm=TRUE), .SDcols=sum_cols]
    SD <- summary_table[, lapply(.SD, sd, na.rm=TRUE), .SDcols=sum_cols]
    QMD_mean <- summary_data[, lapply(.SD, mean, na.rm=TRUE),  .SDcols=c("QMD_LS","QMD_DS")] 
    summary_table2 <- summary_data %>%
      filter(!is.na(SI_M_TLSO)) %>%
      summarise(n = sum(!is.na(AGET_TLSO)),
                Avg = ifelse(n>0, mean(AGET_TLSO, na.rm = T), NA),
                Min = ifelse(n>0, min(AGET_TLSO, na.rm = T), NA),
                Max = ifelse(n>0, max(AGET_TLSO, na.rm = T), NA)) %>%
      as.data.table()
    
    
    table2 <- data.frame(
      n = c(rep(n_sid, 5), summary_table2$n), 
      Avg = c(Avg[1, c(round(BA_HA_L, 1), round(STEMS_HA_L, 0), round(QMD_mean$QMD_LS, 0),  round(VHA_WSV_L, 0), round(VHA_MER_L,0))], round(summary_table2$Avg, 0)),
      Min = c(Min[1, c(round(BA_HA_L, 1), round(STEMS_HA_L, 0), NA,  round(VHA_WSV_L, 0), round(VHA_MER_L,0))], round(summary_table2$Min, 0)),
      Max = c(Max[1, c(round(BA_HA_L, 1), round(STEMS_HA_L, 0), NA,  round(VHA_WSV_L, 0), round(VHA_MER_L,0))], round(summary_table2$Max, 0)),
      SD = c(SD[1, c(round(BA_HA_L, 1), round(STEMS_HA_L, 0), NA,  round(VHA_WSV_L, 0), round(VHA_MER_L,0))], NA),
      Avg_D = c(Avg[1, c(round(BA_HA_D, 1), round(STEMS_HA_D, 0), round(QMD_mean$QMD_DS, 0),  round(VHA_WSV_D, 0), round(VHA_MER_D,0))], NA)
      
    )
    rownames(table2) <- c('Basal Area', "Total Stems (#/ha)", "Quadratic Mean DBH (cm)",
                          "Whole Stem Vol.", "Net Merch Vol.", 
                          "Total age all site trees (yrs)")
    
    flextable2 <- flextable(table2 %>% 
                              rownames_to_column("  ")) %>% 
      set_header_labels(values = c(" ", "n", "Avg", "Min", "Max", "SD", "Avg")) %>% 
      add_header_row(top = TRUE, values = c("", "Live", "Dead"), colwidths = c(2,4,1)) %>%
      align(align = "center", part = "header") %>%
      merge_v(part = "header") # %>%
    
    flextable2 <- flextable::compose(flextable2,
                                     i = 1, j = 1, 
                                     value = as_paragraph('Basal Area (m',as_sup('3'),'/ha)'))
    
    flextable2 <- flextable::compose(flextable2,
                                     i = 4, j = 1, 
                                     value = as_paragraph('Whole Stem Vol. (m',as_sup('3'),'/ha)'))
    
    flextable2 <- flextable::compose(flextable2,
                                     i = 5, j = 1, 
                                     value = as_paragraph('Net Merch Vol. (m',as_sup('3'),'/ha)'))
    
    flextable2 <- autofit(flextable2)
    
    return(flextable2 %>%
             htmltools_value())      
  }
})


output$live_sp <- renderPlot({
  
  if (!is.null(clstr_id())){
    
    summary_data <- summary_data()
    #spcs_dat <- spcs_dat()
    
    spc_ba <- summary_data %>%
      group_by(SPC_GRP1) %>%
      summarise(SUM = sum(BA_HA_L, na.rm = TRUE), 
                FREQ = n()) %>%
      arrange(desc(SUM))
    
    spc_ba <- spc_ba %>%
      mutate(order = row_number(),
             PERC = SUM/sum(SUM, na.rm = TRUE)*100) %>%
      mutate(SPC_GRP2 = ifelse(order <= 7, SPC_GRP1, 'Other')) %>%
      mutate(SPC_GRP2 = ifelse(SPC_GRP2 == 'DE', 'Decid', SPC_GRP2),
             BY = "BA")  %>%
      select(SPC_GRP1, SPC_GRP2, BY, order, SUM, PERC)
    
    # *create species group 3, comprising 8 species classes in descending order by stems / ha;
    spc_stem <- summary_data %>%
      group_by(SPC_GRP1) %>%
      summarise(SUM = sum(STEMS_HA_L, na.rm = TRUE), 
                FREQ = n()) %>%
      arrange(desc(SUM))
    
    spc_stem <- spc_stem %>%
      mutate(order = row_number(),
             PERC = SUM/sum(SUM, SUM = TRUE)*100) %>%
      mutate(SPC_GRP2 = ifelse(order <= 7, SPC_GRP1, 'Other'),
             SPC_GRP2 = ifelse(SPC_GRP2 == 'DE', 'Decid', SPC_GRP2),
             BY = "STEMS")  %>%
      select(SPC_GRP1, SPC_GRP2, BY, order, SUM, PERC)
    
    spc_summary <- rbind(spc_ba, spc_stem) 
    
    spc_summary <- spc_summary %>% 
      arrange(BY, order)
    
    p <- spc_summary |>
      group_by(BY, SPC_GRP2) |>
      mutate(total = sum(PERC)) |>
      ungroup() |>
      select(SPC_GRP2, BY, total) |>
      distinct() |> #important for only graphing single element 
      ggplot(aes(x = reorder(SPC_GRP2, ifelse(BY=="BA",-total, 0)), y = total, fill = factor(BY))) +
      geom_bar(position = position_dodge2(preserve = "single"), stat = "identity", width = 0.7) +
      labs(x = "", y = "% of Total", title = "Live Species Composition") + 
      scale_fill_manual(values = c("steelblue", "#B4464B"), name = NULL, labels = c("by BA", "by # stems")) +
      scale_y_continuous(expand = c(0, 0),limits = c(0, round(max(spc_summary$PERC, na.rm = T),-1)+5)) +
      #theme_bw() + 
      theme(
        axis.line = element_line(colour="darkgray"), 
        panel.grid.major.y = element_line(color = 'darkgray'), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        rect = element_blank()
      ) 
    
    p 
}
  
  #else{
  #  
  #  
  #  p <- plot_ly(dummyData, x = dummyData$tot_stand_age,
  #               y = dummyData$wsvha_liv,
  #               type = "scatter",
  #               mode = "markers") %>%
  #    layout(  autosize=TRUE, dragmode = 'lasso', xaxis = (list(range = c(2013, 2023), title = "Year", automargin = TRUE)),
  #             legend = list(orientation = 'h',  y = 100), margin = list(r = 20, b = 50, t = 50, pad = 4),
  #             yaxis = (list(range = c(0, 100),title = "Mean BA (m2/ha)")))%>%
  #    config(displayModeBar = F)
  #  
  #  # ggplotly(p) %>%
  #  p} 
  
  })


output$bec_dist <- renderPlot({
  
  if (!is.null(clstr_id())){
    
    figdata <- sample_data %>%
      filter(CLSTR_ID %in% clstr_id())
    
    p <- ggplot(figdata, aes(x=fct_infreq(factor(BEClabel)))) +
      geom_bar(stat="count", width=0.5, fill="steelblue") + 
      #scale_x_discrete(breaks = NULL) +
      #theme_bw() + 
      #scale_x_discrete(breaks = NULL) + 
      theme(
        axis.line = element_line(colour="darkgray"), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        rect = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)
      ) +
      scale_y_continuous(
        expand = c(0, 0),
        #breaks= pretty_breaks(),
        breaks = function(x) unique(floor(pretty(seq(min(x), (max(x) + 1) * 1.1)))),
        #breaks = function(x) seq(floor(min(x)), ceiling(max(x))),
        #labels = comma,
        limits = c(0, max(table(figdata$BEClabel))+0.5))+
      labs(x = "", y = "# of YSM samples",
           title = "YSM Sample Distribution by BEC subzone/variant") 
    
    p 
  }
  
})


output$stock_table <- renderPlot({
  
  if (!is.null(clstr_id())){
    
    fig5_dat <- tree_fh_data %>%
      filter(CLSTR_ID %in% clstr_id(), DAM_NUM == 1, LV_D == "L") %>%
      mutate(VOL_WSV_HA = VOL_WSV*PHF_TREE,
             PERC_TOT_VOL_HA = VOL_WSV_HA/sum(VOL_WSV_HA, na.rm = T),
             DBH_CLASS = round(DBH/5)*5) %>% 
      mutate(SPC_GRP1 = substr(SPECIES,1,2)) %>%
      mutate(SPC_GRP1 = ifelse(SPECIES %in% decidspc, 'DE', SPC_GRP1))
    
    fig5_dat <- fig5_dat %>%
      group_by(SPC_GRP1, DBH_CLASS) %>%
      summarise(PERC_TOT_VOL_HA_SPC = sum(PERC_TOT_VOL_HA, na.rm = T)) %>%
      ungroup()
    
    fig5_dat_label <- fig5_dat %>%
      group_by(SPC_GRP1) %>%
      summarize(TOT_VOL_HA = sum(PERC_TOT_VOL_HA_SPC, na.rm = T)) %>%
      arrange(desc(TOT_VOL_HA)) %>%
      mutate(order = row_number())
    
    fig5_dat <- fig5_dat %>%
      left_join(fig5_dat_label, by = "SPC_GRP1")
    
    fig5_dat <- fig5_dat %>%
      mutate(SPC_GRP2 = ifelse(order <= 7, SPC_GRP1, 'Other'),
             SPC_GRP2 = ifelse(SPC_GRP2 == 'DE', 'Decid', SPC_GRP2),
             DBH_CLASS_relevel = cut(DBH_CLASS, breaks = c(seq(-1, 59, 5), Inf), 
                                     labels = c(seq(0, 55, 5), "60+")))
    
    p <- ggplot(fig5_dat, aes(x = factor(DBH_CLASS_relevel), y = PERC_TOT_VOL_HA_SPC, fill = SPC_GRP2)) + 
      geom_bar(stat = "identity") + 
      scale_fill_brewer(name = "", palette = "Set2") +
      scale_x_discrete(drop=FALSE) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "DBH class (cm)", y = "% of total vol/ha",
           title = "Stock Table - live trees") +
      #theme_bw() + 
      theme(
        #axis.line = element_line(colour="darkgray"), 
        #panel.grid.major.y = element_line(color = 'darkgray'), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        rect = element_blank()
      ) 
    
    p 
  }
  
})