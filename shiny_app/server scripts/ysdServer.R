## Server logic for Young Stand Description tab ----

###############################################.
## Indicator definitions ----
###############################################.
#Subsetting by domain 
  
ysd <- reactive({
  req(input$SelectCategory, input$SelectVar)
  
  ysd <-  HTML(paste0("Stand summaries (all species combined) are compiled and summarized for 
       all samples in the target population at the time of the latest measurement. 
       Compilations are for all standing trees >= 4cm DBH, except net merchantable 
       volume (i.e., PL >= 12.5cm DBH, & all other species >= 17.5cm DBH, 
       excluding 30cm stump height, 10cm top diameter, & decay). 
       Species code names are listed on <b> General Notes</b>."))
  
  return(ysd)
})


output$young_stand_description <- renderUI({
  
  ysd()
  
})

summaryflex <- reactive({
  req(input$SelectCategory, input$SelectVar)
  if(!is.null(clstr_id())){
    
    summary_data <- summary_data()
    summary_si <- summary_si()
    
    n_sid = length(clstr_id())
    
    summary_data <- summary_data %>%
      group_by(SITE_IDENTIFIER) %>%
      summarise(BA_LS = sum(BA_HA_LS, na.rm = T),
                STEMS_LS = sum(STEMS_HA_LS, na.rm = T),
                WSV_LS = sum(VHA_WSV_LS, na.rm = T),
                NTWB_LS = sum(VHA_NTWB_LS, na.rm = T),
                BA_DS = sum(BA_HA_DS, na.rm = T),
                STEMS_DS = sum(STEMS_HA_DS, na.rm = T),
                WSV_DS = sum(VHA_WSV_DS, na.rm = T),
                NTWB_DS = sum(VHA_NTWB_DS, na.rm = T)) %>%
      mutate(QMD_LS = sqrt((BA_LS*40000)/(pi*STEMS_LS)),
             QMD_DS = sqrt((BA_DS*40000)/(pi*STEMS_DS))) %>%
      replace(is.na(.), 0)
    
    summary_data1 <- summary_data %>%
      summarise(across(BA_LS:QMD_DS, list(min = min, max = max, mean = mean, sd = sd)))
    
    summary_data2 <- summary_data1 %>%
      pivot_longer(everything(),
                   names_to = c(".value", "LD", "fun"),
                   names_pattern = "(.*)_(.*)_(.*)") 
    
    Avg = setDT(summary_data2)[LD == "LS" & fun == "mean", .(BA, STEMS, QMD, WSV, NTWB)]
    Min = setDT(summary_data2)[LD == "LS" & fun == "min", .(BA, STEMS, QMD, WSV, NTWB)]
    Max = setDT(summary_data2)[LD == "LS" & fun == "max", .(BA, STEMS, QMD, WSV, NTWB)]
    SD = setDT(summary_data2)[LD == "LS" & fun == "sd", .(BA, STEMS, QMD, WSV, NTWB)]
    Avg_D = setDT(summary_data2)[LD == "DS" & fun == "mean", .(BA, STEMS, QMD, WSV, NTWB)]
    
    table2 <- data.frame(
      n = c(rep(n_sid, 5), summary_si$n), 
      Avg = c(Avg[1, c(round(BA, 1), round(STEMS, 0), round(QMD, 1),  round(WSV, 0), round(NTWB,0))], round(summary_si$Avg, 0)),
      Min = c(Min[1, c(round(BA, 1), round(STEMS, 0), NA,  round(WSV, 0), round(NTWB,0))], round(summary_si$Min, 0)),
      Max = c(Max[1, c(round(BA, 1), round(STEMS, 0), NA,  round(WSV, 0), round(NTWB,0))], round(summary_si$Max, 0)),
      SD = c(SD[1, c(round(BA, 1), round(STEMS, 0), NA,  round(WSV, 0), round(NTWB,0))], NA),
      Avg_D = c(Avg_D[1, c(round(BA, 1), round(STEMS, 0), round(QMD, 1),  round(WSV, 0), round(NTWB,0))], NA))
        
    rownames(table2) <- c('Basal Area', "Total Stems (#/ha)", "Quadratic Mean DBH (cm)",
                          "Whole Stem Vol.", "Net Merch Vol.", 
                          "Total age of lead species (yrs)")
    
    flextable2 <- flextable(table2 %>% 
                              rownames_to_column("  ")) %>% 
      set_header_labels(values = c(" ", "n", "Avg", "Min", "Max", "SD", "Avg")) %>% 
      add_header_row(top = TRUE, values = c("", "Live", "Dead"), colwidths = c(2,4,1)) %>%
      align(align = "center", part = "header") %>%
      merge_v(part = "header") # %>%
    
    flextable2 <- flextable::compose(flextable2,
                                     i = 1, j = 1, 
                                     value = as_paragraph('Basal Area (m',as_sup('2'),'/ha)'))
    
    flextable2 <- flextable::compose(flextable2,
                                     i = 4, j = 1, 
                                     value = as_paragraph('Whole Stem Vol. (m',as_sup('3'),'/ha)'))
    
    flextable2 <- flextable::compose(flextable2,
                                     i = 5, j = 1, 
                                     value = as_paragraph('Net Merch Vol. (m',as_sup('3'),'/ha)'))
    flextable2 <- bg(flextable2, 
                     j = 7,
                     bg = "lightgray", part = "all")
    
    flextable2 <- autofit(flextable2)
    
    return(flextable2)
    }      
})


output$stand_summary_flex <- renderUI({
  htmltools_value(summaryflex())
})


livespplot <- reactive({
  req(input$SelectCategory, input$SelectVar)
  if (!is.null(clstr_id())){
    
    summary_data <- summary_data()
    
    spc_ba <- summary_data %>%
      group_by(SPC_GRP1) %>%
      summarise(SUM = sum(BA_HA_LS, na.rm = TRUE), 
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
      summarise(SUM = sum(STEMS_HA_LS, na.rm = TRUE), 
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
      filter(!(SPC_GRP1 == "" & PERC == 0)) %>% 
      arrange(BY, order)
    
    livespplot <- spc_summary |>
      group_by(BY, SPC_GRP2) |>
      mutate(total = sum(PERC)) |>
      ungroup() |>
      select(SPC_GRP2, BY, total) |>
      distinct() |> 
      ggplot(aes(x = reorder(SPC_GRP2, ifelse(BY=="BA",-total, 0)), y = total, fill = factor(BY))) +
      geom_bar(position = position_dodge2(preserve = "single"), stat = "identity", width = 0.7) +
      labs(x = "", y = "% of Total", title = "Live Species Composition") + 
      scale_fill_manual(values = c("steelblue", "#B4464B"), name = NULL, labels = c("by BA", "by # stems")) +
      scale_y_continuous(expand = c(0, 0),limits = c(0, round(max(spc_summary$PERC, na.rm = T),-1)+5)) +
      theme(
        axis.line = element_line(colour="darkgray"), 
        panel.grid.major.y = element_line(color = 'darkgray'), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        rect = element_blank()
      ) 
    
  }
  
  return(livespplot)
})


output$live_sp <- renderPlot({
  
  livespplot()
  
  })

becplot <- reactive({
  req(input$SelectCategory, input$SelectVar)
  if (!is.null(clstr_id())){
    
    #figdata <- sample_data %>%
    #  filter(CLSTR_ID %in% clstr_id())
    #
    #p <- ggplot(figdata, aes(x=fct_infreq(factor(BEClabel)))) +
    #  geom_bar(stat="count", width=0.5, fill="steelblue") +
    #  scale_x_discrete(guide = guide_axis(angle = -45)) +
    #  scale_y_continuous(expand = c(0, 0), 
    #                     label = ~ scales::comma(.x, accuracy = 1)) +
    #  labs(x = "", y = "# of YSM samples",
    #       title = "YSM Sample Distribution by BEC subzone/variant")  + 
    #  theme(
    #    panel.grid.major.x = element_blank(),
    #    panel.grid.minor.x = element_blank(),
    #    panel.grid.major.y = element_line(colour="darkgray"),
    #    rect = element_blank()
    #  )
    
    figdata <- sample_data %>%
      filter(CLSTR_ID %in% clstr_id()) %>%
      pull(BEClabel)
    
    integer_breaks <- function(n = 5, ...) {
      fxn <- function(x) {
        breaks <- floor(pretty(x, n, ...))
        names(breaks) <- attr(breaks, "labels")
        breaks
      }
      return(fxn)
    }
    
    p <- ggplot(data.frame(rev(sort(table(figdata)))), aes(x = figdata, y = Freq)) +
      geom_bar(stat="identity", width=0.5, fill="steelblue") +
      scale_x_discrete(guide = guide_axis(angle = -45)) +
      scale_y_continuous(expand = c(0, 0),
                         breaks = integer_breaks()) +
      labs(x = "", y = "# of YSM samples",
           title = "YSM Sample Distribution by BEC subzone/variant")  + 
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour="darkgray"),
        rect = element_blank()
      )
    
  }
  return(p)
})


output$bec_dist <- renderPlot({
  
  becplot()
  
})

stockplot <- reactive({
  req(input$SelectCategory, input$SelectVar)
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
    
    p <- ggplot(fig5_dat, aes(x = DBH_CLASS_relevel, y = PERC_TOT_VOL_HA_SPC, fill = SPC_GRP2)) + 
      geom_bar(stat = "identity") + 
      scale_fill_brewer(name = "", palette = "Set2") +
      scale_x_discrete(drop=FALSE) +
      scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
      labs(x = "DBH class (cm)", y = "% of total vol/ha",
           title = "Stock Table - live trees") +
      theme(
        #axis.line = element_line(colour="darkgray"), 
        panel.grid.major.y = element_line(color = 'darkgray'), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        rect = element_blank()
      ) 
    
  }
  return(p)
})


output$stock_table <- renderPlot({
  
  stockplot()
  
})