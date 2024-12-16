## Server logic for Site Index tab ----

###############################################.
## Indicator definitions ----
###############################################.
#Subsetting by domain 

sitext <- reactive({
  req(input$SelectCategory, input$SelectVar)
  sitext <- HTML("Site index (SI) is an estimate of a stand's potential productivity by species. 
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
  
  return(sitext)
})


output$site_index_pspl <- renderUI({
  sitext()
})

siflex <- reactive({
  req(input$SelectCategory, input$SelectVar)
  si_dat <- si_dat()
  
  si_flex <- flextable(si_dat) %>% 
    bold(j = c(4,5,9), bold = TRUE, part = "all") %>%
    color(i = ~ sig_rope == "Y", j = 9, color = 'red', part = "body") %>%
    set_header_labels(values = c("Spc", "n", "BHAge", "YSM", "PSPL", "ROM", "L95%", "U95%", "test")) %>% 
    add_header_row(top = TRUE, values = c(" ", "#Pairs", "YSM", "SI (m)", "Ratio of Means", "ROPE"), 
                   colwidths = c(1,1,1,2,3,1)) %>%
    align(align = "center", part = "all")
  
  return(si_flex)   
})


output$si_pspl_flex <- renderUI({
  htmltools_value(siflex())
})


sitrendtext <- reactive({
  req(input$SelectCategory, input$SelectVar)
  
  sitrendtext <- HTML("The average site index vs. breast height age from all valid site trees 
       is plotted for each species and sample measurement (graph below; joined 
       dots are for repeated measurements). Subsequent measurements may not 
       always include the same site trees (due to changes in site tree suitability, 
       or height by DBH crossovers), site index and breast height age may also 
       change over time. The most reliable site index estimate for a given 
       species and YSM plot is the measurement closest to breast height age 50. 
       The number above each line denotes the number of plots used to average the site index.")
  return(sitrendtext)
})


output$trend_si <- renderUI({
  sitrendtext()
})

siplot <- reactive({
  req(input$SelectCategory, input$SelectVar)
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
    theme(axis.line = element_line(colour="darkgray"), 
          panel.grid.major.y = element_line(color = 'darkgray'), 
          #panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          rect = element_blank()
    )  
  
  return(p)
})


output$si_trend <- renderPlot({
  siplot()
})
