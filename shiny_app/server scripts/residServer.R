## Server logic for Residual Trees tab ----

###############################################.
## Indicator definitions ----
###############################################.
#Subsetting by domain 
output$residual <- renderUI({
  
  HTML("Young stands may include older residual trees which have important 
       ecological, management, and growth implications. Residual trees are 
       identified in the field using standard measurement protocols. 
       Depending on the analysis, it may be necessary to separate the residual 
       from post-harvest-regenerated trees in young stands. The contribution of 
       the residual cohort in the measured YSM samples is reported (chart below).")
  
})


#fig6_dat <- reactive({
#  
#  req(input$SelectCategory, input$SelectVar)
#  input$genearate
#  
#  fig6_dat <- tree_fh_data %>%
#    filter(CLSTR_ID %in% clstr_id(), DAM_NUM==1, LV_D == "L") %>%
#    mutate(VOL_WSV_HA = VOL_WSV*PHF_TREE,
#           PERC_TOT_VOL_HA = VOL_WSV_HA/sum(VOL_WSV_HA, na.rm = T),
#           DBH_CLASS = round(DBH/5)*5) 
#  
#  fig6_dat <- fig6_dat %>%
#    mutate(DBH_CLASS_relevel = cut(DBH_CLASS, breaks = c(seq(-1, 59, 5), Inf), 
#                                   labels = c(seq(0, 55, 5), "60+")),
#           RESIDUAL_relevel = fct_recode(factor(RESIDUAL), "Managed"="N", "Residual"="Y"))
#  
#  return(fig6_dat)
#  
#})
#
#
#fig6_max <- reactive({
#  
#  req(input$SelectCategory, input$SelectVar)
#  input$genearate
#  
#  fig6_dat <- fig6_dat()
#  
#  fig6_max <- fig6_dat %>%
#  group_by(DBH_CLASS_relevel) %>%
#  summarise(sum = sum(PERC_TOT_VOL_HA, na.rm = T)) %>%
#  ungroup() %>%
#  summarise(ymax = max(sum, na.rm = T)) %>%
#  pull(ymax)
#  
#  return(fig6_max)
#  
#})
#
#fig6_sum <- reactive({
#  
#  fig6_dat <- fig6_dat()
#  
#  fig6_sum <- fig6_dat %>%
#    filter(RESIDUAL == "Y") %>%
#    summarise(tot = sum(PERC_TOT_VOL_HA, na.rm = T)*100) %>%
#    pull(tot)
#  
#  return(fig6_sum)
#  
#})


output$residual_ysm <- renderPlot({
  
  fig6_dat <- fig6_dat()
  fig6_max <- fig6_max()
  fig6_sum <- fig6_sum()
  
  p <- ggplot(fig6_dat, 
                 aes(x = DBH_CLASS_relevel, y = PERC_TOT_VOL_HA, 
                     fill =RESIDUAL_relevel, color = RESIDUAL_relevel)) + 
    geom_bar(stat = "identity", position = position_stack(reverse = T)) + 
    scale_x_discrete(drop=FALSE) +
    scale_fill_manual(values = c("steelblue", "#B4464B"), drop=FALSE) +
    scale_color_manual(values = c("steelblue", "#B4464B"), drop=FALSE) +
    scale_y_continuous(labels = scales::percent, expand = c(0, 0),
                       limits = c(0, round(fig6_max, 1)+0.1)) + 
    labs(x = "DBH Class (cm)", y = "% of Total Volume",
         caption=paste0("Percent of total volume comprising residuals = ", round(fig6_sum, 0), "%")) +
    #theme_bw() + 
    theme(
      axis.line = element_line(colour="darkgray"), 
      panel.grid.major.y = element_line(color = 'darkgray'), 
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      rect = element_blank(),
      legend.box.background = element_rect(fill = "white", color = NA),
      legend.position =  "inside", 
      legend.position.inside = c(0.90, 0.90),
      legend.title = element_blank(),
      plot.caption = element_text(hjust=0, size=rel(1.2))
    )  
  
  p
  
})