## Server logic for Residual Trees tab ----

###############################################.
## Indicator definitions ----
###############################################.
#Subsetting by domain 

residtext <- reactive({
  req(input$SelectCategory, input$SelectVar)
  residtext <- HTML("Young stands may include older residual trees which have important 
       ecological, management, and growth implications. Residual trees are 
       identified in the field using standard measurement protocols. 
       Depending on the analysis, it may be necessary to separate the residual 
       from post-harvest-regenerated trees in young stands. The contribution of 
       the residual cohort in the measured YSM samples is reported (chart below).")
  
  return(residtext)
})



output$residual <- renderUI({
  
  residtext()
  
})


residplot <- reactive({
  req(input$SelectCategory, input$SelectVar)
  
  fig6_dat <- fig6_dat()
  fig6_max <- fig6_max()
  fig6_sum <- fig6_sum()
  
  p <- ggplot(fig6_dat, 
              aes(x = DBH_CLASS_relevel, y = PERC_TOT_VOL_HA, 
                  fill =RESIDUAL_relevel, color = RESIDUAL_relevel)) + 
    geom_bar(stat = "identity", position = position_stack(reverse = T), width=0.7) + 
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
  
  return(p)
})


output$residual_ysm <- renderPlot({
  
  residplot()
  
})