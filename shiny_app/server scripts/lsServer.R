## Server logic for Leading Species tab ----

###############################################.
## Indicator definitions ----
###############################################.
#Subsetting by domain 

lstext <- reactive({
  req(input$SelectCategory, input$SelectVar)
  
  lstext <- HTML("<p>Leading species (by basal area) is compared between YSM & VRI where the 
       <i>‘correct leading species classification rate’</i> is a percent of all YSM samples 
       with matching inventory leading species at latest measurement (table below). 
       A separate assessment of overall species composition is also compared 
       between YSM and TSR modeled regeneration inputs (figure below). 
       Overall species composition overlap is a rough index, and expressed as 
       the ratio between the minimum in common relative to the maximum in common 
       that could have been. The planted & natural densities used in TSR inputs 
       are combined as part of the overall species composition comparison.</p>")
  
  return(lstext)
})


output$leading_sp <- renderUI({
  lstext()
})

lsflex <- reactive({
  req(input$SelectCategory, input$SelectVar)
  
  if(!is.null(clstr_id())){
    
    LD_dat <- as.data.frame(LD_dat())
    
    LD_table <- proc_freq(LD_dat, "SPC_GRP_VRI", "SPC_GRP1",
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
      add_footer_lines(value = as_paragraph(paste0("Correct Leading Species Classification Rate = ", 
                                                   correct_ls(), "%")))
    
    LD_table <- bg(x = LD_table, 
                   j = (1:(LD_table$body$content$ncol/2))*2, 
                   bg = "lightgray", part = "body")
    
    LD_table <- labelizor(x = LD_table, 
                          part = "header", 
                          labels = c("SPC_GRP_VRI" = "VRI", 
                                     "SPC_GRP1" = "YSM"))  %>%
      align(align = "left", part = "header") %>%
      autofit()
    
    return(LD_table)      
  }
})


output$leading_sp_flex <- renderUI({
  htmltools_value(lsflex())
})

spcomp <- reactive({
  req(input$SelectCategory, input$SelectVar)
  Fig11_dat <- Fig11_dat()
  
  percoverlap <- percoverlap()
  
  p <- Fig11_dat %>%
    filter(spcperc >=0.001) %>%
    ggplot(aes(x=SPC_GRP1, y=spcperc/100, fill=source)) + 
    geom_bar(stat="identity", position = position_dodge2(preserve = "single"), width=0.7) +
    scale_fill_manual(values = c("#B4464B", "steelblue"), name = NULL, labels = c("TSR-INPUT", "YSM")) +
    scale_x_discrete(drop=FALSE) +
    scale_y_continuous(labels = scales::label_percent(), expand = c(0, 0)) +
    labs(x = "Species", y = "% of total Stems/ha",
         title = "Overall Species Composition Comparison",
         caption=paste0("Overall Species Composition Overlap = ", round(percoverlap, 0), "%")) +
    theme(
      panel.grid.major.y = element_line(color = 'darkgray'), 
      plot.caption = element_text(hjust=0, size=rel(1.2)),
      legend.position="top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      rect = element_blank()
    ) 
  
  return(p)
})


output$spc_comp <- renderPlot({
  
  spcomp()
})
