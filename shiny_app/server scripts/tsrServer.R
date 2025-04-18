## Server logic for TSR Comparison tab ----

###############################################.
## Indicator definitions ----
###############################################.

curvoltext <- reactive({
  req(input$SelectCategory, input$SelectVar)
  
  curvoltext <- paste0("<p>Field measured YSM volumes are compared to predicted volumes developed 
  for FAIB’s TSR analysis. FAIB’s TSR yield table development process has three 
  types of yield curves developed for stands less than 50 years of age:</p>
  
  <ol><li><u>TSR TIPSY Opening Specific</u> – where the VRI feature links to a RESULTS 
  opening record that contains the minimum required yield table inputs. 
  These features are projected in TIPSY ver 4.4 using RESULTS opening specific 
  records as inputs.</li>
  <li><u>TSR TIPSY Aggregate</u> – where the VRI feature links to a RESULTS opening 
  record that does not contain the minimum required yield table inputs. 
  These features are projected in TIPSY ver 4.4 using the average of many 
  RESULTS opening specific yield curves within the same management unit and 
  Biogeoclimatic subzone.</li>
  <li><u>TSR VDYP</u> – where the VRI feature does not spatially match a RESULTS 
  opening record. These features are projected in VDYP ver 7 using VRI inventory 
  rank1 attributes.</li></ol>
  
  <p>TSR predicted volumes are compared to YSM volumes using the TSR input age 
       adjusted to the year of ground sampling. The left graph plots YSM actual 
       volume (points are joined where re-measurements are available), plus the 
       average of all spatially intersected TSR predicted yield tables (solid blue line). 
       The right graph illustrates the total bias (predicted minus actual volume) 
       at each individual YSM sample location, at the latest measurement. 
       TSR predicted volumes underestimate current YSM volume when the bias is 
       negative, and overestimate current YSM volume when positive.</p></br>")
  
  return(curvoltext)
})


output$comp_curr_vol <- renderUI({
  
  HTML(curvoltext())
  
})


ysmtsr <- reactive({
  req(input$SelectCategory, input$SelectVar)
  Fig12_dat <- ysm_msyt_vdyp_volume %>%
    filter(CLSTR_ID %in% clstr_id_all())
  
  ## Mean predicted volume (aggregated)
  agg_meanvol <- tsr_tass_volproj %>%
    filter(CLSTR_ID %in% clstr_id_all(), 
           AGE %in% c(10, 20, 30, 40, 50, 60)) %>% 
    group_by(AGE) %>%
    summarise(meanvol = mean(volTSR))
  
  p <- ggplot() +
    geom_line(data = agg_meanvol, aes(x= AGE, y=meanvol), col="deepskyblue", linewidth = 3) +
    geom_point(data = Fig12_dat,
               aes(x=ref_age_adj, y=vol_ntwb_ha_nores), col ="red", size = 3) +
    geom_line(data = Fig12_dat,
              aes(x=ref_age_adj, y=vol_ntwb_ha_nores, group= SITE_IDENTIFIER), 
              col="red", linewidth = 1.2) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 60))+ 
    scale_y_continuous(expand = c(0.01, 0), limits = c(-0.01, NA)) + 
    labs(x = "Total Age (yrs)", y = "Net Merch Volume (m3/ha)",
         title = "YSM Sample Remeasurements vs Average of TSR Yield Tables",
         subtitle = "(Spatially matched to each YSM location)") +
    #theme_bw() + 
    theme(
      plot.title = element_text(lineheight = 0.9),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = 'darkgray'), 
      rect = element_blank()
    ) 
  
  return(p)
})


output$age_vs_netmer <- renderPlot({
  
  ysmtsr()
  
})

ysmtsrbias <- reactive({
  req(input$SelectCategory, input$SelectVar)
  Fig13_dat <- ysm_msyt_vdyp_volume %>%
    filter(CLSTR_ID %in% clstr_id()) %>%
    mutate(grdnv = ifelse(is.na(grdnv), 0, grdnv),
           prednv = ifelse(is.na(prednv), 0, prednv),
           tassnv = ifelse(is.na(tassnv), 0, tassnv),
           voldiffTASS = ifelse(is.na(voldiffTASS), 0, voldiffTASS),
           voldiffTSR = ifelse(is.na(voldiffTSR), 0, voldiffTSR)
    )
  
  p <- ggplot() +
    geom_hline(yintercept = 0, col = "darkgray") +
    geom_point(data = Fig13_dat,
               aes(x=ref_age_adj, y=voldiffTSR, col = yt_source_f, shape = yt_source_f), 
               size= 3,, show.legend = TRUE) +
    scale_colour_manual(name = NULL, values=c("Managed" = "red","AGGREGATE"="deepskyblue" ,
                                              "VDYP" =  "green", "VDYP-fill_missed_tsr" = "darkmagenta"),
                        labels = c("TSR TIPSY Opening Specific", "TSR TIPSY Aggregate",
                                   "TSR VDYP", "TSR missed : VDYP filled"), drop = FALSE) +
    scale_shape_manual(name = NULL, 
                       labels = c("TSR TIPSY Opening Specific", "TSR TIPSY Aggregate",
                                  "TSR VDYP", "TSR missed : VDYP filled"),
                       values = c("Managed" =16, "AGGREGATE" =15, "VDYP" = 17, "VDYP-fill_missed_tsr" = 18), drop = FALSE) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 60))+ 
    #scale_y_continuous(expand = c(0.01, 0), limits = c(-0.01, NA))+ 
    labs(x = "Total Age (yrs)", y = "Predicted - Actual (m3/ha)",
         title = "Total volume bias at each YSM location",
         subtitle = "(Predicted TSR yield table volume - Actual YSM volume)") +
    #theme_bw() + 
    theme(
      legend.position="top",
      #panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = 'darkgray'), 
      panel.grid.minor.y = element_line(color = 'darkgray'), 
      panel.grid.minor.x = element_blank(),
      rect = element_blank()
    ) 
  
  return(p)
})



output$vol_bias <- renderPlot({
  
  ysmtsrbias()
})


agetext <- reactive({
  req(input$SelectCategory, input$SelectVar)
  
  agetext <- HTML(paste0("<p>TSR uses the RESULTS age (for managed stands) or VRI age (for unmanaged
stands) as the starting age in timber supply forecasts. This reference
age (adjusted to the year of ground sampling) is critical as it is used
to compare the TSR predicted volume at the reference age against the
actual measured YSM volume. T-tests of the paired age differences (TSR -
YSM) provides a check for significant differences between TSR and YSM
ages (highlighted when significant at ", "alpha=0.05,", " second chart). Note
that YSM sample tree ages may include a combination of both managed and
(sometimes) older residual cohorts depending on the sample tree data
collection criteria; this may increase the average YSM ground age
compared to the TSR reference age.</p>","<p>Results below show that the TSR
reference age is ", "<b>", ifelse(age_p() < 0.05, "different", "not different"), 
                          "</b>", " from YSM ground age.</p></br>"))
  return(agetext)
})


output$age_comp <- renderUI({
  
  agetext()
  
})

ageflex1 <- reactive({
  req(input$SelectCategory, input$SelectVar)
  Fig14_dat <- Fig14_dat()
  
  ### Age summary table
  age_table <- data.frame(attr = c("TSR", "YSM"),
                          obs = rep(dim(Fig14_dat)[1],2),
                          mean = c(round(mean(Fig14_dat$ref_age_adj), 1),
                                   round(mean(Fig14_dat$AGET_TLSO), 1)))
  
  age_table <- flextable(age_table) %>% 
    set_header_labels(values = c("attr.", "# obs", "Mean Age"), lengths = colwidths) %>%
    align(align = "center", part = "header") 
  
  age_table <- age_table %>% add_header_lines(values = c("TSR vs. YSM Age")) %>%
    bold(part = 'header', bold = TRUE) %>%
    autofit()
  
  return(age_table)      
})



output$age_flex1 <- renderUI({
  
  htmltools_value(ageflex1())
  
})

ageflex2 <- reactive({
  req(input$SelectCategory, input$SelectVar)
  Fig14_dat <- Fig14_dat()
  
  ### T test table
  agediff_table <- broom::tidy(t.test(Fig14_dat$age_diff), conf.int = TRUE)
  
  agediff_table <- agediff_table %>%
    select(p.value, estimate, conf.low, conf.high)  %>%
    mutate(p.value = round(p.value, 3),
           estimate = round(estimate, 1),
           conf.low = round(conf.low, 1),
           conf.high = round(conf.high, 1))
  
  agediff_table <- flextable(agediff_table) %>% 
    color(i = ~ p.value < 0.05, j = 1, color = 'red', part = "body") %>%
    set_header_labels(values = c("p-val", "Diff", "L95", "U95"), lengths = colwidths) %>%
    align(align = "center", part = "header") 
  
  
  agediff_table <- agediff_table %>% 
    #add_header_lines(values = c("Age diff (m3/ha/yr)")) %>%
    add_header_lines(values = as_paragraph('Age diff (m',as_sup('3'),'/ha/yr)')) %>%
    bold(part = 'header', bold = TRUE) %>%
    autofit()
  
  return(agediff_table)   
})

  
output$age_flex2 <- renderUI({
  htmltools_value(ageflex2())
})


ageplot <- reactive({
  req(input$SelectCategory, input$SelectVar)
  Fig14_dat <- Fig14_dat()
  
  all_test<-t.test(Fig14_dat$age_diff)
  mean_CI<-(all_test$conf.int[2]+all_test$conf.int[1])/2
  d <- data.frame(x = rep(0, 3), 
                  y = c((all_test$conf.int[2] + all_test$conf.int[1]) / 2,all_test$conf.int[1],all_test$conf.int[2]),
                  val = c("y", "ymin", "ymax"))
  
  
  p <- ggplot(d, aes(x = x, y = y)) + 
    geom_hline(yintercept = 0, linetype = 2, size =1.2, col = "darkgray") +
    geom_line(linewidth = 1.2, col = "steelblue") +
    geom_text(aes(label = round(y,1)), col = "steelblue", 
              position = position_dodge(.9),  vjust = -1, size = 5) +
    geom_point(aes(x, y), size = 4, col = "steelblue")+
    xlim(-.1, .1) +
    ylim(floor(min(d$y)/5)*5, ceiling(max(d$y)/5)*5) +
    coord_flip() +
    labs(x = "", y = "Years",
         title = "TSR - YSM Mean Age Difference & 95% CI") +
    #theme_bw() + 
    theme(
      axis.ticks = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.y = element_blank(), axis.ticks.x = element_blank(),
      rect = element_blank()
    ) 
  
  return(p)
})


output$age_diff <- renderPlot({
  
  ageplot()
  
})

paitext <- reactive({
  req(input$SelectCategory, input$SelectVar)
  test1_comment <- test1_comment()
  test2_comment <- test2_comment()
  
  paitext <- HTML( paste0("<p>Periodic annual increment (PAI) in units of m<sup>3</sup>/ha/yr, is computed from
all re-measured YSM ground samples, and compared against predicted PAI
from TSR yield tables and from YSM TASS projections, separately over the
same re-measurement period. Paired T-tests check for significant
differences in PAI (highlighted when significant at ", 
               "alpha=0.05,",
               " middle chart). The first test helps evaluate if the TSR growth assumptions from
bare ground are in line with actual YSM growth rates. The second test
provides an accuracy assessment of TASS projections that start from an
existing tree list, compared to actual YSM growth rates.</p> ",
               
               "<p>Results of test 1 (TSR yield tables vs. YSM) show ", test1_comment,"</p>",
               
               "<p>Results of test 2 (TASS tree list projection vs. YSM) show ", test2_comment, "</p></br>"))
  return(paitext)
})



output$pai_comp <- renderUI({
  
  paitext()
  
})


tsrpai1 <- reactive({
  req(input$SelectCategory, input$SelectVar)
  Fig15_dat <- Fig15_dat()
  
  ### TSR PAI summary table
  tsr_pai_table1 <- data.frame(attr = c("YSM", "TSR"),
                               obs = rep(dim(Fig15_dat)[1],2),
                               Yrs = c(round(mean(Fig15_dat$year_dff),0), round(mean(Fig15_dat$year_dff),0)),
                               PAI = c(round(mean(Fig15_dat$grdnv_pai), 2),
                                       round(mean(Fig15_dat$prednv_pai), 2)))
  
  tsr_pai_table1 <- flextable(tsr_pai_table1) %>% 
    set_header_labels(values = c("attr.", "# obs", "Yrs", "PAI"), lengths = colwidths) %>%
    align(align = "center", part = "header") 
  
  tsr_pai_table1 <- tsr_pai_table1 %>% 
    add_header_lines(values = c("YSM vs. TSR MSYTs")) %>%
    bold(part = 'header', bold = TRUE) %>%
    set_caption(as_paragraph(
      as_b(as_chunk("Test 1: Compare YSM actual growth vs. TSR yield table projections")))) %>%
    autofit()
  
  return(tsr_pai_table1)   
})


output$tsr_pai_flex1 <- renderUI({
  
  htmltools_value(tsrpai1())
})

tsrpai2 <- reactive({
  req(input$SelectCategory, input$SelectVar)
  Fig15_dat <- Fig15_dat()
  
  ### T test table
  if (nrow(Fig15_dat) > 1){
    
    tsr_pai_table2 <- broom::tidy(t.test(Fig15_dat$prednv_pai - Fig15_dat$grdnv_pai), conf.int = TRUE)
    
  } else {
    tsr_pai_table2 <- data.frame(matrix(rep("-", 4), ncol=4,nrow=1, 
                                        dimnames=list(NULL, c("p.value", "estimate", "conf.low", "conf.high"))))
  }
  
  tsr_pai_table2 <- tsr_pai_table2 %>%
    select(p.value, estimate, conf.low, conf.high)  %>%
    mutate(p.value = ifelse(is.numeric(p.value), round(p.value, 3),p.value),
           estimate = ifelse(is.numeric(estimate), round(estimate, 3),estimate),
           conf.low = ifelse(is.numeric(conf.low), round(conf.low, 3),conf.low),
           conf.high = ifelse(is.numeric(conf.high), round(conf.high, 3),conf.high))
  
  tsr_pai_table2 <- flextable(tsr_pai_table2) %>% 
    color(i = ~ p.value < 0.05, j = 1, color = 'red', part = "body") %>%
    set_header_labels(values = c("p-val", "Diff", "L95", "U95"), lengths = colwidths) %>%
    align(align = "center", part = "header") 
  
  tsr_pai_table2 <- tsr_pai_table2 %>% 
    add_header_lines(values = as_paragraph('TSR-YSM diff(m',as_sup('3'),'/ha/yr)')) %>%
    bold(part = 'header', bold = TRUE) %>%
    autofit()
  
  return(tsr_pai_table2)   
})


output$tsr_pai_flex2 <- renderUI({
  
  htmltools_value(tsrpai2())
})


tasspai1 <- reactive({
  req(input$SelectCategory, input$SelectVar)
  Fig15_dat <- Fig15_dat()
  
  ### TASS PAI summary table
  tass_pai_table1 <- data.frame(attr = c("YSM", "TASS"),
                                obs = rep(dim(Fig15_dat)[1],2),
                                Yrs = c(round(mean(Fig15_dat$year_dff),0), round(mean(Fig15_dat$year_dff),0)),
                                PAI = c(round(mean(Fig15_dat$grdnv_pai), 2),
                                        round(mean(Fig15_dat$tass_pai), 2)))
  
  tass_pai_table1 <- flextable(tass_pai_table1) %>% 
    set_header_labels(values = c("attr.", "# obs", "Yrs", "PAI"), lengths = colwidths) %>%
    align(align = "center", part = "header") 
  
  tass_pai_table1 <- tass_pai_table1 %>% 
    add_header_lines(values = c("YSM vs. TASS projections")) %>%
    bold(part = 'header', bold = TRUE) %>%
    set_caption(as_paragraph(
      as_b(as_chunk("Test 2: Compare YSM actual growth vs. TASS YSM tree list projections")))) %>%
    autofit()
  
  return(tass_pai_table1)   
})



output$tass_pai_flex1 <- renderUI({
  
  htmltools_value(tasspai1())
})

tasspai2 <- reactive({
  req(input$SelectCategory, input$SelectVar)
  Fig15_dat <- Fig15_dat()
  
  ### T test table
  if (nrow(Fig15_dat) > 1){
    tass_pai_table2 <- broom::tidy(t.test(Fig15_dat$tass_pai - Fig15_dat$grdnv_pai), conf.int = TRUE)
  } else {
    tass_pai_table2 <- data.frame(matrix(rep("-", 4),ncol=4,nrow=1, 
                                         dimnames=list(NULL, c("p.value", "estimate", "conf.low", "conf.high"))))
  }
  
  tass_pai_table2 <- tass_pai_table2 %>%
    select(p.value, estimate, conf.low, conf.high)  %>%
    mutate(p.value = ifelse(is.numeric(p.value), round(p.value, 3),p.value),
           estimate = ifelse(is.numeric(estimate), round(estimate, 3),estimate),
           conf.low = ifelse(is.numeric(conf.low), round(conf.low, 3),conf.low),
           conf.high = ifelse(is.numeric(conf.high), round(conf.high, 3),conf.high))
  
  tass_pai_table2 <- flextable(tass_pai_table2) %>% 
    color(i = ~ p.value < 0.05, j = 1, color = 'red', part = "body") %>%
    set_header_labels(values = c("p-val", "Diff", "L95", "U95"), lengths = colwidths) %>%
    align(align = "center", part = "header") 
  
  tass_pai_table2 <- tass_pai_table2 %>% 
    add_header_lines(values = as_paragraph('TASS-YSM diff(m',as_sup('3'),'/ha/yr)')) %>%
    bold(part = 'header', bold = TRUE) %>%
    autofit()
  
  return(tass_pai_table2)   
})


output$tass_pai_flex2 <- renderUI({
  
  htmltools_value(tasspai2())
})


tassdiff <- reactive({
  req(input$SelectCategory, input$SelectVar)
  Fig15_dat <- Fig15_dat()
  
  if (nrow(Fig15_dat) > 1){
    test1<-t.test(Fig15_dat$prednv_pai - Fig15_dat$grdnv_pai)
    test2<-t.test(Fig15_dat$tass_pai - Fig15_dat$grdnv_pai)
    mean_CI_1<-(test1$conf.int[2]+test1$conf.int[1])/2
    mean_CI_2<-(test2$conf.int[2]+test2$conf.int[1])/2
    d <- data.frame(x1 = rep(1, 3),
                    x2 = rep(0, 3),
                    y1 = c((test1$conf.int[2] + test1$conf.int[1]) / 2,test1$conf.int[1],test1$conf.int[2]),
                    y2 = c((test2$conf.int[2] + test2$conf.int[1]) / 2,test2$conf.int[1],test2$conf.int[2]),
                    val = c("y", "ymin", "ymax"))
  } else {
    d <- data.frame(x1 = rep(NA, 3),
                    x2 = rep(NA, 3),
                    y1 = rep(NA, 3),
                    y2 = rep(NA, 3),
                    val = c("y", "ymin", "ymax"))
    test1<-NA
    test2<-NA
  }
  
  p <- if (nrow(Fig15_dat) > 1){ ggplot(d) + 
      geom_point(aes(x = x1, y = y1, col = "steelblue"), size = 4)+
      geom_line(aes(x = x1, y = y1, col = "steelblue"), linewidth = 1.2) +
      geom_text(aes(x = x1, y = y1, label = round(y1,1), col = "steelblue"), 
                position = position_dodge(.9),  vjust = -1, size = 5, show.legend = FALSE) +
      geom_point(aes(x = x2, y = y2, col = "#B4464B"), size = 4)+
      geom_line(aes(x = x2, y = y2, col = "#B4464B"), linewidth = 1.2) +
      geom_text(aes(x = x2, y = y2, label = round(y2,1), col = "#B4464B"), 
                position = position_dodge(.9),  vjust = -1, size = 5, show.legend = FALSE) +
      geom_hline(yintercept = 0, linetype = 2, size =1.2, col = "darkgray") +
      scale_color_manual(name = NULL, values = c( "#B4464B","steelblue"),
                         guide = guide_legend(override.aes = list(linetype=c(1,1),
                                                                  linewidth = c(1.2,1.2),
                                                                  size = c(4,4))),
                         labels = c("TASS - YSM","TSR - YSM")) +
      xlim(-1, 2) +
      ylim(floor(min(d$y1, d$y2)/5)*5, ceiling(max(d$y1, d$y2)/5)*5) +
      coord_flip() +
      labs(x = "", y = expression(m^3~"/ha/yr"),
           title = "PAI Mean Difference & 95% CI") +
      theme(
        legend.position = "left",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(),
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


output$pai_diff <- renderPlot({
  
  tassdiff()
})
