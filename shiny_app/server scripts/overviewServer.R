## Server logic for Overview tab ----

###############################################.
## Indicator definitions ----
###############################################.
#Subsetting by domain 

output$overview <- renderUI({
  
  gridsize <- sample_data %>% 
    filter(SITE_IDENTIFIER %in% site_id()) %>% 
    select(GRID_SIZE) %>% 
    distinct() %>% 
    pull()
  
  gridsize <- gridsize[length(gridsize)]
  
  HTML(
    paste0("Young Stand Monitoring (YSM) programs have been established 
across a number of management units in BC. This handout provides a 
high-level technical summary of results compiled by FAIB for the YSM program in ",
           "<b>", title(),"</b>", ".", '<br/>','<br/>',
           "The target population for TSA-based monitoring programs is defined as
15-50 year old Crown forested stands in the Vegetation Resources
Inventory (VRI) Vegcomp rank 1 layer. TFL-based monitoring programs may
use other criteria (eg., harvest history).", '<br/>','<br/>',
           "Ground samples (dots on map, below) are established on a ", 
           "<b>",gridsize,"</b>"," grid, with trees tagged in 0.04ha permanent plots with a planned
five-year re-measurement cycle.", '<br/>','<br/>',
           "Some key YSM objectives are to: describe the characteristics and
structure of young stands, report on forest health, assess the accuracy
of predicted attributes and spatial coverages, and compare against
growth models to help evaluate if young stands will meet future timber
supply expectations.", '<br/>','<br/>',
           "The TSA map (right) includes the source of site index found in the 
latest Provincial Site Productivity Layer (PSPL) : either TEM/PEM & 
SIBEC, or Biophysical Site Index Model.
")
  )
  
})



output$plotgraph <- renderLeaflet({
  
  if(!is.null(site_id())){
    
    location <- sample_data %>% 
      filter(SITE_IDENTIFIER %in% site_id()) %>% 
      group_by(SITE_IDENTIFIER) %>% 
      mutate(visit_num = length(VISIT_NUMBER),
             visit_year = paste0(MEAS_YR, collapse  = ',')) %>%
      select(SITE_IDENTIFIER, SAMPLE_ESTABLISHMENT_TYPE, visit_num, visit_year, #BEClabel,
             TSA_DESC, BEC_ZONE, BEC_SBZ, BEC_VAR, GRID_SIZE,
             BC_ALBERS_X, BC_ALBERS_Y) %>% 
      distinct()
    
    location <- st_as_sf(x = location,                         
                         coords = c("BC_ALBERS_X", "BC_ALBERS_Y"),
                         crs = 3005)
    
    location <- st_transform(location , crs = 4326)
    
    if(input$SelectCategory == "TSA_DESC"){
      
      tsa_sub <- tsa_sp %>%
        filter(TSA_NUMBER %in% substr(unique(sample_data[sample_data$SITE_IDENTIFIER %in% site_id(),]$MGMT_UNIT), 4, 5))
      
      lng1 = as.numeric(st_bbox(tsa_sub)[1])
      lat1 = as.numeric(st_bbox(tsa_sub)[2])
      lng2 = as.numeric(st_bbox(tsa_sub)[3])
      lat2 = as.numeric(st_bbox(tsa_sub)[4])
      
    } else {
      tsa_sub <- tsa_sp
      lng1 = -139.06
      lat1 = 48.30
      lng2 = -114.03
      lat2 = 60.00
    }
    
    
    leaflet() %>% 
      addTiles() %>% 
      addProviderTiles("Esri.WorldImagery", group = "Satellite view") %>%
      addProviderTiles("Esri.WorldTerrain", group = "Terrain only") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Base map") %>%
      setMaxBounds(lng1 = -142,
                   lat1 = 46, 
                   lng2 = -112,
                   lat2 =  62) %>%
      fitBounds(lng1 = lng1, lat1 = lat1, lng2 = lng2, lat2 = lat2) %>%
      addLayersControl(
        baseGroups = c("Base map", "Terrain only", "Satellite view"),
        options = layersControlOptions(collapsed = FALSE),
      ) %>%
      addPolygons(data = tsa_sub, stroke = TRUE, color = "#3c8dbc", weight = 2,
                  opacity = 0.9, fill = TRUE, fillOpacity = 0.2) %>%
      addCircleMarkers(data = location,
                       radius = 5, stroke = FALSE, fillOpacity = 1,
                       popup = paste(sep = "<br/>",
                                     paste(paste("<b>Sample ID</b> - ", location$SITE_IDENTIFIER, "<br/>"),
                                           paste("<b>Sample Type</b> - ", location$SAMPLE_ESTABLISHMENT_TYPE, "<br/>"),
                                           paste("<b>BEC zone</b> - ", location$BEC_ZONE, "<br/>"), 
                                           paste("<b>BEC subzone</b> - ", location$BEC_SBZ, "<br/>"),
                                           paste("<b>BEC variant</b> - ", location$BEC_VAR, "<br/>"), 
                                           paste("<b># of measures</b> - ", location$visit_num, "<br/>"),
                                           paste("<b>Visited year</b> - ",location$visit_year, "<br/>")))
      )   
  }
}

)



output$flex <- renderUI({
  
  if(!is.null(site_id())){
    
    table1_dat <- sample_data %>%
      filter(SITE_IDENTIFIER %in% site_id()) %>%
      mutate(meas_yr_adj = ifelse(month(MEAS_DT) >= 7, MEAS_YR, MEAS_YR - 1))
    
    flextable1 <- proc_freq(table1_dat, "VISIT_NUMBER", "meas_yr_adj",
                            include.row_total = F,
                            include.row_percent = F,
                            include.column_percent = F,
                            include.table_percent = F) 
    
    flextable1 <- labelizor(
      x = flextable1, 
      part = "header", 
      labels = c("VISIT_NUMBER" = "Meas #", 
                 "meas_yr_adj" = "# Ground Samples by Year\n (end of growing season)")) %>%
      autofit()
    
    return(flextable1 %>%
             htmltools_value())      
  }
  #
})


output$key_finding <- renderUI({
  
  projectiontable <- projectiontable()
  max_row = which.max(abs(projectiontable$meanvoldiff/projectiontable$meanvol_tass*100))
  
  maxvoldiff = projectiontable$percvoldiff[max_row]
  ageatmaxvoldiff = projectiontable$AGE[max_row]
  Significant = ifelse(projectiontable$pval[max_row] <0.05, "Yes", "No")
  TSRbias1 = ifelse(projectiontable$meanvoldiff[max_row] < 0, "Conservative", "Optimistic")
  TSRbias2 = ifelse(Significant == "No", "No", TSRbias1)
  
  HTML(paste0("<h3>Summary of Key Findings for Existing Young Stands in ",
              title(), " related to Timber Supply</h3>","</br>",
              
              "<ol><li>The leading species from YSM ground samples is compared to the 
    interpreted leading species in the Vegetation Resources Inventory forest 
    inventory coverage (VRI). The leading species percent agreement is: ", 
    "<b><font color='#FF0000'>" ,correct_ls(), "</b> (% agreement)</font></li>",
    
    "<li>Species composition from YSM ground samples is compared against 
    TSR inputs (ie., regeneration assumptions used in modeling existing stands 
    in Timber Supply Review [TSR]). The overall species composition overlap is: ", 
    "<b><font color='#FF0000'>", round(percoverlap()*100, 0), "</b> (% overlap)</font></li>",
    
    "<li>The Provincial Site Productivity Layer (PSPL), one of the TSR 
    inputs used for modeling existing managed stands, is assessed for bias 
    from YSM ground based site index data. If significant, site index bias 
    (in percent) is listed by species, where a positive percent is an 
    under-estimate in the PSPL, and a negative percent is an over-estimate: ", 
    "<b><font color='#FF0000'>", si_bias(), "</b> </font></li>",
    
    "<li>YSM sample measurements include conifer and deciduous tree species. 
    The deciduous proportion in the YSM samples (% of total volume) is: ",
    "<b><font color='#FF0000'>", decid_vol(), "</b> (% of m<sup>3</sup>/ha)</font></li>",
    
    "<li>YSM sample measurements include separate tracking of both managed 
    vs. residual cohorts. The residual proportion in the YSM samples 
    (% of total volume) is: ", "<b><font color='#FF0000'>",
    round(fig6_sum(), 0), "</b> (% of m<sup>3</sup>/ha)</font></li>",
    
    "<li>The periodic annual increment (PAI) of TSR yield tables are compared 
    against re-measured YSM samples over the same remeasurement period, to test 
    if TSR projections are significantly different from YSM growth rates: ",
    "<b><font color='#FF0000'> TSR is ", 
    ifelse(is.na(test1()), "-", ifelse(test1() > 0, "over", "under")), 
 "estimating actual growth by ", round(abs(test1()), 1), " m<sup>3</sup>/ha/yr.</b></font></li>",
 
    "<li>The PAI of YSM TASS projections are compared against re-measured 
    YSM samples over the same remeasurement period, to test if TASS projections 
    are significantly different from YSM growth rates: ",
 "<b><font color='#FF0000'> TASS is ", 
 ifelse(is.na(test2()), "-", ifelse(test2() > 0, "over", "under")), 
 "estimating actual growth by ", round(abs(test2()), 1), " m<sup>3</sup>/ha/yr.</b></font></li>",
 
    "<li>For YSM measurements since 2017, the impact from stem rusts can be 
    directly modeled in TASS using GRIM / CRIME. The volume impact of TASS YSM 
    projections by age 100 (in addition to default OAFs) is: ", 
 "<b><font color='#FF0000'> ", year100_inc(), "</b> (% of m<sup>3</sup>/ha)</font></li>",
 
    "<li>To address forest health risks from all other damage agents, an 
    interim simplistic approach is applied to estimate future impacts of all 
    known forest health agents, and results in an impact by age 100 (in addition 
    to default OAFs) of: ", "<b><font color='#FF0000'> ",
 year100_comb(),"</b> (% of m<sup>3</sup>/ha)</font></li>",
 
    "<li>TSR yield tables are compared against YSM TASS projections, to test 
    if TSR assumptions will meet future expectations of young stands by rotation 
    age. Reported outputs include the maximum percent difference of TSR volume 
    relative to YSM (a negative % is where TSR is lower than YSM, positive % is 
    where TSR is greater than YSM); the projected age this occurs at; and a test 
    to determine if the differences between the two is significant. 
 
               </br><font color='#FF0000'> <ul><li><b><i>Max % vol diff</i>",'&emsp;', maxvoldiff, "</b></li>",
               "<li><b><i>Age @max vol diff</i>",'&emsp;', ageatmaxvoldiff, "</b></li>",
               "<li><b><i>Significant?</i>",'&emsp;', Significant, "</b></li>",
               "<li><b><i>TSR bias?</i>", '&emsp;', TSRbias2, "</b></li></font> </br>","</ol>"))
  
})