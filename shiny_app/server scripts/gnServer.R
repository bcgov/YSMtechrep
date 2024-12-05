## Server logic for General Notes tab ----

###############################################.
## Indicator definitions ----
###############################################.
#Subsetting by domain 


#output$ysm_tables <- renderPlot({
#  
#  temp <- sample_data %>%
#    filter(CLSTR_ID %in% clstr_id()) %>%
#    select(SAMPLE_ESTABLISHMENT_TYPE, GRID_SIZE)
#  
#  temp1 <- melt(temp, id.vars=1)
#  temp2 <-temp1 %>%
#    group_by(SAMPLE_ESTABLISHMENT_TYPE, value) %>%
#    summarize(n = n()) 
#  
#  t1 <- flextable(temp2)
#  t1 <- labelizor(
#    x = t1, 
#    part = "header", 
#    labels = c("SAMPLE_ESTABLISHMENT_TYPE" = "Sample Type", 
#               "value" = "Grid (km)",
#               "n" = "Total")) %>%
#    autofit()
#  
#  
#  t2 <- proc_freq(ysm_msyt_vdyp_volume %>%
#                    filter(CLSTR_ID %in% clstr_id()) %>%
#                    mutate(yt_source_f = factor(yt_source, 
#                                                levels = c("TSR VDYP" = "VDYP", 
#                                                           "TSR TIPSY Opening Specific" = "Managed" , 
#                                                           "TSR TIPSY Aggregated" ="AGGREGATE",
#                                                           "Excluded" = "Excluded"),
#                                                labels = c("TSR VDYP", "TSR TIPSY Opening Specific", 
#                                                           "TSR TIPSY Aggregated", "Excluded")))
#                  , "yt_source_f",
#                  #main = "# Ground Samples by Year (end of growing season)",
#                  include.row_percent = F,
#                  include.column_percent = F,
#                  include.table_percent = F) 
#  
#  t2 <- t2 %>%
#    add_header_lines(values = "TSR Yield Table Assignment \nCurrent Regime", top = T) %>%
#    delete_rows(i = 2, part = "header") %>%
#    merge_at(i = 1, j = 1:3, part = "header") %>%
#    autofit()
#  
#  
#  t3 <- proc_freq(ysm_msyt_vdyp_volume  %>%
#                    filter(CLSTR_ID %in% clstr_id()) %>%
#                    mutate(occupancy = factor(ifelse(vol_wsv_ha != 0, "Treed", "Empty"),
#                                              levels = c("Treed", "Empty")))
#                  , "occupancy",
#                  #main = "# Ground Samples by Year (end of growing season)",
#                  include.row_percent = F,
#                  include.column_percent = F,
#                  include.table_percent = F) 
#  
#  t3 <- t3 %>%
#    add_header_lines(values = "YSM Plot Occupancy \nof trees >=4cm DBH", top = T) %>%
#    delete_rows(i = 2, part = "header") %>%
#    merge_at(i = 1, j = 1:3, part = "header") %>%
#    autofit()
#  
#  
#  t4 <- proc_freq(ysm_msyt_vdyp_volume  %>%
#                    filter(CLSTR_ID %in% clstr_id()) %>%
#                    mutate(xy_f = factor(xy, levels = c("Y", "N"),
#                                         labels = c("Stem mapped plots", "Not stem mapped")))
#                  , "xy_f",
#                  #main = "# Ground Samples by Year (end of growing season)",
#                  include.row_percent = F,
#                  include.column_percent = F,
#                  include.table_percent = F) 
#  
#  t4 <- t4 %>%
#    add_header_lines(values = "Availability of stem mapped YSM \nSamples used in TASS Projections", top = T) %>%
#    delete_rows(i = 2, part = "header") %>%
#    merge_at(i = 1, j = 1:3, part = "header") %>%
#    autofit()
#  
#  
#  t5 <- proc_freq(ysm_msyt_vdyp_volume  %>%
#                    filter(CLSTR_ID %in% clstr_id())  %>%
#                    mutate(TASS_ver_f = factor(TASS_ver, levels = c(2, 3),
#                                               labels = c("TASS ver. 2", "TASS ver. 3")))
#                  , "TASS_ver_f",
#                  #main = "# Ground Samples by Year (end of growing season)",
#                  include.row_percent = F,
#                  include.column_percent = F,
#                  include.table_percent = F) 
#  
#  t5 <- t5 %>%
#    add_header_lines(values = "TASS Version used \nfor YSM Projections", top = T) %>%
#    delete_rows(i = 2, part = "header") %>%
#    merge_at(i = 1, j = 1:3, part = "header") %>%
#    autofit()
#  
#  t1 = gen_grob(t1)
#  t2 = gen_grob(t2)
#  t3 = gen_grob(t3)
#  t4 = gen_grob(t4)
#  t5 = gen_grob(t5)
#  
#  p <- grid.arrange(t1, t2, t3, t4, t5, ncol = 2)
#  
#  p
#  
#})


output$ysm_tables1 <- renderUI({
  
  if (input$SelectCategory == "TSA_DESC"){
  temp <- sample_data %>%
    filter(CLSTR_ID %in% clstr_id()) %>%
    select(SAMPLE_ESTABLISHMENT_TYPE, GRID_SIZE)
  
  temp1 <- melt(temp, id.vars=1)
  temp2 <-temp1 %>%
    group_by(SAMPLE_ESTABLISHMENT_TYPE, value) %>%
    summarize(n = n()) 
  
  t1 <- flextable(temp2)
  t1 <- labelizor(
    x = t1, 
    part = "header", 
    labels = c("SAMPLE_ESTABLISHMENT_TYPE" = "Sample Type", 
               "value" = "Grid (km)",
               "n" = "Total")) %>%
    bold(part = 'header', bold = TRUE) %>%
    autofit()
  
  } else {
    
    t1 <- proc_freq(sample_data %>%
                      filter(CLSTR_ID %in% clstr_id()), 'MGMT_UNIT')
    t1 <- labelizor(
      x = t1, 
      part = "header", 
      labels = c("MGMT_UNIT" = "Management Unit (TSA & TFL)")) %>%
      bold(part = 'header', bold = TRUE) %>%
      autofit()
    
  }
  return(t1 %>%
           htmltools_value())
})


output$ysm_tables2 <- renderUI({
  
  t2 <- proc_freq(ysm_msyt_vdyp_volume %>%
                    filter(CLSTR_ID %in% clstr_id()), "yt_source_f",
                  #main = "# Ground Samples by Year (end of growing season)",
                  include.row_percent = F,
                  include.column_percent = F,
                  include.table_percent = F) 
  
  t2 <- t2 %>%
    add_header_lines(values = "TSR Yield Table Assignment \nCurrent Regime", top = T) %>%
    delete_rows(i = 2, part = "header") %>%
    merge_at(i = 1, j = 1:3, part = "header") %>%
    bold(part = 'header', bold = TRUE) %>%
    autofit()
  
  return(t2 %>%
           htmltools_value())
  
})


output$ysm_tables3 <- renderUI({
  
  t3 <- proc_freq(ysm_msyt_vdyp_volume  %>%
                    filter(CLSTR_ID %in% clstr_id()) %>%
                    mutate(occupancy = factor(ifelse(vol_wsv_ha != 0, "Treed", "Empty"),
                                              levels = c("Treed", "Empty")))
                  , "occupancy",
                  #main = "# Ground Samples by Year (end of growing season)",
                  include.row_percent = F,
                  include.column_percent = F,
                  include.table_percent = F) 
  
  t3 <- proc_freq(LD_dat()  %>%
                    #filter(CLSTR_ID %in% clstr_id()) %>%
                    mutate(occupancy = factor(ifelse(SPC_GRP1 =="Nonstock", "Empty", "Treed"),
                                              levels = c("Treed", "Empty")))
                  , "occupancy",
                  #main = "# Ground Samples by Year (end of growing season)",
                  include.row_percent = F,
                  include.column_percent = F,
                  include.table_percent = F) 
  
  t3 <- t3 %>%
    add_header_lines(values = "YSM Plot Occupancy \nof trees >=4cm DBH", top = T) %>%
    delete_rows(i = 2, part = "header") %>%
    merge_at(i = 1, j = 1:3, part = "header") %>%
    bold(part = 'header', bold = TRUE) %>%
    autofit()
  
  return(t3 %>%
           htmltools_value())
  
})


output$ysm_tables4 <- renderUI({
  
  t4 <- proc_freq(ysm_msyt_vdyp_volume  %>%
                    filter(CLSTR_ID %in% clstr_id()) %>%
                    mutate(xy_f = factor(xy, levels = c("Y", "N"),
                                         labels = c("Stem mapped plots", "Not stem mapped")))
                  , "xy_f",
                  #main = "# Ground Samples by Year (end of growing season)",
                  include.row_percent = F,
                  include.column_percent = F,
                  include.table_percent = F) 
  
  t4 <- t4 %>%
    add_header_lines(values = "Availability of stem mapped YSM \nSamples used in TASS Projections", top = T) %>%
    delete_rows(i = 2, part = "header") %>%
    merge_at(i = 1, j = 1:3, part = "header") %>%
    bold(part = 'header', bold = TRUE) %>%
    autofit()
  
  return(t4 %>%
           htmltools_value())
  
})


output$ysm_tables5 <- renderUI({
  
  t5 <- proc_freq(ysm_msyt_vdyp_volume  %>%
                    filter(CLSTR_ID %in% clstr_id())  %>%
                    mutate(TASS_ver_f = factor(TASS_ver, levels = c(2, 3),
                                               labels = c("TASS ver. 2", "TASS ver. 3")))
                  , "TASS_ver_f",
                  #main = "# Ground Samples by Year (end of growing season)",
                  include.row_percent = F,
                  include.column_percent = F,
                  include.table_percent = F) 
  
  t5 <- t5 %>%
    add_header_lines(values = "TASS Version used \nfor YSM Projections", top = T) %>%
    delete_rows(i = 2, part = "header") %>%
    merge_at(i = 1, j = 1:3, part = "header") %>%
    bold(part = 'header', bold = TRUE) %>%
    autofit()
  
  return(t5 %>%
           htmltools_value())
  
})




output$sp_dam_header <- renderUI({
  
  HTML(paste0("<h3>Tree Species and Damage Agents Recorded from YSM Samples in ", title(),"</h3>"))
  
})






output$sp_dam_header <- renderUI({
  
  HTML(paste0("<h3>Tree Species and Damage Agents Recorded from YSM Samples in ", title(),"</h3>"))
  
})


output$sp_table <- renderDT({
  
  sp_cd <- tree_fh_data  %>%
    filter(CLSTR_ID %in% clstr_id(), S_F == "S") %>%
    pull(SPECIES) %>%
    unique()
  
  spcd1 <- spcd %>%
    filter(species %in% sp_cd)
  
  names(spcd1) <- c("Code", "Name")
  
  datatable(spcd1, rownames= FALSE)
  
})


output$dam_table <- renderDT({
  
  dam_agn <- tree_fh_data  %>%
    filter(CLSTR_ID %in% clstr_id(), S_F == "S") %>%
    pull(AGN) %>%
    unique()
  
  damcd1 <- damcd %>%
    filter(dam_agna %in% dam_agn)
  
  names(damcd1) <- c("Code", "Name")
  
  datatable(damcd1, rownames= FALSE)
  
})


output$assumps <- renderUI({
  
  HTML("
       <p>* TSR existing MSYTs are based on FAIB ver. 2022MAR08 using TIPSY 4.4, with RESULTS opening age, species density, regen type and 
genetic worth, and based on spatially matched RESULTS opening ID where present, or based on aggregated RESULTS input data where 
Opening ID is unavailable. TSR existing NSYTs are generated by running VDYP7 batch console together with the published 2022 VRI, for 
YSM sample locations modeled as 'existing unmanaged'. Each YSM GPS location is intersected with the published 2022 VRI and linked to 
each TSR yield table by the FEATURE_ID attribute.</p>

<p>* YSM samples that collectively comprise at least 80% PL & SW combined use TASS 3 (ver 3.01.21) to project yields. YSM samples that 
include all other species combinations use TASS 2 (ver. 2.07.76) to project yield. The rationale is that Tipsy 4.4 base data consists of 
TASS3-derived yield tables for PL & SW, and TASS2-derived yield tables for all other species.</p>

<p>* TASS projections of each YSM ground sample plot use all available tree data collected at each measurement. This includes individual tree 
stem mapping, species, DBH and height from all tagged trees, site index by species from all available site trees (including site height and 
site age), forest health damage and severity (up to 5 per tree), plus specific stem rust damage agent data collected to permit running TASS 
GRIM/CRIME modules. The TASS-required height vigour coefficients are computed as a ratio of each individual tree height divided by the 
compiled site height of the same species. Separate tracking of both managed and residual cohorts from the YSM tree list is maintained 
throughout the entire TASS projection.
</p>

<p>* For those YSM samples that are stem-mapped, TASS input tree lists are generated from each sample by first clipping hexagon shaped 
plots from the circular 0.04ha main plot (tagged trees >9cm DBH) and the 0.01ha subplot (tagged trees 4-9cm DBH), replicating each 
hexagon and randomly orienting and superimposing both main and subplots onto an expanded 1ha stem-mapped tree list comprising all 
tagged trees >=4cm DBH. For those YSM samples not yet stem-mapped, TASS assumes a random spatial pattern of the tagged trees.
</p>

<p>* When comparing future yield expectations at rotation between spatially matched TSR-derived yield tables and YSM TASS projections, the 
same reference age is used, which is either based on the age of the intersecting RESULTS opening ID if present, otherwise from the 
intersecting VRI VEGCOMP poly rank1 projected age attribute (adjusted to year of ground sampling). To ensure consistency with 
assumptions in the TSR MSYTs, the future yield expectations at rotation of YSM TASS projections exclude the residual cohort.
</p>

<p>* All YSM TASS projections include standard OAF1 (15%) and OAF2 (5%) adjustments, with OAF1 applied in a non-standard way by 
assigning no OAF1 at the YSM sample's current age and volume, and linearly increasing OAF1 so that 100% of its value is achieved by 
rotation age (assumed as 80yrs).</p>

<p>* TASS projections of YSM samples measured since 2017 include additional GRIM/CRIME modeled impacts (ie., height & %encirclement 
data are measured for each observed stem rust). The impact of all other recorded damage agents are approximated as having either 
immediate impact (@90%) or incremental impact (@2.5%/decade), and modeled as additional reduction factors. The subset of damage 
agents estimated as having immediate or incremental impact on future yields, were identified following discussions with provincial 
government forest health specialists.</p>

<p>* Potential site index (used to compare against YSM ground-based site index) originates from the Provincial Site Productivity Layer (PSPL) 
ver 7.0 intersected with each YSM GPS location. Note these potential site index estimates are used as inputs for the TSR-MSYTs.
</p>

<p>* YSM leading species is compared to the VRI VEGCOMP leading species for an assessment of leading species agreement. YSM overall 
species composition is compared to the RESULTS species composition used in TSR MSYTs (which includes planted or natural cohorts).
</p>

<p>* Stand table & species composition histograms are computed as pooled (weighted) averages of all YSM samples combined.</p>

<p>* Forest health incidence histograms are computed as pooled (weighted) averages, together with 95% confidence intervals computed using 
the 'variance of a ratio estimator'. Current incidence summaries for a given damage agent are based on all possible occurrences recorded 
for a given tree, which can be up to 5 damage agents per tree. Therefore, current incidence of all damage agents can together add up to 
more than 100%. Conversely, when summarizing change in forest health incidence over time, only the primary damage agent is included; 
therefore, the total change in average incidence will add up to 100%.</p>

<p>* Components of change are computed from individual tree changes (ie., survivor, ingrowth, mortality) between the last two measurements 
only. Trees initially tagged as subplot trees (4-9 cm DBH) and subsequently as main plot trees (>9 cm DBH) have their 'per-hectare-factors' 
(PHF) fixed relative to the first measurement.</p>

<p>*All reported net merchantable volumes are at 12.5cm close utilization for PL, 17.5cm for other species, and include all species (deciduous 
+ conifer)
</p>
</br>
")
  
  
})



output$refs <- renderUI({
  
  HTML("<p><b>YSM Technical Handouts for all available TSAs, plus resource information on Provincial Monitoring Programs</b></br>
  <a href='https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/ground-sample-inventories/provincial-monitoring'>
  https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/ground-sample-inventories/provincial-monitoring</a>
</br><p><b>Public access to Ground Sample Data approved as OpenData under BC Open Government Licence</b></br>
<a href='https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/ground-sample-inventories/provincial-monitoring'>
https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/ground-sample-inventories/provincial-monitoring</a></p>
")
  
})
