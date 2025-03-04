## Server logic for General Notes tab ----

###############################################.
## Indicator definitions ----
###############################################.

t1 <- reactive({
  req(input$SelectCategory, input$SelectVar)
  
  if (input$SelectCategory == "TSA_DESC"){
    t1_1 <- sample_data %>%
      filter(CLSTR_ID %in% clstr_id()) %>%
      select(SAMPLE_ESTABLISHMENT_TYPE, GRID_SIZE)
    
    t1_2 <- reshape2::melt(t1_1, id.vars=1)
    t1_3 <-t1_2 %>%
      group_by(SAMPLE_ESTABLISHMENT_TYPE, value) %>%
      summarize(n = n()) 
    
    t1 <- flextable(t1_3)
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
  return(t1)
})


output$ysm_tables1 <- renderUI({
  
  htmltools_value(t1())
})


t2 <- reactive({
  req(input$SelectCategory, input$SelectVar)
  t2 <- proc_freq(ysm_msyt_vdyp_volume %>%
                    filter(CLSTR_ID %in% clstr_id()) %>%
                    mutate(yt_source_label = fct_recode(yt_source_f, 
                                                        "TSR TIPSY Opening Specific" = "Managed",
                                                        "TSR TIPSY Aggregate" ="AGGREGATE",
                                                        "TSR VDYP" = "VDYP" ,
                                                        "TSR missed : VDYP filled" = "VDYP-fill_missed_tsr")), 
                  "yt_source_label",
                  #main = "# Ground Samples by Year (end of growing season)",
                  include.row_percent = F,
                  include.column_percent = F,
                  include.table_percent = F) 
  
  t2 <- t2 %>%
    add_header_lines(values = "TSR Current Yield Table Regime \nver: (MSYT_Ver4_Delivery)**", top = T) %>%
    delete_rows(i = 2, part = "header") %>%
    merge_at(i = 1, j = 1:3, part = "header") %>%
    bold(part = 'header', bold = TRUE) %>%
    autofit()
  
  return(t2)
})


output$ysm_tables2 <- renderUI({
  
  htmltools_value(t2())
  
})

t3 <- reactive({
  req(input$SelectCategory, input$SelectVar)
  #t3 <- proc_freq(ysm_msyt_vdyp_volume  %>%
  #                  filter(CLSTR_ID %in% clstr_id()) %>%
  #                  mutate(occupancy = factor(ifelse(vol_wsv_ha != 0, "Treed", "Empty"),
  #                                            levels = c("Treed", "Empty")))
  #                , "occupancy",
  #                #main = "# Ground Samples by Year (end of growing season)",
  #                include.row_percent = F,
  #                include.column_percent = F,
  #                include.table_percent = F) 
  
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
  
  return(t3)
})



output$ysm_tables3 <- renderUI({
  
  htmltools_value(t3())
  
})

t4 <- reactive({
  req(input$SelectCategory, input$SelectVar)
  t4 <- proc_freq(ysm_msyt_vdyp_volume  %>%
                    filter(CLSTR_ID %in% clstr_id()) %>%
                    mutate(xy_f = ifelse(is.na(xy), "Empty", xy),
                           xy_f = factor(xy_f, levels = c("Y", "N", "Empty"),
                                         labels = c("Stem mapped plots", "Not stem mapped", "Empty")))
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
  
  return(t4)
})



output$ysm_tables4 <- renderUI({
  
  htmltools_value(t4())
  
})

t5 <- reactive({
  req(input$SelectCategory, input$SelectVar)
  t5 <- proc_freq(ysm_msyt_vdyp_volume  %>%
                    filter(CLSTR_ID %in% clstr_id())  %>%
                    mutate(TASS_ver = ifelse(is.na(TASS_ver), 1, TASS_ver),,
                           TASS_ver_f = factor(TASS_ver, levels = c(2, 3, 1),
                                               labels = c("TASS ver. 2", "TASS ver. 3", "Empty")))
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
  
  return(t5)
})



output$ysm_tables5 <- renderUI({
  
  htmltools_value(t5())
  
})



output$sp_dam_header <- renderUI({
  
  HTML(paste0("<h3>Tree Species and Damage Agents Recorded from YSM Samples in ", title(),"</h3>"))
  
})


spcdtab <- reactive({
  req(input$SelectCategory, input$SelectVar)
  sp_cd <- tree_fh_data  %>%
    filter(CLSTR_ID %in% clstr_id(), S_F == "S") %>%
    pull(SPECIES) %>%
    unique()
  
  spcd1 <- spcd %>%
    filter(species %in% sp_cd)
  
  names(spcd1) <- c("Code", "Name")
  
  return(spcd1)
})



output$sp_table <- renderDT({
  datatable(spcdtab(), rownames= FALSE)
})

damcdtab <- reactive({
  req(input$SelectCategory, input$SelectVar)
  dam_agn <- tree_fh_data  %>%
    filter(CLSTR_ID %in% clstr_id(), S_F == "S") %>%
    pull(AGN) %>%
    unique()
  
  damcd1 <- damcd %>%
    filter(dam_agna %in% dam_agn)
  
  names(damcd1) <- c("Code", "Name")
  
  return(damcd1)
})


output$dam_table <- renderDT({
  datatable(damcdtab(), rownames= FALSE)
})


assumptext <- reactive({
  req(input$SelectCategory, input$SelectVar)
  
  assumptext <- paste0('<p>* TSR existing MSYTs are based on FAIB MSYT ver. 4 using TIPSY 4.4, 
       with RESULTS opening age, species density, regen type and genetic worth, 
       and based on spatially matched RESULTS opening ID where present, or based 
       on aggregated RESULTS input data where Opening ID is unavailable. TSR 
       existing NSYTs are generated by running VDYP7 batch console together with 
       the latest published 2023 VRI, for YSM sample locations modeled as 
       "existing unmanaged". Each YSM GPS location is linked to each TSR yield 
       table by FEATURE_ID attribute from the the previous 2022 VRI (ie., the 
       VRI version used for the latest TSR MSYTs).</p>
       
<p>* YSM samples that collectively comprise at least 80% PL & SW combined 
use TASS 3 (ver 3.01.21) to project yields. YSM samples that include all 
other species combinations use TASS 2 (ver. 2.07.76) to project yield. 
The rationale is that Tipsy 4.4 base data consists of TASS3-derived yield 
tables for PL & SW, and TASS2-derived yield tables for all other species.</p>

<p>* TASS projections of each YSM ground sample plot use all available 
tree data collected at each measurement. This includes individual tree 
stem mapping, species, DBH and height from all tagged trees, site index 
by species from all available site trees (including site height and site 
age), forest health damage and severity (up to 5 per tree), plus specific 
stem rust damage agent data collected to permit running TASS GRIM/CRIME 
modules. The TASS-required height vigour coefficients are computed as a 
ratio of each individual tree height divided by the average height by 
species from a subset of trees representing the tallest 100 live trees/ha 
(to ensure the height vigour of the site trees approximates 1.0). All 
residual trees are assigned a default height vigour of 0.8. Separate 
tracking of both managed and residual cohorts from the YSM tree list is 
maintained throughout the entire TASS projection.</p>

<p>* For those YSM samples that are stem-mapped, TASS input tree lists 
are generated from each sample by first clipping hexagon shaped plots 
from the circular 0.04ha main plot (tagged trees >9cm DBH) and the 0.01ha 
subplot (tagged trees 4-9cm DBH), replicating each hexagon and randomly 
orienting and superimposing both main and subplots onto an expanded 1ha 
stem-mapped tree list comprising all tagged trees >=4cm DBH. For those YSM 
samples not yet stem-mapped, TASS assumes a random spatial pattern of the 
tagged trees.</p>

<p>* When comparing future yield expectations at rotation between 
spatially matched TSR-derived yield tables and YSM TASS projections, 
the same reference age is used, which is either based on the age of the 
intersecting RESULTS opening ID if present, otherwise from the 
intersecting VRI VEGCOMP poly rank1 projected age attribute (adjusted to 
year of ground sampling). To ensure consistency with assumptions in the 
TSR MSYTs, the future yield expectations at rotation of YSM TASS 
projections exclude the residual cohort.</p>

<p>* All YSM TASS projections include standard OAF1 (15%) and OAF2 (5%) 
adjustments, with OAF1 applied in a non-standard way by assigning no OAF1 
at the YSM samples current age and volume, and linearly increasing OAF1 
so that 100% of its value is achieved by rotation age (assumed as 80yrs).</p>

<p>* TASS projections of YSM samples measured since 2017 include 
additional GRIM/CRIME modeled impacts (ie., height & %encirclement data 
are measured for each observed stem rust). The impact of all other 
recorded damage agents are approximated as having either immediate impact 
(@90%) or incremental impact (@2.5%/decade), and modeled as additional 
reduction factors. The subset of damage agents estimated as having 
immediate or incremental impact on future yields, were identified 
following discussions with provincial government forest health 
specialists.</p>

<p>* Potential site index (used to compare against YSM ground-based 
site index) originates from the Provincial Site Productivity Layer 
(PSPL) ver 7.0 intersected with each YSM GPS location. Note these 
potential site index estimates are used as inputs for the TSR-MSYTs.</p>

<p>* YSM leading species is compared to the VRI VEGCOMP leading species 
for an assessment of leading species agreement. YSM overall species 
composition is compared to the RESULTS species composition used in TSR 
MSYTs (which includes planted or natural cohorts).</p>

<p>* Stand table & species composition histograms are computed as 
pooled (weighted) averages of all YSM samples combined for a given 
management unit.</p>

<p>* Forest health incidence histograms are computed as pooled 
(weighted) averages, together with 95% confidence intervals computed 
using the "variance of a ratio estimator". Current incidence summaries 
for a given damage agent are based on all possible occurrences recorded 
for a given tree, which can be up to 5 damage agents per tree. Therefore, 
current incidence of all damage agents can together add up to more than 
100%. Conversely, when summarizing change in forest health incidence over 
time, only the primary damage agent is included; therefore, the total 
change in average incidence will add up to 100%.</p>

<p>* Components of change are computed from individual tree changes 
(ie., survivor, ingrowth, mortality) between the last two measurements 
only. Trees initially tagged as subplot trees (4-9 cm DBH) and 
subsequently as main plot trees (>9 cm DBH) have their 
"per-hectare-factors" (PHF) fixed relative to the first measurement.</p>

<p>* All reported net merchantable volumes are at 12.5cm close 
utilization for PL, 17.5cm for other species, and include all 
species (deciduous + conifer).</p></br>')
  
  return(assumptext)
})

output$assumps <- renderUI({
  
  HTML(assumptext())
  
})


reftext <- reactive({
  req(input$SelectCategory, input$SelectVar)
  
  reftext <- HTML("<p><b>YSM Technical Handouts for all available TSAs, plus 
  resource information on Provincial Monitoring Programs</b></br>
  <a href='https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/ground-sample-inventories/provincial-monitoring'>
  https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/ground-sample-inventories/provincial-monitoring</a>
  </br><p><b>Public access to Ground Sample Data approved as OpenData under 
  BC Open Government Licence</b></br>
<a href='https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/ground-sample-inventories/provincial-monitoring'>
https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/ground-sample-inventories/provincial-monitoring</a></p>
<p><b>Github repository</b></br><a href='https://github.com/bcgov/YSMtechrep'</a></p>
")
  return(reftext)
})


output$refs <- renderUI({
  
  reftext()
  
})
