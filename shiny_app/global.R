#-------------------------------------------------------------------------------------------------
# Copyright 2018 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.
#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
# Load packages
#-------------------------------------------------------------------------------------------------
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet.extras)
library(leaflet)
library(sf)
library(plotly)
library(ggplot2)
library(scales)
library(bcmaps)
library(data.table)
library(dplyr)
library(gridExtra)
#library(knitr)
library(flextable)
library(tibble)
library(tidyverse)
library(forcats)
library(RColorBrewer)
#library(basemaps)
#library(openxlsx)
#library(RODBC)
#-------------------------------------------------------------------------------------------------
#Functions for retrieving data from the postgres server (vector, raster and tables)
#-------------------------------------------------------------------------------------------------
css_hide_errors <- function() {
  css_add("
.shiny-output-error {visibility: hidden;}
.shiny-output-error:before {visibility: hidden;}
")
}


css_add <- function(x) {
  shiny::tags$head(shiny::tags$style(shiny::HTML(x)))
}

waiter_html <- function(x){
  tagList(waiter::spin_chasing_dots(),
          br2(),
          h3(x))
}

br2 <- function() tagList(br(), br())

list_subset <- function(sampledata,
                        strata,
                        title) { 
  # If object "sampledata" exists, use it. Otherwise import from csv file.
  if (exists("sampledata")){
    sample_data <- sampledata
  } else {
    sample_data <- fread(sampledata)
  }
  # Assign report unit.
  if (strata == "BEC"){
    rep_unit <- sample_data$BEC_ZONE
  } else if (strata == "TSA"){
    rep_unit <- sample_data$TSA_DESC
  } else {
    stop("strata muse be either BEC or TSA.")
  }
  
  lookup_table <- sample_data %>%
    mutate(UNIT = rep_unit) %>%
    filter(
      UNIT %in% title, 
      TFL == ""
    )
  
  site_id <- unique(lookup_table$SITE_IDENTIFIER)
  
  clstr_id <- lookup_table %>% 
    filter(LAST_MSMT == "Y") %>% 
    pull(CLSTR_ID)
  
  clstr_id_all <- unique(lookup_table$CLSTR_ID)
  
  clstr_id_last2 <- lookup_table %>% 
    group_by(SITE_IDENTIFIER) %>%
    filter(n() > 1) %>% 
    arrange(VISIT_NUMBER) %>% 
    slice_tail(n = 2) %>%
    pull(CLSTR_ID)
  
  list_ysm <- list(site_id, clstr_id, clstr_id_all, clstr_id_last2)
  names(list_ysm) <- c("site_id", "clstr_id", "clstr_id_all", "clstr_id_last2")
  
  return(list_ysm)
}

##Data objects
sample_data <- readRDS('C:/Users/HYEWOO/Documents/R/YSMtechrep/shiny_app/data/sample_data.rds')
sp_samplePoints <- st_transform(st_as_sf(sample_data, coords=c("BC_ALBERS_X","BC_ALBERS_Y"), crs = 3005),4326)

tsa_sp <- st_transform(st_read("C:/Users/HYEWOO/Documents/R/YSMtechrep/shiny_app/data/tsa_sp.shp"),4326)

i <- sapply(sp_samplePoints, is.factor)

sp_samplePoints[i] <- lapply(sp_samplePoints[i], as.character)

# Variable
decidspc <- c('A','AC','ACT','ACB','AT',
              'DM','DR','E','EA','EP','EW',
              'MB','MV','KC','RA','V','VB','VP','VV',
              'W','WB','WP','WT','ZH','XH','XC')

#   
#----------------
#Non-Spatial 
siteage_data <- readRDS('C:/Users/HYEWOO/Documents/R/YSMtechrep/shiny_app/data/siteage_data.rds')
socs_data <- readRDS('C:/Users/HYEWOO/Documents/R/YSMtechrep/shiny_app/data/spcs_data.rds')
summary_data <- readRDS('C:/Users/HYEWOO/Documents/R/YSMtechrep/shiny_app/data/summary_data.rds')
regen_data <- readRDS('C:/Users/HYEWOO/Documents/R/YSMtechrep/shiny_app/data/regen_data.rds')
tree_fh_data <- readRDS('C:/Users/HYEWOO/Documents/R/YSMtechrep/shiny_app/data/tree_fh_data.rds')
vegcomp_pspl_sample <- readRDS('C:/Users/HYEWOO/Documents/R/YSMtechrep/shiny_app/data/vegcomp_pspl_sample.rds')
ysm_msyt_vdyp_volume <- readRDS('C:/Users/HYEWOO/Documents/R/YSMtechrep/shiny_app/data/ysm_msyt_vdyp_volume.rds')
tsr_tass_volproj <- readRDS('C:/Users/HYEWOO/Documents/R/YSMtechrep/shiny_app/data/tsr_tass_volproj.rds')

dummyData <- head(subset(as.data.frame(sp_samplePoints), select = c(PROJ_AGE_ADJ, MEAS_YR)),1)
dummyData[,c("PROJ_AGE_ADJ", "MEAS_YR" )] <- 0