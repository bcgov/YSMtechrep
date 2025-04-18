###############################################################################
#
# Global script ---- 
#
###############################################################################

# contains :- 

# 1. required packages
# 2. required datafiles
# 3. lists for dashboard filters
# 4. common chart themes
# 5. extra UI components that are not reactive (cookie box/guided tours/updates modal)
# 6. sourcing functions created for app (see functions folder) 



# 1. required packages ----------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(data.table)
library(ggplot2)
#library(plotly)
library(dplyr)
options(dplyr.summarize.inform = FALSE)
library(sf)
library(leaflet)
library(DT)
#library(rmarkdown)
#library(knitr)
library(flextable)
set_flextable_defaults(font.family = "Arial")
library(tibble)
library(tidyverse)
library(scales)
library(gridExtra)
library(waiter)
library(tinytex)
library(kableExtra)
library(pandoc)
library(pagedown)
library(htmltools)
library(webshot)


# 2. required datafiles ------------------------------------------------------------

# main datasets 
sample_data <- readRDS("data/sample_data.rds")
spcs_data <- readRDS("data/spcs_data.rds")
siteage_data <- readRDS("data/siteage_data.rds")
SI_data <- readRDS("data/SI_data.rds")
tree_fh_data <- readRDS("data/Tree_FH_data.rds")
regen_data <- readRDS("data/regen_data.rds")
vegcomp_pspl_sample <- readRDS("data/vegcomp_pspl_sample.rds")
ysm_msyt_vdyp_volume <-readRDS("data/ysm_msyt_vdyp_volume.rds")
tsr_tass_volproj <-readRDS("data/tsr_tass_volproj.rds")


# lookups 
spcd<-readRDS("data/spcd.rds")
damcd<-readRDS("data/damcd.rds")


# shapefiles (for map) 
tsa_sp <- st_transform(st_read("data/tsa_sp.shp"),4326)
becmap <- st_transform(st_read("data/becmap.shp"),4326)
beczonemap <- st_transform(st_read("data/beczone_lowres1000.shp"),4326)


# SPCD for deciduous 
decidspc <- c('A','AC','ACT','ACB','AT',
              'DM','DR','E','EA','EP','EW',
              'MB','MV','KC','RA','V','VB','VP','VV',
              'W','WB','WP','WT','ZH','XH','XC')



#3. lists for filter dropdowns ------------------------------------------------------

# for TSA selection
tsa_list <- sort(unique(sample_data %>% 
                          filter(TSA_filter == "Y") %>% 
                          group_by(TSA_DESC) %>% 
                          filter(n() > 10) %>% 
                          pull(TSA_DESC)))

# for BEC subzone selection
bec_list <- sort(unique(sample_data %>% 
                          filter(BEC_filter == "Y") %>% 
                          group_by(BECsub) %>% 
                          filter(n() > 10) %>% 
                          pull(BECsub)))

# for BEC selection
beczone_list <- sort(unique(sample_data %>% 
                          filter(BEC_filter == "Y") %>% 
                          group_by(BEC_ZONE) %>% 
                          filter(n() > 10) %>% 
                          pull(BEC_ZONE)))


# 4. chart themes  ----------------------------------------------------------------
theme_set(theme_bw(15, base_family = 'Arial'))
#theme_set(theme_bw(15) + theme(panel.grid.major = element_line(colour = "gray")))


# colour palettes for plots

# common parameters for plots

# 5. extra UI components  ----------------------------------------------------------


# 6. sourcing functions created for app (see functions folder) -------------------------------
#list.files("functions") %>% 
#  map(~ source(paste0("functions/", .)))
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


##END