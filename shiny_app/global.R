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
#library(webshot)
library(reshape2)


# 2. required datafiles ------------------------------------------------------------

# main datasets 
sample_data <- readRDS("data/sample_data.rds")
sample_data <- sample_data %>%
  dplyr::mutate(across(where(~ inherits(.x, "IDate")), as.Date))
spcs_data <- readRDS("data/spcs_data.rds")
siteage_data <- readRDS("data/siteage_data.rds")
smtr_data <- readRDS("data/smtr_data.rds")
SI_data <- readRDS("data/SI_data.rds")
tree_fh_data <- readRDS("data/tree_fh_data.rds")
regen_data <- readRDS("data/regen_data.rds")
vegcomp_pspl_sample <- readRDS("data/vegcomp_pspl_sample.rds")
ysm_msyt_vdyp_volume <-readRDS("data/ysm_msyt_vdyp_volume.rds")
tsr_tass_volproj <-readRDS("data/tsr_tass_volproj.rds")
tass_sp <-readRDS("data/tass_sp.rds")


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


db_levels <- c("0", "5", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55", "60+") 


#3. lists for filter dropdowns ------------------------------------------------------

# for TSA selection
tsa_list <- sample_data %>%
  filter(TSA_filter == "Y") %>%
  count(TSA_DESC) %>%
  filter(n >= 10) %>%
  pull(TSA_DESC) %>%
  sort() %>%
  unique()

# for BEC subzone selection
bec_list <- sample_data %>%
  filter(BEC_filter == "Y") %>%
  count(BECsub) %>%
  filter(n >= 10) %>%
  pull(BECsub) %>%
  sort() %>%
  unique()

# for BEC selection
beczone_list <- sample_data %>%
  filter(BEC_filter == "Y") %>%
  count(BEC_ZONE) %>%
  filter(n >= 10) %>%
  pull(BEC_ZONE) %>%
  sort() %>%
  unique()


# 4. chart themes  ----------------------------------------------------------------

# common parameters for plots
theme_set(theme_bw(15, base_family = 'Arial'))
#theme_set(theme_bw(15) + theme(panel.grid.major = element_line(colour = "gray")))


# colour palettes for plots
tree_colors <- c("BA" = "#99600F", "BG" = "#B3823E", "BL" = "#CCAA7A",
                 "HM" = "#54990F", "HW" = "#78B33E", "YC" = "#990F26", "HR" = "#B33E52",
                 "JR" = "#A6763D", "UP" = "#FB6A4A", "EP" = "#C79E00",
                 "CW" = "#CC7A88", "TW" = "#E6B8BF", "DE" = "#F3C300", "Decid" = "#F3C300", "FD" = "#AA4499",
                 "LA" = "#FDBF6F", "LW" = "#FFCC80", "LT" = "#F39C12",
                 "PA" = "#0F8299", "PL" = "#3E9FB3", "PW" = "#7ABECC", "PY" = "#B8DEE6",
                 "SB" = "#3D0F99", "SE" = "#653EB3", "SS" = "#967ACC", "SW" = "#C7B8E6",
                 "XC" = "#9E9E9E", "Other" = "#9E9E9E")

species_order <- sort(names(tree_colors))


tass_colors <- c("Acb"= "#E0B200", "At"= "#FFD84D",  "Ba" = "#99600F",  "Bl"= "#CCAA7A",  
                 "Cwc" = "#CC7A88", "Cwi" = "#E6B8BF", "Dr" = "#F2A900", "Ep"= "#C79E00" , 
                 "Fdc" = "#AA4499" ,"Fdi" = "#DD77CC",
                 "Hm"  = "#54990F", "Hwc" = "#78B33E" ,"Hwi" = "#99DD55",
                 "Lw"  = "#FFCC80" ,"Pli"  = "#3E9FB3","Pw"  = "#7ABECC" ,"Py"  = "#B8DEE6",
                 "Sb" = "#3D0F99" ,"Se" = "#653EB3" , "Ss"= "#967ACC",  "Sw"= "#C7B8E6")



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

# --- Extract legend from p1 ---
g_legend <- function(a.gplot){
  tmp <- ggplotGrob(a.gplot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  tmp$grobs[[leg]]
}

## custom progress function
#myProgress <- function(total, labels) {
#  list(update = function(i) {
#    shiny::setProgress(value = i/total, message = paste0("Running chunk ", i, "/", total, ": "), detail = labels[i])
#    cat(sep = "", i, "/", total, ": ", labels[i], "\n")
#  }, done = function() {
#    shiny::setProgress(1, message = "Done")
#  })
#}
#
## register it with knitr
#op <- options(knitr.progress.fun = myProgress)


##END