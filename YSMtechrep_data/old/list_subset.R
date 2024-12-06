library(data.table)
library(dplyr)

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

