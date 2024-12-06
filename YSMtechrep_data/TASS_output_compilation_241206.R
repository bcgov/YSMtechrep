################################################################################
### Load library
################################################################################
library(data.table)
library(dplyr)
library(DT)
library(tibble)
library(tidyverse)

################################################################################
### TASS output compilation - this may take hours
################################################################################

### Path to TASS output folder 
tasspath <- "/TASS_output"
### Four subfolders
case <- c("xyNrustN", "xyYrustN", "xyYrustY", "xyNrustY")

### Create list of outputs in folders
tassrun_list <- c()

for (i in case){
  temp_list<-list.files(path = paste0(tasspath, "/", i), 
                        pattern = "*.txt", 
                        full.names = T, ignore.case = T, include.dirs = T)
  temp_list<- temp_list[-grep("list",temp_list)]
  
  tassrun_list <- c(tassrun_list, temp_list)
  
}


### Append all outputs in the list
tass_output<-data.frame()

### Only leave the file names without path
tassrun_files<-c()

for (i in 1:length(tassrun_list)){
  FILE_ID<-i
  FILE_PATH<-dirname(tassrun_list[i])
  FILE_NAME<-basename(tassrun_list[i])
  
  try({  
    tassrun_1<- read.delim(paste0(FILE_PATH, "/",FILE_NAME), header = F)
    
    RESID<-ifelse(grepl("res", FILE_NAME, fixed = TRUE), "Y", "N")
    SPECIES<-ifelse(gsub(".*summ*","\\1", gsub(".txt.*","\\1", FILE_NAME))=="", "ALL",
                    gsub(".*summ*","\\1", gsub(".txt.*","\\1", FILE_NAME)))
    TASS_VER<-paste0("TASS", gsub(".*TASS (.+) Custom.*", "\\1", tassrun_1[1,]))
    
    CLSTR_ID<-gsub(".*clstr_id: ", "\\1", tassrun_1[2,])
    
    datarow<-which(tassrun_1[,1] %like% "--")+1
    tassrun_df<-data.frame(matrix(scan(text=tassrun_1[datarow:nrow(tassrun_1),]), 
                                  nrow=length(datarow:nrow(tassrun_1)), ncol=8, byrow=T))
    names(tassrun_df)<-c("Year","Age","Stems","BA","WSV","GMV","DBHQ", "Top_HT")
    
    tass_df<-cbind(FILE_PATH, FILE_NAME, FILE_ID, CLSTR_ID, SPECIES, RESID, tassrun_df, TASS_VER)
    
    tass_output<-rbind(tass_output, tass_df)
  })
}

tass_output <- tass_output %>%
  mutate(SITE_IDENTIFIER = as.numeric(str_extract(CLSTR_ID, "[^-]+")),
         VISIT_NUMBER = as.numeric(str_sub(CLSTR_ID, -1,-1)),
         TASS_ver = as.numeric(str_sub(TASS_VER, 6, 6)),
         xy = str_sub(FILE_PATH, -6,-6),
         rust = str_sub(FILE_PATH, -1,-1))


### Set data export location
savepath <- "/YSMtechrep/shiny_app/data"
### Save data for application
saveRDS(tass_output, paste0(savepath, "tass_output.rds"))

