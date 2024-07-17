###############################################
#
# Define server for the Shiny app
#
##############################################
server <- function(input, output, session) {
  
  observeEvent(input$SelectCategory,{
    freezeReactiveValue(input, "SelectVar")
    updateSelectInput(session,'SelectVar',
                      choices = if(input$SelectCategory == "TSA_DESC") {
                        c(Choose = "", tsa_list)} else {
                          c(Choose = "", bec_list )
                        }
                      )
  })
  
  # Call reactive values and data
  source(file.path("server scripts/reactServer.R"), local = TRUE) 
  
  # Source files with server code for each tab -----------------------------------------
  source(file.path("server scripts/overviewServer.R"), local = TRUE)$value 
  source(file.path("server scripts/ysdServer.R"), local = TRUE)$value 
  source(file.path("server scripts/lsServer.R"), local = TRUE)$value 
  source(file.path("server scripts/residServer.R"), local = TRUE)$value 
  source(file.path("server scripts/siServer.R"), local = TRUE)$value 
  source(file.path("server scripts/tsrServer.R"), local = TRUE)$value 
  source(file.path("server scripts/fhServer.R"), local = TRUE)$value 
  source(file.path("server scripts/tassServer.R"), local = TRUE)$value 
  source(file.path("server scripts/gnServer.R"), local = TRUE)$value 
  
}

## END 