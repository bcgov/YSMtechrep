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
                        c(Choose = "", tsa_list)
                        } else if (input$SelectCategory == "BECsub"){
                          c(Choose = "", bec_list )
                        } else if (input$SelectCategory == "BEC_ZONE"){
                          c(Choose = "", beczone_list )
                        }
                      )
  })

  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0('YSM_report_', title(), "_", Sys.Date(), switch(
        input$format, PDF = '.pdf', HTML = '.html')
      )
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      ## Create a Progress object
      #progress <- shiny::Progress$new()
      ## Make sure it closes when we exit this reactive, even if there's an error
      #on.exit(progress$close())
      #progress$set(message = "Creating report", value = 10)
      
      out <- rmarkdown::render('report.Rmd', switch(
        input$format,
        HTML = rmarkdown::html_document(),
        PDF = rmarkdown::pdf_document()
        #PDF = rmarkdown::pandoc_convert(rmarkdown::render('report.Rmd', "html_document"), output = 'report.pdf')
      ))
      
      file.rename(out, file)
    }
  )
  
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