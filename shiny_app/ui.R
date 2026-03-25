## UI
# Use a fluid Bootstrap layout
ui <- dashboardPage(
  
  title = "YSM Technical Report", # Browser title
  
  dashboardHeader(tags$li(a(href = 'https://gov.bc.ca',
                            img(src = 'logo-banner.png',
                                title = "Home", height = "41px"),
                            style = "padding-top:10px; padding-bottom:10px;
                            background-color: #036; margin-right: 10px;"),
                          class = "dropdown"),
                  title = div('YSM Technical Report', style = "color: white; font-weight: bold; font-size: 24px;
                              font-family: 'BCSans', 'Noto Sans', Verdana, Arial, sans-serif;
                              padding-top:10px;")
    ),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    
    tags$head(tags$style("body{min-height: 800px;  height: auto;  max-width: 1296px;  margin: auto;
                         background-color: #b3b1b3}")),
  fluidPage(    
    
    waiter::use_waiter(),
    
    # BC gov custom css
    includeCSS("www/bcgov2.css"),
    
    # Overwrite shinydashboard color
    tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {background-color: #036; width: 800px;}  
        .skin-blue .main-header .logo:hover {background-color: #036;}
        .skin-blue .main-header .navbar {background-color: #036; margin-left: 100px;}
        
        .well {background-color: #fff;  border: 1px solid #5a7dab; border-radius: 4px;
        -webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,.05);
        box-shadow: inset 0 1px 1px rgba(0,0,0,.05);}
        
        .navbar-brand {color:#38598a;}
        
        .content-wrapper, .right-side {background-color: #FFFFFF;}
        
        .shiny-options-group { 
          /*height: 100px;*/
          width: 600px;
          -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
          -moz-column-count: 2;    /* Firefox */ 
            
          row-count: 2;
          -webkit-column-fill: auto;
          -moz-column-fill: auto;
          column-fill: auto;
          margin-top: 0px;
        } 
        
        .control-label {
          padding-bottom: 5px;
        }
        
        div.radio {
          margin-top: 5px;
          margin-bottom: 0px;
          padding-bottom: 5px;
        }
        
        a:hover {
          color: #4b5e7e !important;
        }
        
        footer ul li a:hover {
          color: #FFFFFF !important;
        }
      '))),
    
  box(title = "Select the area of interest", 
      solidHeader = TRUE, status = "primary", width = NULL,
      
      column(3, 
             list(tags$div(align = 'left', 
                           class = 'multicol', 
                           radioButtons("SelectCategory", "Strata",
                                        choices = list("By TSA" = "TSA_DESC", 
                                                       "By BEC subzone" = "BECsub",
                                                       "By BEC zone" = "BEC_ZONE")
                           ), style = "font-size:100%")), align = "center"
      ), 
      
      column(3, offset = 1, selectInput(inputId = "SelectVar",
                                        label = "Select",
                                        choices = NULL), 
             HTML("<font size='-1'>*only n&ge;10 are selectable.</font>")),
      
      column(3, offset = 1, downloadButton("downloadReport", "Download report"), br(),
             radioButtons("format", "Document format", c("HTML"), inline = TRUE)
      )
      
  ), # selection box
  
  column(12, 
         
   navlistPanel(
           
    "Overview",
    tabPanel(title = "Overview",
             uiOutput('overview_header'),
             uiOutput("overview"),
             br(),
             withSpinner(leafletOutput("plotgraph", height = "600px")),
             br(),
             uiOutput("overviewflex"),
             br()
    ),
    
    tabPanel(title = "Summary of Key Findings",
             withSpinner(uiOutput("key_finding"))
    ),
    
    "Young Stand Description",
    tabPanel(title = "Stand Summaries",
             uiOutput("young_stand_description"),
             br(),
             withSpinner(uiOutput("stand_summary_flex")),
             br(),
             div(
             plotOutput("live_sp", width = "700px"),
             br(),
             plotOutput("bec_dist", width = "500px"),
             br(),
             plotOutput("stock_table", width = "700px"),
             br(),
             plotOutput("stock_table_stem", width = "700px"),
             br(),
             plotOutput("smalltree", width = "500px"), align = "center"),
    br(),
    ),
    
    tabPanel(title = "Leading Species",
             h3("Leading Species vs. VRI, & Overall Species Composition vs. TSR Inputs"),
             uiOutput("leading_sp"),
             br(),
             uiOutput("leading_sp_flex"),
             br(),
             div(withSpinner(plotOutput("spc_comp", width = "700px")), align = "center"),
             br(),
    ),
    tabPanel(title = "Residual Trees",
             h3("Post-Harvest Regenerated vs. Residual Trees"),
             uiOutput("residual"),
             br(),
             div(withSpinner(plotOutput("residual_ysm", width = "600px")), align = "center"),
             br(),
    ),
    
    "Site Index",
    tabPanel(title = "Provincial Site Productivity Layer",
             h3("Site Index vs. Provincial Site Productivity Layer"),
             uiOutput("site_index_pspl"),
             br(),
             withSpinner(uiOutput("si_pspl_flex")),
             br(),
    ),
    tabPanel(title = "Trends in Site Index Estimates",
             h3("Trends in Site Index Estimates over Time"),
             uiOutput("trend_si"),
             br(),
             div(withSpinner(plotOutput("si_trend", width = "700px")), align = "center"),
             br(),
    ),
    
    "Comparison with TSR",
    tabPanel(title = "Current Volumes",
             h3("Comparing Current Volumes: TSR Predicted Yield Tables vs. YSM Actual Measurements"),
             uiOutput("comp_curr_vol"),
             br(),
             div(plotOutput("age_vs_netmer", width = "700px"),
             br(),
             withSpinner(plotOutput("vol_bias", width = "700px")), align = "center"),
             br(),
    ),
    tabPanel(title = "Stand Age",
             h3("Test to Compare TSR Total Age vs. YSM Ground Sample Age"),
             uiOutput("age_comp"),
             br(),
             fluidRow(
               column(6,
                      uiOutput("age_flex1")),
               column(6,
                      uiOutput("age_flex2")),
             ),
             br(),
             fluidRow(
               column(12, align = "center", 
                      withSpinner(plotOutput("age_diff", height = "200px", width = "400px")))
             ),
             br(),
    ),      
    tabPanel(title = "Periodic Annual Increment",
             h3("Test to Compare Modeled vs. YSM Re-measured Periodic Annual Increment"),
             uiOutput("pai_comp"),
             br(),
             fluidRow(
               column(6,
                      uiOutput("tsr_pai_flex1"),
                      br(),
                      uiOutput("tsr_pai_flex2"),
                      br()),
               column(6,
                      uiOutput("tass_pai_flex1"),
                      br(),
                      uiOutput("tass_pai_flex2"),
                      br())
             ),
             br(),
             div(withSpinner(plotOutput("pai_diff", height = "200px", width = "600px")), align = "center"),
             br(),
    ),  
    
    "Forest Health",
    tabPanel(title = "Growth and Mortality",
             h3("Quantifying Change in Growth and Mortality"),
             uiOutput("quant_coc"),
             br(),
             div(withSpinner(plotOutput("coc_chart", width = "600px")), align = "center"),
             br(),
    ),
    tabPanel(title = "Current Forest Health Incidence",
             h3("Current Forest Health Incidence"),
             uiOutput("health_inci"),
             br(),
             div(withSpinner(plotOutput("curr_fh_inci")), align = "center"),
             br(),
    ),
    tabPanel(title = "Change in Forest Health Incidence",
             h3("Comparing Change in Forest Health Incidence"),
             uiOutput("comp_coc"),
             br(),
             div(withSpinner(plotOutput("change_dam")), align = "center"),
             br(),
             uiOutput("fh_trees"),
             br(),
             uiOutput("fh_trees_flex"),
             br(),
    ), 
    tabPanel(title = "Future Forest Health Risks",
             h3("Approximating Future Forest Health Risks"),
             uiOutput("future_fh"),
             br(),
             div(withSpinner(plotOutput("dam_immed", height = "300px", width = "800px")), align = "center"),
             br(),
    ), 
    
    "YSM TASS Projection",
    tabPanel(title = "Will Existing Young Stands Meet Expectations at Rotation?",
             h3("Will Existing Young Stands Meet Expectations at Rotation?"),
             uiOutput("tass_tsr"),
             br(),
             withSpinner(plotOutput("tass_tsr_netvol", width = "800px")),
             br(),
             plotOutput("tass_tsr_netvol_sp", width = "800px"),
             br(),
             plotOutput("tass_tsr_netvol_sp_prop", width = "800px"),
             br(),
             fluidRow(
               column(6,
                      uiOutput("tasstable_flex")),
               column(6,
                      uiOutput("culmtable_flex"))
             ),
             br(),
    ),
    tabPanel(title = "YSM TASS projections vs. TSR Predicted Yield Tables",
             h3("YSM TASS projections vs. TSR Predicted Yield Tables"),
             uiOutput("tass_tsr_test"),
             br(),
             withSpinner(uiOutput("tass_tsr_volproj")),
             br(),
    ),
    
    "General Notes",
    #tabPanel(title = "Total number of YSM samples",
    #         h3("Total number of YSM samples by:"),
    #         plotOutput("ysm_tables"),
    #),
    tabPanel(title = "Total number of YSM samples",
             h3("Total number of YSM samples by:"),
             br(),
             
             fluidRow(align = 'center',
                      column(6,
                             uiOutput("ysm_tables1"),
                             uiOutput("ysm_tables3"),
                             uiOutput("ysm_tables5")),
                      column(6,
                             uiOutput("ysm_tables2"),
                             uiOutput("ysm_tables4")))
    ),
    tabPanel(title = "Tree Species and Damage Agents",
             uiOutput('sp_dam_header'),
             fluidRow(
               column(width = 6,
                      h4("Tree Species Codes / Names"),
                      DT::dataTableOutput("sp_table")),
               column(width = 6,
                      h4("Damage Agent Codes / Names"),
                      DT::dataTableOutput('dam_table'))
             )
    ),
    tabPanel(title = "General Notes / Assumptions / References",
             h3("General Notes / Assumptions / References"),
             #textOutput("deploymentDate"),
             uiOutput("deploymentDate"),
             br(),
             uiOutput("assumps")
    ),
    #tabPanel(title = "References",
    #         h3("References"),
    #         uiOutput("refs")
    #),
    
  ),  # navlistPanel 
  br(),
  
  ), # navlist column
  br(),
  
), #fluidPage

br(),
br(),
br(),
div(class = "footer",
        includeHTML("footer.html")
    )
) # dashboardBody

) # dashboardPage
