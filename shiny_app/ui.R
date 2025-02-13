## UI
# Use a fluid Bootstrap layout
ui <- dashboardPage(
  
  title = "YSM Technical Report", # Browser title
  
  dashboardHeader(
                  tags$li(a(href = 'https://gov.bc.ca',
                            img(src = 'logo-banner.png',
                                title = "Home", height = "41px"),
                            style = "padding-top:10px; padding-bottom:10px;
                            background-color: #036; margin-right: 10px;"),
                          class = "dropdown"),
                  title = div('YSM Technical Report', style = "color: white; font-weight: bold; font-size: 24px;
                              font-family: 'BCSans', 'Noto Sans', Verdana, Arial, sans-serif;
                              padding-top:10px;")
    #titleWidth='100%',
    #title = span(
    #  tags$img(src="logo-banner.png", title = "Home", height = "41px"), 
    #  column(12, class="title-box", 
    #         tags$h1(class="primary-title", style="color: white; font-weight: bold; font-size: 24px;
    #                          font-family: 'BCSans', 'Noto Sans', Verdana, Arial, sans-serif;
    #                          padding-top:10px;", 'YSM Technical Report')
    #         #tags$h2(class="primary-subtitle", style='margin-top:10px;', 'EXPERT ELICITATION FOR ADAPTIVE MANAGEMENT')
    #  )
    #              )
    ),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    
    tags$head(tags$style("body{min-height: 800px;  height: auto;  max-width: 1296px;  margin: auto;
                         background-color: #b3b1b3}")),
    
  fluidPage(    
    
    waiter::use_waiter(),
    #waiter::waiter_show_on_load(html = waiter_html("Loading App")),
    
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
        
        /*.nav-tabs>li>a {font-family: "BCSans", "Noto Sans", Verdana, Arial, sans-serif; color:#036;}
        .nav-tabs>li.active>a {font-family: "BCSans", "Noto Sans", Verdana, Arial, sans-serif; color:#036;}*/
        
        /*.navbar{color: #036;}
        .navbar-default .navbar-brand {color: #cc3f3f;}*/
        
        .content-wrapper, .right-side {background-color: #FFFFFF;}
        
        /*.footer {
             border-top: 2px solid #fcba19;
             color: #fff;
             position: absolute;
             font-family: ‘BCSans’, ‘Noto Sans’, Verdana, Arial, sans-serif; 
             bottom: 0;
             width: 100%;
             height: 60px; 
             background-color: #036;}*/
      '))),
    
    box(title ="Note: This site is currently under development.", 
        solidHeader = T, collapsible = T,status = "warning", width = NULL, collapsed=TRUE,
        #p(strong("*Note that this site is currently under development."),style = "color:red"),
        p("
Published versions of the YSM Technical reports can be found at:
https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/ground-sample-inventories/provincial-monitoring/reports",
          style = "color:red")),
    
  box(title = "Select the area of interest", #background = "light-blue", 
      solidHeader = TRUE, status = "primary", width = NULL,
      
  column(3, radioButtons(inputId = "SelectCategory", label = "Strata",
                        choices = c("By TSA" = "TSA_DESC", 
                                    #"By BEC zone" = "BEC_ZONE",
                                    "By BEC subzone" = "BECsub"), inline = F)
                
         ), # radiobutton column
  
  column(3, offset = 1, selectInput(inputId = "SelectVar",
                                    label = "Select",
                                    choices = NULL), 
         HTML("<font size='-1'>*only n&ge;10 are selectable.</font>")),
  
  column(3, offset = 1, downloadButton("downloadReport", "Download report"), br(),
         radioButtons("format", "Document format", c("HTML", "PDF"), inline = TRUE))
  
  ), # box
  
  column(12, 
         
         navlistPanel(
           
           #tags$style("t{color:blue;}"), 
    
    "Overview",
    tabPanel(title = "Overview",
             #hr(),
             uiOutput('overview_header'),
             uiOutput("overview"),
             br(),
             leafletOutput("plotgraph"),
             br(),
             uiOutput("overviewflex"),
             br()
    ),
    
    tabPanel(title = "Summary of Key Findings",
             uiOutput("key_finding")
    ),
    
    "Young Stand Description",
    tabPanel(title = "Stand Summaries",
             uiOutput("young_stand_description"),
             br(),
             uiOutput("stand_summary_flex"),
             br(),
             plotOutput("live_sp", width = "700px"),
             br(),
             plotOutput("bec_dist", width = "500px"),
             br(),
             plotOutput("stock_table", width = "700px"),
             br(),
    ),
    
    tabPanel(title = "Leading Species",
             h3("Leading Species vs. VRI, & Overall Species Composition vs. TSR Inputs"),
             uiOutput("leading_sp"),
             br(),
             uiOutput("leading_sp_flex"),
             br(),
             plotOutput("spc_comp", width = "700px"),
             br(),
    ),
    tabPanel(title = "Residual Trees",
             h3("Post-Harvest Regenerated vs. Residual Trees"),
             uiOutput("residual"),
             br(),
             plotOutput("residual_ysm", width = "600px"),
             br(),
    ),
    
    "Site Index",
    tabPanel(title = "Provincial Site Productivity Layer",
             h3("Site Index vs. Provincial Site Productivity Layer"),
             uiOutput("site_index_pspl"),
             br(),
             uiOutput("si_pspl_flex"),
             br(),
    ),
    tabPanel(title = "Trends in Site Index Estimates",
             h3("Trends in Site Index Estimates over Time"),
             uiOutput("trend_si"),
             br(),
             plotOutput("si_trend", width = "700px"),
             br(),
    ),
    
    "Comparison with TSR",
    tabPanel(title = "Current Volumes",
             h3("Comparing Current Volumes: TSR Predicted Yield Tables vs. YSM Actual Measurements"),
             uiOutput("comp_curr_vol"),
             br(),
             plotOutput("age_vs_netmer", width = "700px"),
             br(),
             plotOutput("vol_bias", width = "700px"),
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
                      uiOutput("age_flex2"))
             ),
             br(),
             fluidRow(
               column(12, align = "center", 
                      plotOutput("age_diff", height = "200px", width = "400px")),
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
             plotOutput("pai_diff", height = "200px", width = "600px"),
             br(),
    ),  
    
    "Forest Health",
    tabPanel(title = "Growth and Mortality",
             h3("Quantifying Change in Growth and Mortality"),
             uiOutput("quant_coc"),
             br(),
             plotOutput("coc_chart", width = "600px"),
             br(),
    ),
    tabPanel(title = "Current Forest Health Incidence",
             h3("Current Forest Health Incidence"),
             uiOutput("health_inci"),
             br(),
             plotOutput("curr_fh_inci"),
             br(),
    ),
    tabPanel(title = "Change in Forest Health Incidence",
             h3("Comparing Change in Forest Health Incidence"),
             uiOutput("comp_coc"),
             br(),
             plotOutput("change_dam"),
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
             plotOutput("dam_immed", height = "300px", width = "800px"),
             br(),
             #plotOutput("dam_incr")
    ), 
    
    "YSM TASS Projection",
    tabPanel(title = "Will Existing Young Stands Meet Expectations at Rotation?",
             h3("Will Existing Young Stands Meet Expectations at Rotation?"),
             uiOutput("tass_tsr"),
             br(),
             plotOutput("tass_tsr_netvol", width = "800px"),
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
             uiOutput("tass_tsr_volproj"),
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
                             uiOutput("ysm_tables1")),
                      column(6,
                             uiOutput("ysm_tables2")),
             ),
             br(),
             fluidRow(align = 'center',
                      column(6,
                             uiOutput("ysm_tables3")),
                      column(6,
                             uiOutput("ysm_tables4"))
             ),
             fluidRow(align = 'center',
                      column(6,
                             uiOutput("ysm_tables5"))
             ),
             #plotOutput("ysm_tables"),
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
    tabPanel(title = "General Notes / Assumptions",
             h3("General Notes / Assumptions"),
             uiOutput("assumps")
    ),
    tabPanel(title = "References",
             h3("References"),
             uiOutput("refs")
    ),
    
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
