# Run in R 3.5.2
source('global.R')

shinyUI(fluidPage(theme="yeti.css",
                  shinyjs::useShinyjs(),
                  div(
                    id = "loading_page",
                    h1("Loading...")
                  ),
                  hidden(
                    div(
                      id = "main_content",
                      # suppress error messages as data loads, hacky
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      ),
                      navbarPage(paste("VDEQ ",assessmentCycle," IR Lacustrine Assessment Tool", sep=''),
                              #   tabPanel('Data Upload',
                              #            h3('Tool Overview'),
                              #            p("The Lacustrine Assessment Tool is designed to expedite analysis, assessment
                              #              decisions, and quality assurance/quality control (QA/QC) procedures for Virginia's
                              #              contribution to the 2020 Integrated Report (IR). The data window analyzed covers 
                              #              January 1, 2013 to December 31, 2018. Tool users can expect significant time savings
                              #              on repetitive procedures including: raw data organization from disparate databases, 
                              #              geospatial organization of stations by assessment unit, standard/criteria calculations, 
                              #              and data visualization."),
                              #            p('The application represents the first iteration of an automated assessment tool. The datasets
                              #              and parameters chosen for analysis readily lend themselves to automated processing. Future versions
                              #              of the tool may include additional datasets and analyses.'),
                              #            p('For feedback, troubleshooting, and questions regarding analyses or missing data, please contact 
                              #              Emma Jones (emma.jones@deq.virginia.gov)'),
                              #            br(),
                              #            h3('Tool Inputs'),br(),
                              #            h4('Prepopulated Tool Inputs'),
                              #            tags$ul(
                              #              tags$li('Conventionals- CEDS data pulled and analyzed by Roger Stewart (roger.stewart@deq.virginia.gov) for each 
                              #                      Integrated Report data window.'), 
                              #              tags$li("Statewide Assessment (spatial) layer- The spatial dataset that identifies each regional office's lakes
                              #                      or reservoirs that require assessment."),
                              #              tags$li("Statewide Lacustrine Assessment Units (spatial) layer- The spatial dataset that organizes assessment units
                              #                      by each lake or reservoir.")),
                              #            br(),
                              #            h4('User Defined Tool Inputs'),
                              #            p('In order to reduce processing time and facilitate peristent data storage, users must
                              #              upload certain datasets that follow a specified template. These include their regional
                              #              Stations Table 2.0, limited to lake/reservoir sites. The dataProcessing_Statewide.Rmd walks
                              #              users through organizing the station table and splitting up riverine and lacustrine stations.'),
                              #            h5('Stations Table 2.0'),
                              #            helpText('This dataset is derived before any Lacustrine Assessment Tool analysis 
                              #                     procedures can commence using the ',span(strong('dataProcessing_Statewide.Rmd.')), 
                              #                     'After completing the requisite analyses from the ',
                              #                     span(strong('dataProcessing_Statewide.Rmd')),'once, users can 
                              #                     upload their results to the Lacustrine Assessment Tool each time they open
                              #                     the tool for analysis.'),
                              #            fileInput('stationsTable','Choose your Regional Stations Table 2.0.',
                              #                      accept = c(".csv")),
                               #           br(),br(),br()),
                      tabPanel('Lake Selection',
                               sidebarPanel(
                                 h4('Instructions:'),
                                 p("Use the drop down box to select an Assessment Unit (AU) to assess. All AU's are organized by lake. 
                                    The map will update based on your selection. Once you have reviewed the data below the map, proceed 
                                    to the 'Lake Assessment' Tab to begin analyzing the AU."),
                                 dynamicSelectInput('regionSelection', "Select DEQ Region to Assess", multiple = FALSE),
                                 dynamicSelectInput("lakeSelection", "Select Lake to Assess", multiple = FALSE)#,
                                 #dynamicSelectInput("AUselection", "Select Assessment Unit to Assess", multiple = FALSE)
                               ),
                               mainPanel(
                                 leafletOutput("lakeMap"),br(),
                                 h5(strong('Assessment units in selected lake')),
                                 DT::dataTableOutput('AUSummary'), br(),
                                 h5(strong('Stations in selected lake that were sampled in current window')),
                                 DT::dataTableOutput('stationSummary'))
                      ),
                      tabPanel('Assessment Unit Review',
                               fluidRow(column(9, DT::dataTableOutput('selectedLake')),
                                        column(3,br(),actionButton('pullLakeData','Select Watershed for analysis'),
                                               helpText('If the button above is disabled, there are no AUs in the selected lakes.'))),
                               hr(),
                               uiOutput('AUSelection_'),
                               h5(strong('AU information from last cycle')),
                               DT::dataTableOutput('selectedAU'),br(),
                               uiOutput('stationSelection_'),
                               fluidRow(column(4, DT::dataTableOutput('stationInfo')),
                                        column(4, leafletOutput('stationMap', height = 300, width = 300),
                                               helpText("The AUs displayed on the map above represent all AUs associated with the selected
                                                        station (listed in a station's ID305B_1/ID305B_2/ID305B_3 fields) for context. ")),
                                        column(4, DT::dataTableOutput('stationHistoricalInfo'))),
                               hr(),
                               h3('Station Results for Review')#,
                               
                      )
                      )))
                  )
        )