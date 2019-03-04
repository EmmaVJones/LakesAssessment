#source('startupServer.R')

shinyServer(function(input, output, session) {
  
  # display the loading feature until data loads into app
  load_data()
  
  # Query AU's By Selectize arguments
  the_data <- reactive({lakeStations})
  region_filter <- shiny::callModule(dynamicSelect, 'regionSelection', the_data, "OFFICE_NM")
  lake_filter <- shiny::callModule(dynamicSelect, "lakeSelection", region_filter, "SIGLAKENAME" )
  au_filter <- shiny::callModule(dynamicSelect, "AUselection", lake_filter, "ID305B" )
  output$verbatim <- renderPrint({au_filter()})
  
  # Station Map
  callModule(stationMap, "lakeMap",au_filter)
  
})