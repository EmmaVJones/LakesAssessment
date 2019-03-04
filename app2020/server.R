#source('startupServer.R')


mapviewOptions(basemaps = c( "OpenStreetMap",'Esri.WorldImagery'),
               vector.palette = colorRampPalette(brewer.pal(3, "Set1")),
               na.color = "magenta",
               legend=FALSE)


shinyServer(function(input, output, session) {
  
  # display the loading feature until data loads into app
  load_data()
  
  
  
  
  ### Lake Selection Tab
  
  # Query AU's By Selectize arguments
  the_data <- reactive({lakeStations})
  region_filter <- shiny::callModule(dynamicSelect, 'regionSelection', the_data, "OFFICE_NM")
  lake_filter <- shiny::callModule(dynamicSelect, "lakeSelection", region_filter, "SIGLAKENAME" )
  #au_filter <- shiny::callModule(dynamicSelect, "AUselection", lake_filter, "ID305B" )

  
  # Station Map
  output$lakeMap <- renderLeaflet({
    req(lake_filter())
    points_sf <- lake_filter() %>%
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = F, # don't remove these lat/lon cols from df
               crs = 4326) # add coordinate reference system, needs to be geographic for now bc entering lat/lng, 
    AUs <- filter(lakeAU, ID305B %in% as.character( points_sf $ID305B_1) |
                    ID305B %in% as.character( points_sf $ID305B_2) |
                    ID305B %in% as.character( points_sf $ID305B_3))
    AUs$ID305B <- factor(AUs$ID305B) # drop extra factor levels so colors come out right
    
    map1 <- mapview(AUs,zcol = 'ID305B', label= AUs$ID305B, layer.name = 'Assessment Unit',
                    popup= popupTable(AUs, zcol=c("ID305B","Acres","CYCLE","WATER_NAME"))) + 
      mapview(points_sf, color='yellow',lwd=5,
              stroke=TRUE,label= points_sf$FDT_STA_ID, layer.name = c('Selected Stations'),
              popup= popupTable(points_sf, zcol=c("FDT_STA_ID","STA_DESC","ID305B_1", "ID305B_2", "ID305B_3")))
    map1@map })
  
  # Table of AUs in selected lake
  output$AUSummary <-  DT::renderDataTable({ 
    req(lake_filter())
    z <- dplyr::select(lake_filter(),ID305B, WATERSHED_ID, VAHU6, GNIS_ID:Assess_TYPE) %>%
      distinct(ID305B, .keep_all = T)
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "200px", dom='t')) 
  })
  
  # Table of stations in selected lake
  output$stationSummary <- DT::renderDataTable({
    req(lake_filter())
    z <- dplyr::select(lake_filter(), FDT_STA_ID:STA_REC_CODE, Latitude:VAHU6, CATEGORY_ID:Assess_TYPE)
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "200px", dom='t'))  })
  
  
  
  
  #### Assessment Unit Review Tab

  # Show selected AU
  output$selectedLake <- DT::renderDataTable({
    datatable(lake_filter() %>% dplyr::select(VAHU6, SIGLAKENAME, ID305B, SEC187 ) %>% distinct(ID305B, .keep_all = T),
              rownames = FALSE, options= list(pageLength = 1, scrollY = "35px", dom='t'))})
  
  # Pull Conventionals data for selected Lake on click
  conventionals_Lake <- eventReactive( input$pullLakeData, {
    z <- filter(conventionals, FDT_STA_ID %in% unique(lake_filter()$FDT_STA_ID)) %>%
      left_join(dplyr::select(lakeStations, FDT_STA_ID, SEC, CLASS, SPSTDS,PWS, ID305B_1, ID305B_2, ID305B_3,
                              STATION_TYPE_1, STATION_TYPE_2, STATION_TYPE_3, ID305B, SEC187, SIG_LAKE, USE, SIGLAKENAME, 
                              Chlorophyll_A_limit, TPhosphorus_limit, Assess_TYPE ), by='FDT_STA_ID')  })
  
  output$AUSelection_ <- renderUI({ 
    req(conventionals_Lake())
    selectInput('AUSelection', 'Assessment Unit Selection', choices = unique(conventionals_Lake()$ID305B)) })
  
  output$selectedAU <- DT::renderDataTable({
    req(conventionals_Lake(),input$AUSelection)
    z <- filter(lakeAU, ID305B %in% input$AUSelection) %>% st_set_geometry(NULL) %>% as.data.frame()
    datatable(z, rownames = FALSE, 
              options= list(pageLength = nrow(z),scrollX = TRUE, scrollY = "200px", dom='t'))})
  
  output$stationSelection_ <- renderUI({ 
    req(conventionals_Lake(), input$AUSelection)
    z <- filter(conventionals_Lake(), ID305B_1 %in% input$AUSelection | 
                  ID305B_2 %in% input$AUSelection | 
                  ID305B_2 %in% input$AUSelection) %>%
      distinct(FDT_STA_ID)
    fluidRow(selectInput('stationSelection', 'Station Selection', choices = unique(z$FDT_STA_ID)),
             helpText("The stations available in the drop down are limited to stations with an ID305B_1 designation equal 
                      to the selected AU. All AU's associated with the selected station can be viewed in the map below."))})
  
  
  AUData <- eventReactive( input$AUSelection, {
    filter(conventionals_Lake(), ID305B_1 %in% input$AUSelection | 
             ID305B_2 %in% input$AUSelection | 
             ID305B_2 %in% input$AUSelection) %>% 
      left_join(WQSvalues, by = 'CLASS') }) 
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData(), FDT_STA_ID %in% input$stationSelection) })
  
  output$stationInfo <- DT::renderDataTable({ 
    req(stationData())
    z <- dplyr::select(stationData()[1,], FDT_STA_ID:STA_REC_CODE, Latitude:Assess_TYPE) %>% 
      t() %>% as.data.frame() %>% rename(`Station and WQS Information` = 1)
    DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "250px", dom='t'))  })
  
  output$stationMap <- renderLeaflet({
    req(stationData())
    point <- dplyr::select(stationData()[1,],  FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:ID305B_3, Latitude, Longitude ) %>%
      st_as_sf(coords = c("Longitude", "Latitude"), 
               remove = F, # don't remove these lat/lon cols from df
               crs = 4269) # add projection, needs to be geographic for now bc entering lat/lng
    AU <- filter(lakeAU, ID305B %in% as.character(point$ID305B_1) |
                   ID305B %in% as.character(point$ID305B_2) |
                   ID305B %in% as.character(point$ID305B_3))
    map1 <- mapview(AU,zcol = 'ID305B', label= AU$ID305B, layer.name = 'Assessment Unit (ID305B_1)',
                    popup= popupTable(AU, zcol=c("ID305B","Acres","CYCLE","WATER_NAME"))) + 
      mapview(point, color = 'yellow', lwd = 5, label= point$FDT_STA_ID, layer.name = c('Selected Station'),
              popup= popupTable(point, zcol=c("FDT_STA_ID","STA_DESC","ID305B_1", "ID305B_2", "ID305B_3")))
    map1@map
    
  })
  
  output$stationHistoricalInfo <- DT::renderDataTable({ 
    req(stationData())
    z <- filter(lakeStations, FDT_STA_ID == input$stationSelection) %>% 
      dplyr::select(STATION_ID:COMMENTS) %>%
      t() %>% as.data.frame() %>% rename(`Station Information From Last Cycle` = 1)
    DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "250px", dom='t'))  })
  
  ## Station Table View Section
})
  
  