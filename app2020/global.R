library(shiny)
library(shinyjs)
library(leaflet)
library(mapview)
library(tidyverse)
library(sf)
library(plotly)
library(raster)
library(DT)
library(FSA)

#####################################   UPDATE EACH NEW TOOL REBUILD #############################################
# Establish Assessment Period 
assessmentPeriod <- as.POSIXct(c("2013-01-01 00:00:00 UTC","2018-12-31 23:59:59 UTC"),tz='UTC')
assessmentCycle <- '2020'
##################################################################################################################


# Bring in modules
source('appModules/multipleDependentSelectizeArguments.R')
source('newBacteriaStandard_workingUpdatedRecSeason.R') # version with 2/3 samples in April-Oct




#modulesToReadIn <- c('temperature','pH','DO','SpCond','Salinity','TN','Ecoli','chlA','Enteroccoci', 'TP','sulfate',
#                     'Ammonia', 'Chloride', 'Nitrate','metals', 'fecalColiform','SSC','Benthics')
#for (i in 1:length(modulesToReadIn)){
#  source(paste('appModules/',modulesToReadIn[i],'Module.R',sep=''))
#}


# Loading screen
load_data <- function() {
  Sys.sleep(2)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}


WQSvalues <- tibble(CLASS = c('I',"II","II","III","IV","V","VI","VII"),
                    `Description Of Waters` = c('Open Ocean', 'Tidal Waters in the Chowan Basin and the Atlantic Ocean Basin',
                                                'Tidal Waters in the Chesapeake Bay and its tidal tributaries',
                                                'Nontidal Waters (Coastal and Piedmont Zone)','Mountainous Zone Waters',
                                                'Stockable Trout Waters','Natural Trout Waters','Swamp Waters'),
                    `Dissolved Oxygen Min (mg/L)` = c(5,4,NA,4,4,5,6,NA),
                    `Dissolved Oxygen Daily Avg (mg/L)` = c(NA,5,NA,5,5,6,7,NA),
                    `pH Min` = c(6,6,6.0,6.0,6.0,6.0,6.0,3.7),
                    `pH Max` = c(9.0,9.0,9.0,9.0,9.0,9.0,9.0,8.0),
                    `Max Temperature (C)` = c(NA, NA, NA, 32, 31, 21, 20, NA))



withinAssessmentPeriod <- function(x){
  if((range(unique(x$FDT_DATE_TIME2))[1] < assessmentPeriod[1]) | 
     (range(unique(x$FDT_DATE_TIME2))[2] > assessmentPeriod[2])){
    print('Data included that falls outside of assessment period. Review input data.')
  }else{print('All input data falls within the assessment period.')}
}

# Super Assessment function
assessmentDetermination <- function(parameterDF,parameterAssessmentDF,parameter,use){
  
  results <- data.frame(nSamples = nrow(parameterDF),nExceedance = nrow(parameterAssessmentDF))%>%
    mutate(exceedanceRate = (nExceedance/nSamples)*100)
  
  if(results$exceedanceRate > 10.5 & results$nSamples > 10){outcome <- paste('Water impaired for',parameter)}
  if(results$exceedanceRate < 10.5 & results$nSamples > 10){outcome <- paste('Water not impaired for',parameter)}
  if(results$nExceedance >= 2 & results$nSamples < 10){outcome <- paste('Water impaired for',parameter)}
  if(results$nExceedance < 2 & results$nSamples < 10){outcome <- paste('Water not impaired for',parameter)}
  
  results <- mutate(results,Assessment=outcome, Use= use)
  return(results)
}
#assessmentDetermination(temp,temp_Assess,"temperature","Aquatic Life")


# Station Table building functions

concatinateUnique <- function(stuff){
  if(length(stuff)==1){
    if(is.na(stuff)){return(NA)
    }else{
      return(paste(unique(stuff), collapse= ', ')) }
  } 
  if(length(stuff) > 1){return(paste(unique(stuff), collapse= ', '))}
}

changeDEQRegionName <- function(stuff){
  # have to do this bc different places in conventionals report the assessment region over sample region
  if(length(stuff) == 1){
    if(stuff == "Valley"){return('VRO')}
    if(stuff == "Northern"){return('NRO')}
    if(stuff == "Piedmont"){return('PRO')}
    if(stuff == "Blue Ridge"){return('BRRO')}
    if(stuff == "Tidewater"){return('TRO')}
    if(stuff == "Southwest" ){return('SWRO')}
    if(stuff == 'NA'){return('NA')}
    #  if(is.na(stuff)){return("NA")}
  } else {return(concatinateUnique(stuff))}
}


StationTableStartingData <- function(x){
  data.frame(ID305B_1= concatinateUnique(x$ID305B_1), ID305B_2= concatinateUnique(x$ID305B_2), ID305B_3= concatinateUnique(x$ID305B_3),
             DEPTH = concatinateUnique(x$FDT_DEPTH_DESC), STATION_ID = concatinateUnique(x$FDT_STA_ID), REGION = changeDEQRegionName(concatinateUnique(x$Deq_Region)), 
             STATION_TYPE_1= concatinateUnique(x$STATION_TYPE_1), STATION_TYPE_2=concatinateUnique(x$STATION_TYPE_2), 
             STATION_TYPE_3= concatinateUnique(x$STATION_TYPE_3), STATION_LAT = concatinateUnique(x$Latitude), 
             STATION_LON = concatinateUnique(x$Longitude), WATERSHED_ID= concatinateUnique(x$ID305B_1),
             VAHU6 = concatinateUnique(x$Huc6_Vahu6) )
  # Should be this but issues with shiny application of function          
  #WATERSHED_ID= substr(strsplit(as.character(concatinateUnique(x$ID305B_1), '-'))[[1]][2], 1, 3), 
  
}

#dataFrame <-ammonia
#dateTimeColumn <- 'FDT_DATE_TIME2'
#nYears <- 3
#consecutive <- TRUE
lastXyears <- function(dataFrame, dateTimeColumn, nYears, consecutive){ # not bombproof but works for nYears=3
  uniqueYears <- dplyr::select(dataFrame, dateTimeColumn) %>% 
    mutate(sampleYear = lubridate::year(get(dateTimeColumn))) %>% 
    distinct(sampleYear) %>% 
    dplyr::pull()
  #uniqueYears <- c(2011, 2012, 2013, 2014, 2015, 2018)
  #uniqueYears <- c(2013, 2014, 2015, 2017)
  #uniqueYears <- c(2013, 2014, 2015, 2017, 2018)
  nYearsConversion <- nYears-1
  
  if(length(uniqueYears) > nYearsConversion){
    years <- sort(uniqueYears)[(length(uniqueYears)-nYearsConversion):length(uniqueYears)]
  } else { 
    years <- sort(uniqueYears)}
  
  if(consecutive == TRUE){
    recent <- years[which(years > max(years)-nYears)]
    return(recent)
    
  } else {return(years)}
}

#lastXyears(dataFrame, dateTimeColumn, 7, TRUE)


