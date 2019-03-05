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
library(RColorBrewer)

#####################################   UPDATE EACH NEW TOOL REBUILD #############################################
# Establish Assessment Period 
assessmentPeriod <- as.POSIXct(c("2013-01-01 00:00:00 UTC","2018-12-31 23:59:59 UTC"),tz='UTC')
assessmentCycle <- '2020'
##################################################################################################################


# Bring in modules
source('appModules/multipleDependentSelectizeArguments.R')
source('newBacteriaStandard_workingUpdatedRecSeason.R') # version with 2/3 samples in April-Oct




modulesToReadIn <- c('temperature','DO','pH','Chl_a_')#,,'SpCond','Salinity','TN','Ecoli','chlA','Enteroccoci', 'TP','sulfate',
                    # 'Ammonia', 'Chloride', 'Nitrate','metals', 'fecalColiform','SSC','Benthics')
for (i in 1:length(modulesToReadIn)){
  source(paste('appModules/',modulesToReadIn[i],'Module.R',sep=''))
}


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
quickStats <- function(parameterDataset, parameter){
  if(nrow(parameterDataset) > 0 ){
    results <- data.frame(VIO = nrow(filter(parameterDataset, exceeds == TRUE)),
                          SAMP = nrow(parameterDataset)) %>%
      mutate(exceedanceRate = as.numeric(format((VIO/SAMP)*100,digits=3)))
    
    if(results$VIO >= 1){outcome <- 'Review'} # for Mary
    if(results$VIO >= 1 & results$exceedanceRate < 10.5){outcome <- 'Review'}
    if(results$exceedanceRate > 10.5 & results$VIO >= 2 & results$SAMP > 10){outcome <- '10.5% Exceedance'}
    if(results$VIO < 1 &results$exceedanceRate < 10.5 & results$SAMP > 10){outcome <- 'S'}
    if(results$VIO >= 1 & results$SAMP <= 10){outcome <- 'Review'}
    if(results$VIO < 1 & results$SAMP <= 10){outcome <- 'S'}
    
    
    results <- mutate(results, STAT = outcome)
    names(results) <- c(paste(parameter,names(results)[1], sep = '_'),
                        paste(parameter,names(results)[2], sep = '_'),
                        paste(parameter,names(results)[3], sep = '_'),
                        paste(parameter,names(results)[4], sep = '_'))
    #rename based on parameter entered
    return(results)
  } else {
    z <- data.frame(VIO = NA, SAMP=NA, exceedanceRate= NA, STAT=NA)
    names(z) <- paste(parameter,names(z), sep='_')
    return(z)
  }
}

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





# Table of thermocline depth (if thermocline exists) for each sample date
stratifiedLake <- function(x){
  template <- data.frame(FDT_STA_ID=NA,SampleDate=NA,ThermoclineDepth=NA)
  holder <- template
  if(nrow(x)>0){
    for(k in 1:length(unique(x$FDT_STA_ID))){
      oneStation <- filter(x,FDT_STA_ID %in% unique(x$FDT_STA_ID)[k])
      for(i in 1:length(unique(oneStation$SampleDate))){
        oneProfile <- filter(oneStation,SampleDate %in% unique(oneStation$SampleDate)[i])%>%
          dplyr::select(SampleDate,FDT_DEPTH,FDT_TEMP_CELCIUS) %>%
          mutate(DepthDiff=c(NA,diff(FDT_DEPTH)),
                 TempDiff=c(NA,diff(FDT_TEMP_CELCIUS))) %>%
          filter(DepthDiff==1) %>% # get rid of changes less than 1 meter depth
          filter(TempDiff<=-1) # find where temperature changes are 1C or greater
        therm <- if(nrow(oneProfile)>0){min(oneProfile$FDT_DEPTH)-0.5}else{NaN} # subtract 0.5 to get the top of the thermocline
        holder[i,] <- cbind(oneStation$FDT_STA_ID[1],unique(oneStation$SampleDate)[i],format(therm))
      }
      template <- rbind(template,holder)
      template <- template[complete.cases(template),]
    }
    row.names(template) <- 1:nrow(template)
    return(template)
  }else{
    return(template)
  }
}



#### Temperature Assessment Functions ---------------------------------------------------------------------------------------------------

#Max Temperature Exceedance Function
temp_Assessment <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME, FDT_DEPTH, FDT_TEMP_CELCIUS, `Max Temperature (C)`)%>% # Just get relevant columns, 
    filter(!is.na(FDT_TEMP_CELCIUS))%>% #get rid of NA's
    mutate(TemperatureExceedance=ifelse(FDT_TEMP_CELCIUS > `Max Temperature (C)`,T,F))%>% # Identify where above max Temperature, 
    filter(TemperatureExceedance==TRUE) # Only return temp measures above threshold
  temp$FDT_DATE_TIME <- as.character(temp$FDT_DATE_TIME)
  return(temp)
}

# Exceedance Rate Temperature
exceedance_temp <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME,FDT_DEPTH, FDT_TEMP_CELCIUS,`Max Temperature (C)`)%>% # Just get relevant columns, 
    filter(!is.na(FDT_TEMP_CELCIUS)) #get rid of NA's
  temp_Assess <- temp_Assessment(x)
  
  temp_results <- assessmentDetermination(temp,temp_Assess,"temperature","Aquatic Life")
  return(temp_results)
}

# For station table, presented a little differently
#Max Temperature Exceedance Function
tempExceedances <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME,FDT_TEMP_CELCIUS, `Max Temperature (C)`)%>% # Just get relevant columns, 
    filter(!is.na(FDT_TEMP_CELCIUS))%>% #get rid of NA's
    rename(parameter = !!names(.[2]), limit = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above max Temperature, 
  
  quickStats(temp, 'TEMP')
}
#tempExceedances(x)



#### Dissolved Oxygen Assessment Functions ---------------------------------------------------------------------------------------------------

#Min DO Exceedance Function
DO_Assessment <- function(x,qualifier){ # qualifier allows you to run the analysis including many or few stratification qualifications
  DO <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,DO,LakeStratification,FDT_DATE_TIME2,`Dissolved Oxygen Min (mg/L)`)%>% # Just get relevant columns, 
    filter(!is.na(DO))%>% #get rid of NA's
    filter(LakeStratification %in% qualifier)%>%
    ##filter(LakeStratification=="Epilimnion") # Only assess Epilimnion
    ##filter(LakeStratification %in% c("Epilimnion",NA))%>% # Only assess Epilimnion or NaN (no stratification)
    mutate(DOExceedance=ifelse(DO < `Dissolved Oxygen Min (mg/L)`,T,F))%>% # Identify where above max Temperature, 9VAC25-260-50 ClassIII= 32C
    filter(DOExceedance==TRUE) # Only return temp measures above threshold
  
  DO$FDT_DATE_TIME <- as.character(DO$FDT_DATE_TIME2)
  DO <- dplyr::select(DO,-c(DOExceedance,FDT_DATE_TIME2)) # Don't show user column, could be confusing to them
  
  return(DO)
}

# Exceedance Rate DO
exceedance_DO <- function(x, qualifier){
  DO <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,DO,LakeStratification,`Dissolved Oxygen Min (mg/L)`)%>% # Just get relevant columns, 
    filter(!is.na(DO))%>% #get rid of NA's
    filter(LakeStratification %in% qualifier)
  ##filter(LakeStratification %in% c("Epilimnion",NA)) # Only assess Epilimnion or NaN (no stratification)
  ##filter(LakeStratification=="Epilimnion") # Only assess Epilimnion
  DO_Assess <- DO_Assessment(x,qualifier)
  DO_results <- assessmentDetermination(DO,DO_Assess,"Dissolved Oxygen","Aquatic Life")
  return(DO_results)
}

# For station table, presented a little differently
DOExceedances_Min <- function(x){
  DO <- dplyr::select(x,FDT_DATE_TIME2,DO,`Dissolved Oxygen Min (mg/L)`,LakeStratification)%>% # Just get relevant columns, 
    filter(!is.na(DO)) %>% 
    filter(LakeStratification %in% c("Epilimnion",NA)) %>%
    rename(parameter = !!names(.[2]), limit = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(exceeds = ifelse(parameter < limit, T, F)) # Identify where below min DO 
  
  quickStats(DO, 'DO')
}
#DOExceedances_Min(x)




#### pH Assessment Functions ---------------------------------------------------------------------------------------------------

pH_rangeAssessment <- function(x){
  pH <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,FDT_FIELD_PH,`pH Min`,`pH Max`, LakeStratification)%>% # Just get relavent columns, 
    filter(!is.na(FDT_FIELD_PH))%>% #get rid of NA's
    filter(LakeStratification %in% c("Epilimnion",NA)) %>% # As of 2018 IR pH standards only apply to epilimnion or non stratified lake samples
    mutate(pHrange=ifelse(FDT_FIELD_PH < `pH Min` | FDT_FIELD_PH > `pH Max`,TRUE, FALSE)) %>% # Identify where pH outside of assessment range
    filter(pHrange==TRUE) %>% # Only return pH measures outside of assessement range
    dplyr::select(-c(pHrange)) # Don't show user interval column, could be confusing to them, T/F in pHrange column sufficient
  pH$FDT_DATE_TIME <- as.character(pH$FDT_DATE_TIME)
  return(pH)
}


exceedance_pH <- function(x,pHrange){
  pH <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,FDT_FIELD_PH, `pH Min`,`pH Max`, LakeStratification)%>% # Just get relavent columns, 
    filter(!is.na(FDT_FIELD_PH)) #get rid of NA's
  pH_rangeAssess <- pH_rangeAssessment(x)
  
  pH_results <- assessmentDetermination(pH %>% filter(!is.na(FDT_FIELD_PH))%>% #get rid of NA's
                                          filter(LakeStratification %in% c("Epilimnion",NA)),
                                        pH_rangeAssess,"pH","Aquatic Life")
  return(pH_results)
}


# For station table, presented a little differently

pHExceedances <- function(x){
  pH <- dplyr::select(x,FDT_DATE_TIME,FDT_FIELD_PH,`pH Min`,`pH Max`, LakeStratification)%>% # Just get relevant columns, 
    filter(!is.na(FDT_FIELD_PH))%>% #get rid of NA's
    filter(LakeStratification %in% c("Epilimnion",NA)) %>%
    rowwise() %>% mutate(interval=findInterval(FDT_FIELD_PH,c(`pH Min`,`pH Max`)))%>% # Identify where pH outside of assessment range
    ungroup()%>%
    mutate(exceeds=ifelse(interval == 1, F, T)) # Highlight where pH doesn't fall into assessment range
  quickStats(pH, 'PH')
}
#pHExceedances(x)




#### Chlorophyll a Assessment Functions ---------------------------------------------------------------------------------------------------

chlA_Assessment_OneStation <- function(x){
  chlA <- filter(x, !is.na(CHLOROPHYLL))%>%
    dplyr::select(FDT_STA_ID,FDT_DEPTH,FDT_DATE_TIME, SampleDate,CHLOROPHYLL,Chlorophyll_A_limit,Assess_TYPE)%>%
    mutate(Year=format(FDT_DATE_TIME,"%Y"),Month=format(FDT_DATE_TIME,'%m'))%>%
    filter(Month %in% c('04','05','06','07','08','09','10'))%>% # make sure only assess valid sample months
    group_by(Year)%>%
    mutate(samplePerYear= n(),pct90=quantile(CHLOROPHYLL,0.9),
           chlA_Exceedance=ifelse(pct90>Chlorophyll_A_limit,T,F),
           LacustrineZone=ifelse(!is.na(unique(x$Assess_TYPE)) & unique(x$Assess_TYPE)=="LZ",TRUE,FALSE))%>%
    dplyr::select(FDT_STA_ID,Year,samplePerYear,pct90,Chlorophyll_A_limit,chlA_Exceedance,LacustrineZone)%>%
    distinct(Year,.keep_all=T)
  return(chlA)
}

chlA_Assessment <- function(x){
  holder <- list()
  for(i in 1:length(unique(x$FDT_STA_ID))){
    dat <- filter(x,FDT_STA_ID %in% unique(x$FDT_STA_ID)[i])
    holder[[i]] <-  as.data.frame(chlA_Assessment_OneStation(dat))
  }
  alldat <- do.call(rbind,holder)#%>%filter(!is.na(Year))
  return(alldat)
}


exceedance_chlA <- function(x){
  chlA_Assess <- chlA_Assessment(x)
  if(nrow(chlA_Assess) < 1){
    return('No Chlorophyll a data for station ')
  }else{
    if(class(chlA_Assess$FDT_STA_ID)=="factor"){ # have to split this step up bc n stationID's affect how split performs
      chlA_Assess$FDT_STA_ID <- droplevels(chlA_Assess$FDT_STA_ID) # have to drop unused levels from factor or it messes with split function and mixes up data in each list item
    }
    dat <- split(chlA_Assess,f=chlA_Assess$FDT_STA_ID)
    holder <- list()
    for(i in 1:length(dat)){
      # Find two most recent years with >= 6 data points
      step1 <- filter(dat[[i]],samplePerYear>=6) # verify enough samples
      step2 <- filter(step1,Year %in% tail(sort(unique(step1$Year)),2)) %>% # get two most recent years from valid sample years 
        mutate(ID305B_1 = as.character(filter(lakeStations, STATION_ID %in% unique(step1$FDT_STA_ID))$ID305B_1))
      
      if(nrow(step2)>1){ # only  do this if more than 1 year of data
        if(step2$chlA_Exceedance[1]!=step2$chlA_Exceedance[2]){ # if the exceedances contradict one another in two years grab third year
          step1alt <- filter(dat[[i]],samplePerYear>=6) # verify enough samples 
          step2 <- filter(step1,Year %in% tail(sort(unique(step1$Year)),3)) %>% # get three most recent years from valid sample years
            mutate(ID305B_1 = as.character(filter(lakeStations, STATION_ID %in% unique(step1$FDT_STA_ID))$ID305B_1))
        }
      }
      holder[[i]] <-  step2
    }
    do.call(rbind,holder) # output table for user to review
  }
  
}


