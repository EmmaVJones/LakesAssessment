source('global.R')


# Draft 2020 data
conventionals <- read_csv('data/draft2020data/CEDSWQM_2020_IR_DATA-CONVENTIONALS_20190213.csv') %>%
  filter(!is.na(Latitude)|!is.na(Longitude)) %>% # remove sites without coordinates
  rename('DO' = "DO_mg/L", "NITROGEN" = "NITROGEN_mg/L",  "AMMONIA" = "AMMONIA_mg/L" ,
         #"NH3_DISS" = , "NH3_TOTAL"  , 
         "PHOSPHORUS"= "PHOSPHORUS_mg/L" , "FECAL_COLI" = 'STORET_31616', "E.COLI" = 'ECOLI_CFU/100mL', 
         "ENTEROCOCCI" = 'STORET_31649', "CHLOROPHYLL" = 'STORET_32211', "SSC" = "STORET_SSC-TOTAL" , 
         "SSC_RMK" = "RMK_SSC-TOTAL" , "NITRATE" = "NITRATE_mg/L",  "CHLORIDE" = "CHLORIDE_mg/L" , 
         "SULFATE_TOTAL" = "SULFATE_mg/L",   "SULFATE_DISS" = 'STORET_00946')
conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")


lakeAU <- st_read('GIS/draft2018IR_AUs/va_2018_aus_reservoir.shp')

lakeStations <- read_csv('processedStationData/draft2020data/lakeStations2020draft.csv')
